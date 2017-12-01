package controllers

import akka.actor.{Actor, _}
import net.liftweb.json.{parse => liftParse, _}
import services.{Post, WordpressService}
import slack.api.SlackApiClient
import utilities.CalendarHelper
import utilities.ConfigHelper._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

case class FormattedBlogger(name: String, numberOfBlogs: Int, totalViews: Int)

case object ProcessBlogsView

case class Blogger(blogId: List[Int], authorId: Int, name: String, numberOfBlogs: Int, views: List[Int], totalViews: Int)

class WordpressController(implicit val system: ActorSystem) extends Actor with ActorLogging {

  val wordpressService: WordpressService = WordpressService

  implicit val formats = DefaultFormats

  def receive: Receive = {
    case ProcessBlogsView =>
      log.info("Received message Process Blog Views and going to getTotalPost function")

      val firstDay = CalendarHelper.getFirstDay
      val lastDay = CalendarHelper.getLastDay
      val totalPosts = wordpressService.getTotalPost(firstDay, lastDay)

      log.info("Posts = " + totalPosts.mkString("\n"))

      val postByAuthorIds = totalPosts.fold(Map[(Int, String), List[Post]]()) {
        _.groupBy { post =>
          if (post.author.first_name.nonEmpty) {
            (post.author.ID, post.author.first_name)
          } else {
            (post.author.ID, post.author.name)
          }
        }
      }

      val finalResult = getFinalResult(postByAuthorIds)

      log.info("total number of blogs in a month" + totalPosts.size)
      log.info("result" + finalResult.mkString("\n"))

      postMessageOnSlack(totalPosts.fold(List[Post]())(identity), finalResult)
  }

  def postMessageOnSlack(totalPosts: List[Post], finalResult: List[Blogger]): Future[String] = {
    val client = SlackApiClient(SLACK_API_TOKEN)

    val eventualMaybeChanId = client.listChannels().map { channels =>
      channels.map { channel =>
        channel.name -> channel.id
      }.toMap.get(CHANNEL_NAME)
    }

    eventualMaybeChanId flatMap { maybeChanId =>
      maybeChanId.fold(Future.successful("")) { chanId =>
        val index = 1 to totalPosts.size

        val formattedFinalResult = finalResult.map { blogger =>
          FormattedBlogger(blogger.name, blogger.numberOfBlogs, blogger.totalViews)
        }

        val formattedBlogsWithIndex =
          (index, formattedFinalResult)
            .zipped
            .map((index, formattedBlogger) => (index, formattedBlogger))
            .map { case (rank, formattedBlogger) =>
              rank + ". " + formattedBlogger.name + " " + formattedBlogger.numberOfBlogs + " " + formattedBlogger.totalViews
            }

        client.postChatMessage(chanId, formattedBlogsWithIndex mkString "\n", Some("Bot"))
      }
    }
  }

  def getFinalResult(postByAuthorIds: Map[(Int, String), List[Post]]): List[Blogger] = {
    val result = postByAuthorIds.map {
      case ((authorId, authorName), posts) =>
        val viewsCountAndBlogId = posts.map { blog =>
          val viewsCount = wordpressService.getPostViewByPostId(blog.ID)
          (blog.ID, viewsCount.getOrElse(0))
        }
        val totalViews = viewsCountAndBlogId.map { case (_, views) => views }.sum
        val blogIds = viewsCountAndBlogId.map { case (blogId, _) => blogId }
        val viewCounts = viewsCountAndBlogId.map { case (_, views) => views }
        Blogger(blogIds, authorId, authorName, blogIds.size, viewCounts, totalViews)
    }.toList

    result.sortBy(_.totalViews).reverse
  }
}
