package controllers

import akka.actor.{Actor, _}
import net.liftweb.json.{parse => liftParse, _}
import services.{Post, WordpressService}
import slack.api.SlackApiClient
import utilities.ConfigHelper._
import utilities.{CalendarHelper, LoggerHelper, MessageHelper}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

case class FormattedBlogger(name: String, numberOfBlogs: Int, views: List[Int], totalViews: Int)

case object ProcessBlogsView

class WordpressController(implicit val system: ActorSystem) extends Actor with LoggerHelper {

  val wordpressService: WordpressService = WordpressService
  val messageHelper: MessageHelper = MessageHelper

  implicit val formats = DefaultFormats

  def receive: Receive = {
    case ProcessBlogsView =>
        info("Got a request to get total blogs and blogs views")

      val firstDay = CalendarHelper.getFirstDay
      val lastDay = CalendarHelper.getLastDay
      val optionalPosts = wordpressService.getTotalPost(firstDay, lastDay)

        info("Posts = " + optionalPosts.fold("")(_.mkString("\n")))
        info(s"Total number of posts between $firstDay and $lastDay is ${optionalPosts.fold(0)(_.size)}")

      optionalPosts match {
        case Some(posts) =>
          val postByAuthorIds = posts.groupBy { post =>
            if (post.author.first_name.nonEmpty) {
              val authorLastName = if(post.author.last_name.nonEmpty) " " + post.author.last_name else ""

              (post.author.ID, post.author.first_name + authorLastName)
            } else {
              (post.author.ID, post.author.name)
            }
          }

          val formattedBloggers = getFormattedBloggers(postByAuthorIds)

          postMessageOnSlack(formatResult(formattedBloggers, posts))
        case None        =>
            info(s"No blog found between $firstDay and $lastDay")
      }
  }

  def getFormattedBloggers(postByAuthorIds: Map[(Int, String), List[Post]]): List[FormattedBlogger] = {
    val bloggers = postByAuthorIds.map {
      case ((authorId, authorName), posts) =>
        val viewsCountAndBlogId = posts.map { blog =>
          val viewsCount = wordpressService.getPostViewByPostId(blog.ID)
          (blog.ID, viewsCount.getOrElse(0))
        }
        val totalViews = viewsCountAndBlogId.map { case (_, views) => views }.sum
        val blogIds = viewsCountAndBlogId.map { case (blogId, _) => blogId }
        val viewCounts = viewsCountAndBlogId.map { case (_, views) => views }
        FormattedBlogger(authorName, blogIds.size, viewCounts, totalViews)
    }.toList

    bloggers.sortBy(_.totalViews).reverse
  }

  def formatResult(bloggers: List[FormattedBlogger], totalPosts: List[Post]): String = {
    val index = 1 to totalPosts.size

    val formattedBlogsWithIndex =
      (index, bloggers)
        .zipped
        .map((index, formattedBlogger) => (index, formattedBlogger))
        .map { case (rank, formattedBlogger) =>
          f"$rank%-10s${formattedBlogger.name}%-30s${formattedBlogger.numberOfBlogs}%-20d${formattedBlogger.totalViews}%-10d"
        } mkString "\n"

    messageHelper.getMessage(bloggers, formattedBlogsWithIndex, totalPosts.length)
  }

  def postMessageOnSlack(formattedResult: String): Future[String] = {
    val client = SlackApiClient(SLACK_API_TOKEN)
    val eventualChannelId = client.listChannels().map { channels =>
      channels.map { channel =>
        channel.name -> channel.id
      }.toMap.get(CHANNEL_NAME)
    }

    eventualChannelId flatMap { optionalChannelId =>
      optionalChannelId.fold(Future.successful("")) { chanId =>
        client.postChatMessage(optionalChannelId.get, formattedResult, Some("Blogger of the Month"))
      }
    }
  }
}
