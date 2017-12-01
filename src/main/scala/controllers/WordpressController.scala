package controllers

import java.text.SimpleDateFormat
import java.util.Calendar

import akka.actor.{Actor, _}
import net.liftweb.json.{parse => liftParse, _}
import services.WordpressService
import slack.api.SlackApiClient
import utilities.ConfigHelper._

import scala.concurrent.ExecutionContext.Implicits.global

case class FormattedBlogger(name: String, numberOfBlogs: Int, totalViews: Int)

case object ProcessBlogsView

case class Blogger(blogId: List[Int], authorId: Int, name: String, numberOfBlogs: Int, views: List[Int], totalViews: Int)

class WordpressController(implicit val system: ActorSystem) extends Actor with ActorLogging {

  val wordpressService: WordpressService = WordpressService

  implicit val formats = DefaultFormats

  def receive: Receive = {
    case ProcessBlogsView =>
      val calendar = Calendar.getInstance()

      calendar.add(Calendar.MONTH, -2)
      calendar.set(Calendar.DATE, 1)

      val firstDay = calendar.getTime

      calendar.add(Calendar.MONTH, 1)
      calendar.set(Calendar.DATE, 1)

      val lastDay = calendar.getTime

      val format = new SimpleDateFormat("yyyy-MM-dd")
      val formattedFirstDay = format.format(firstDay)
      val formattedLastDay = format.format(lastDay)

      log.info("Received message Process Blog Views and going to getTotalPost function")

      val totalPosts = wordpressService.getTotalPost(formattedFirstDay, formattedLastDay) // yyyy-MM-dd

      log.info("Posts = " + totalPosts.mkString("\n"))

      val postByAuthorIds = totalPosts.groupBy { post =>
        if (post.author.first_name.nonEmpty) {
          (post.author.ID, post.author.first_name)
        } else {
          (post.author.ID, post.author.name)
        }
      }
      val result = postByAuthorIds.map {
        case ((authorId, authorName), posts) =>
          val viewsCountAndBlogId = posts.map { blog =>
            val viewsCount = wordpressService.getPostViewByPostId(blog.ID)
            (blog.ID, viewsCount)
          }
          val totalViews = viewsCountAndBlogId.map { case (_, views) => views }.sum
          val blogIds = viewsCountAndBlogId.map { case (blogId, _) => blogId }
          val viewCounts = viewsCountAndBlogId.map { case (_, views) => views }
          Blogger(blogIds, authorId, authorName, blogIds.size, viewCounts, totalViews)
      }.toList

      val finalResult = result.sortBy(_.totalViews).reverse

      log.info("total number of blogs in a month" + totalPosts.size)
      log.info("result" + finalResult.mkString("\n"))

      val client = SlackApiClient(SLACK_API_TOKEN)
      val eventualChannels = client.listChannels()

      val eventualChannelNameToId = eventualChannels.map { channels =>
        channels.map { channel =>
          channel.name -> channel.id
        }.toMap
      }

      val eventualMaybeChanId = eventualChannelNameToId.map { channelNameToId =>
        channelNameToId.get(CHANNEL_NAME)
      }

      val eventualChanId = eventualMaybeChanId.map { maybeChanId =>
        maybeChanId.fold("")(identity)
      }

      eventualChanId.map { chanId =>
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
