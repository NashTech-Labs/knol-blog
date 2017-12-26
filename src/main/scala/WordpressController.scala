import java.text.SimpleDateFormat
import java.util.{Calendar, Date}

import akka.actor.{Actor, ActorSystem, _}
import com.typesafe.config.ConfigFactory
import dispatch._
import net.liftweb.json.JsonAST.JArray
import net.liftweb.json.{parse => liftParse, _}
import org.joda.time.DateTime
import slack.api.SlackApiClient
import com.typesafe.akka.extension.quartz.QuartzSchedulerExtension

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global

case class Author(ID: Int, login: String, email: Boolean, name: String, first_name: String, last_name: String, nice_name: String,
                  URL: String, avatar_URL: String, profile_URL: String, site_ID: Int)

case class Post(ID: Int, author: Author, title: String)

case class Blogs(found: Int, posts: List[Post])

case class FormattedBlogger(name: String, numberOfBlogs: Int, totalViews: Int)

case object ProcessBlogsView

case object Start

case class Blogger(blogId: List[Int], authorId: Int, name: String, numberOfBlogs: Int, views: List[Int], totalViews: Int)

case class BlogViews(date: Date, views: Int)

object BlogViews {
  def apply(dateAndViews: (JsonAST.JValue, JsonAST.JValue)): BlogViews = {
    val (dateJson, viewsJson) = dateAndViews
    val date = DateTime.parse(extractJValue(dateJson).asInstanceOf[String]).toDate
    val views = extractJValue(viewsJson).asInstanceOf[BigInt].toInt
    new BlogViews(date, views)
  }

  def extractJValue(myJValue: JsonAST.JValue) = myJValue match {
    case JInt(views)   => views
    case JString(date) => date
  }
}

class WordpressController(implicit system: ActorSystem) extends Actor with ActorLogging {

  val date = new DateTime(System.currentTimeMillis())
  val config = ConfigFactory.load()
  val SITE_NAME = sys.env.getOrElse("SITE_NAME", config.getString("site.name"))
  val ACCESS_TOKEN = sys.env.getOrElse("SITE_ACCESS_TOKEN", config.getString("site.accessToken"))
  val BLOGS_LIMIT = sys.env.getOrElse("BLOGS_LIMIT", config.getInt("site.blogslimit"))
  val SLACK_API_TOKEN = sys.env.getOrElse("SLACK_API_TOKEN", config.getString("site.slackApiToken"))

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

      println("-----------------------------First day = " + firstDay)
      println("-----------------------------Last day = " + lastDay)
      println("-----------------------------Formatted First day = " + formattedFirstDay)
      println("-----------------------------Formatted Last day = " + formattedLastDay)

      println("Received message Process Blog Views and going to getTotalPost function")
      val totalPosts = getTotalPost(formattedFirstDay, formattedLastDay) // yyyy-MM-dd

      println("------------------------- Total no. of posts = " + totalPosts.size)

      println("-------------------------- Posts = " + totalPosts.mkString("\n"))

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
            val viewsCount = getPostViewByPostId(blog.ID)
            (blog.ID, viewsCount)
          }
          val totalViews = viewsCountAndBlogId.map { case (_, views) => views }.sum
          val blogIds = viewsCountAndBlogId.map { case (blogId, _) => blogId }
          val viewCounts = viewsCountAndBlogId.map { case (_, views) => views }
          Blogger(blogIds, authorId, authorName, blogIds.size, viewCounts, totalViews)
      }.toList

      val finalResult = result.sortBy(_.totalViews).reverse

      println("total number of blogs in a month >>>>>>>>>>>" + totalPosts.size)
      println("result>>>>>>>>>>>" + finalResult.mkString("\n"))

      val client = SlackApiClient(SLACK_API_TOKEN)
      val eventualChannels = client.listChannels()

      val eventualChannelNameToId = eventualChannels.map { channels =>
        channels.map { channel =>
          channel.name -> channel.id
        }.toMap
      }

      val eventualMaybeChanId = eventualChannelNameToId.map { channelNameToId =>
        channelNameToId.get("blogger-of-the-month")
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

  def getTotalPost(afterDate: String, beforeDate: String): List[Post] = {
    println("Inside getTotalPost function")
    val requestUrl =
      s"""https://public-api.wordpress.com/rest/v1.1/sites/$SITE_NAME/posts
         |?after=%s&before=%s&number=$BLOGS_LIMIT""".stripMargin.replaceAll("\n", "") format(afterDate, beforeDate)

    println("Sending request")

    val eventualResponse = Http.default(dispatch.url(requestUrl) OK as.String)

    val maybeBlogs = JArray(liftParse(eventualResponse()).children).extractOpt[Blogs]

    val initialOffset = 100

    @tailrec
    def getPostInOffset(found: Int, totalPosts: List[Post], offset: Int): List[Post] = {
      if (offset > found || found == 0) {
        totalPosts
      } else {
        val requestUrl =
          s"""https://public-api.wordpress.com/rest/v1.1/sites/$SITE_NAME/posts
             |?after=%s&before=%s&number=$BLOGS_LIMIT&offset=$offset""".stripMargin.replaceAll("\n", "") format(afterDate, beforeDate)

        val response = executeRequestWithoutAuth(requestUrl)
        val maybeOffsetPosts = (liftParse(response) \\ "posts").extractOpt[List[Post]]

        val (newFound, newTotalPosts, newOffset) = maybeOffsetPosts.fold(0, totalPosts, offset) { offsetPosts =>
          val filteredPosts = offsetPosts.filterNot(_.title.toLowerCase.contains("knolx"))
          val newTotalPosts = totalPosts ++ filteredPosts

          (found, newTotalPosts, offset + 100)
        }
        getPostInOffset(newFound, newTotalPosts, newOffset)
      }
    }

    maybeBlogs.fold(List[Post]()) { blogs =>
      getPostInOffset(blogs.found,
        blogs.posts.filterNot(_.title.toLowerCase.contains("knolx")),
        initialOffset)
    }
  }

  def getPostViewByPostId(postId: Int): Int = {
    try {
      val calendar = Calendar.getInstance
      val requestUrl =
        s"""https://public-api.wordpress.com/rest/v1.1/sites/$SITE_NAME/stats/post/$postId"""

      val response = executeRequest(requestUrl)

      val dailyViews =
        (liftParse(response) \\ "data").children.map(_.children).map(jValueList => jValueList.head -> jValueList(1)).toMap
      val blogPostDate =
        DateTime.parse(BlogViews.extractJValue(liftParse(response) \\ "post" \ "post_date").asInstanceOf[String].split(" ").head).toDate

      dailyViews.map(BlogViews(_)).toList.filter { blogViews =>
        calendar.setTime(blogPostDate)
        calendar.add(Calendar.DATE, 31)
        blogViews.date before calendar.getTime
      }.map(_.views).sum
    } catch {
      case ex: Exception =>
        0
    }
  }

  private def executeRequest(url: String): String = {
    val request = dispatch.url(url).setHeader("Authorization", s"Bearer $ACCESS_TOKEN")
    val eventualResponse = Http.default(request OK as.String)
    eventualResponse()
  }

  private def executeRequestWithoutAuth(url: String): String = {
    val request = dispatch.url(url)
    val eventualResponse = Http.default(request OK as.String)
    eventualResponse()
  }
}

object Boot extends App {
  implicit val system = ActorSystem("Blogger-Scheduler")

  println("Sending message Start")

  val blogScheduler = system.actorOf(Props(new WordpressController()))

  val scheduler = QuartzSchedulerExtension(system)

  scheduler.schedule("EveryMonth", blogScheduler, ProcessBlogsView)

}
