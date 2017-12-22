import java.util.{Calendar, Date}

import akka.actor.{Actor, ActorSystem, _}
import com.typesafe.config.ConfigFactory
import dispatch._
import net.liftweb.json.JsonAST.JArray
import net.liftweb.json.{parse => liftParse, _}
import org.joda.time.DateTime

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

case class Author(ID: Int, login: String, email: Boolean, name: String, first_name: String, last_name: String, nice_name: String,
                  URL: String, avatar_URL: String, profile_URL: String, site_ID: Int)

case class Post(ID: Int, author: Author)

case class Blogs(found: Int, posts: List[Post])

case object ProcessBlogsView

case object Start

case class Bloggers(blogId: List[Int], authorId: Int, name: String, numberOfBlogs: Int, views: List[Int], totalViews: Int)

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

class WordpressController(implicit system: ActorSystem) extends Actor {

  val date = new DateTime(System.currentTimeMillis())
  val config = ConfigFactory.load()
  val SITE_NAME = sys.env.getOrElse("SITE_NAME", config.getString("site.name"))
  val ACCESS_TOKEN = sys.env.getOrElse("SITE_ACCESS_TOKEN", config.getString("site.accessToken"))
  val BLOGS_LIMIT = sys.env.getOrElse("BLOGS_LIMIT", config.getInt("site.blogslimit"))

  implicit val formats = DefaultFormats

  def receive: Receive = {
    case Start            =>
      val month = new DateTime(System.currentTimeMillis()).getMonthOfYear
      val day = new DateTime(System.currentTimeMillis()).getDayOfMonth
      val year = new DateTime(System.currentTimeMillis()).getYear

      val blogScheduler = system.actorOf(Props(new WordpressController()))

      system.scheduler.schedule(0.seconds, 30.minutes, blogScheduler, ProcessBlogsView)
    case ProcessBlogsView =>
      val optionalBlogs = getTotalPost("2017-10-01", "2017-11-01")

      optionalBlogs.fold() { blogs =>
        val postByAuthorIds = blogs.posts.groupBy { post =>
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
            Bloggers(blogIds, authorId, authorName, blogIds.size, viewCounts, totalViews)
        }.toList

        val finalResult = result.sortBy(_.totalViews).reverse

        println("total nu,ber of blogs in a month >>>>>>>>>>>" + blogs.found)
        println("result>>>>>>>>>>>" + finalResult.mkString("\n"))
      }
  }

  def getTotalPost(afterDate: String, beforeDate: String): Option[Blogs] = {
    val requestUrl =
      s"""https://public-api.wordpress.com/rest/v1.1/sites/$SITE_NAME/posts
          |?after=%s&before=%s&number=$BLOGS_LIMIT""".stripMargin.replaceAll("\n", "") format(afterDate, beforeDate)

    val eventualResponse = Http.default(dispatch.url(requestUrl) OK as.String)

    JArray(liftParse(eventualResponse()).children).extractOpt[Blogs]
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
}

object Boot extends App {
  implicit val system = ActorSystem("Blogger-Scheduler")

  system.actorOf(Props(new WordpressController)) ! Start
}
