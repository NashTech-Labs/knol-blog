package services

import java.util.{Calendar, Date}

import dispatch._
import net.liftweb.json.JsonAST.JArray
import net.liftweb.json.{parse => liftParse, _}
import org.joda.time.DateTime
import utilities.ConfigHelper._
import utilities.LoggerHelper

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global

case class Author(ID: Int, login: String, email: Boolean, name: String, first_name: String, last_name: String, nice_name: String,
                  URL: String, avatar_URL: String, profile_URL: String, site_ID: Int)

case class Post(ID: Int, author: Author, title: String)

case class Blogs(found: Int, posts: List[Post])

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

object WordpressService extends WordpressService

class WordpressService extends LoggerHelper {

  implicit val formats = DefaultFormats

  val INITIAL_OFFSET = 100

  def getTotalPost(afterDate: String, beforeDate: String): Option[List[Post]] = {
    try {
      val requestUrl =
        s"""https://public-api.wordpress.com/rest/v1.1/sites/$SITE_NAME/posts
           |?after=%s&before=%s&number=$BLOGS_LIMIT""".stripMargin.replaceAll("\n", "") format(afterDate, beforeDate)

      val eventualResponse = Http.default(dispatch.url(requestUrl) OK as.String)

      val optionalBlogs = JArray(liftParse(eventualResponse()).children).extractOpt[Blogs]

      @tailrec
      def getPostInOffset(found: Int, totalPosts: List[Post], offset: Int): List[Post] = {
        if (offset > found || found == 0) {
          totalPosts
        } else {
          val requestUrl =
            s"""https://public-api.wordpress.com/rest/v1.1/sites/$SITE_NAME/posts
               |?after=%s&before=%s&number=$BLOGS_LIMIT&offset=$offset""".stripMargin.replaceAll("\n", "") format(afterDate, beforeDate)

          val response = executeRequestWithoutAuth(requestUrl)
          val optionalPosts = (liftParse(response) \\ "posts").extractOpt[List[Post]]

          val (newFound, newTotalPosts, newOffset) = optionalPosts.fold(0, totalPosts, offset) { offsetPosts =>
            val filteredPosts = offsetPosts.filterNot(_.title.toLowerCase.contains("knolx"))
            val newTotalPosts = totalPosts ++ filteredPosts

            (found, newTotalPosts, offset + 100)
          }
          getPostInOffset(newFound, newTotalPosts, newOffset)
        }
      }

      optionalBlogs match {
        case Some(blogs) =>
          Some(getPostInOffset(blogs.found, blogs.posts.filterNot(_.title.toLowerCase.contains("knolx")), INITIAL_OFFSET))
        case None        =>
          info(s"No blog found between $afterDate and $beforeDate")
          None
      }
    } catch {
      case ex: Exception =>
        error(s"Got an exception while getting total posts from Wordpress: $ex")
        None
    }
  }

  def getPostViewByPostId(postId: Int): Option[Int] = {
    try {
      val calendar = Calendar.getInstance
      val requestUrl =
        s"""https://public-api.wordpress.com/rest/v1.1/sites/$SITE_NAME/stats/post/$postId"""

      val response = executeRequest(requestUrl)

      val dailyViews =
        (liftParse(response) \\ "data").children.map(_.children).map(jValueList => jValueList.head -> jValueList(1)).toMap
      val blogPostDate =
        DateTime.parse(BlogViews.extractJValue(liftParse(response) \\ "post" \ "post_date").asInstanceOf[String].split(" ").head).toDate

      val totalViews = dailyViews.map(BlogViews(_)).toList.filter { blogViews =>
        calendar.setTime(blogPostDate)
        calendar.add(Calendar.DATE, 31)
        blogViews.date before calendar.getTime
      }.map(_.views).sum

      Some(totalViews)
    } catch {
      case ex: Exception =>
        error(s"Got an exception while getting blog views by blog id $postId from Wordpress: $ex")
        None
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
