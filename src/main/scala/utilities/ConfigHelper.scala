package utilities

import com.typesafe.config.ConfigFactory
import org.joda.time.DateTime

object ConfigHelper {

  val config = ConfigFactory.load()
  val date = new DateTime(System.currentTimeMillis())
  val SITE_NAME = config.getString("site.name")
  val ACCESS_TOKEN = config.getString("site.accessToken")
  val BLOGS_LIMIT = config.getInt("site.blogsLimit")
  val SLACK_API_TOKEN = config.getString("slack.token")
  val CHANNEL_NAME = config.getString("slack.name")
}
