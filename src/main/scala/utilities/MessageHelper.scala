package utilities

import controllers.FormattedBlogger

class MessageHelper {

  def getMessage(formattedFinalResult: List[FormattedBlogger],
                 formattedBlogsWithIndex: String,
                 totalPosts: Int): String = {
    val serialNoText = "Sr.No."
    val nameText = "Name"
    val totalBlogsPostedText = "Blog(s) Posted"
    val totalViewsText = "No. of Views"
    val winnerName = formattedFinalResult.headOption.fold("")(_.name)

    val initialText =
      s"Hi Knolders,\n\n" +
        s"Time to reveal some statistics regarding the blogs written by you guys in *${CalendarHelper.getMonthAndYear}*\n\n" +
        s"Total blogs we got this time are *$totalPosts* and total views are *${formattedFinalResult.map(_.totalViews).sum}*\n\n" +
        "Below are the standings for the month - \n\n"

    val header = f"$serialNoText%-10s$nameText%-30s$totalBlogsPostedText%-20s$totalViewsText%-10s%n%n"
    val standings = "```" + header + formattedBlogsWithIndex + "```"
    val endingText =
      s"\n\nAs per the number of views the MVB is : *$winnerName*\n\n" +
        s"Congrats $winnerName :taco: :taco:"

    initialText + standings + endingText
  }
}

object MessageHelper extends MessageHelper
