package utilities

import java.text.SimpleDateFormat
import java.util.Calendar

object CalendarHelper {
  val format = new SimpleDateFormat("yyyy-MM-dd")

  def getFirstDay: String = {
    val calendar = Calendar.getInstance()

    calendar.add(Calendar.MONTH, -2)
    calendar.set(Calendar.DATE, 1)

    format.format(calendar.getTime)
  }

  def getLastDay: String = {
    val calendar = Calendar.getInstance()

    calendar.add(Calendar.MONTH, -1)
    calendar.set(Calendar.DATE, 1)

    format.format(calendar.getTime)
  }

  def getMonthAndYear: String = {
    val calendar = Calendar.getInstance()
    val monthFormat = new SimpleDateFormat("MMM, yyyy")

    calendar.add(Calendar.MONTH, -2)

    monthFormat.format(calendar.getTime)
  }
}
