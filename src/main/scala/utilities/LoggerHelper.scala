package utilities

import org.apache.log4j.Logger

trait LoggerHelper {

  private val logger = Logger.getLogger(this.getClass)

  def info(message: String) = logger.info(message)

  def error(message: String) = logger.error(message)

  def debug(message: String) = logger.debug(message)

  def warn(message: String) = logger.warn(message)

}
