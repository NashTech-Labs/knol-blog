package utilities

import org.slf4j.LoggerFactory

trait LoggerHelper {

  val logger = LoggerFactory.getLogger(this.getClass().getName())

}
