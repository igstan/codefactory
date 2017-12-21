package codefactory

import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory.getLogger

trait Logging {
  implicit val log: Logger = Logger(getLogger(getClass.getName))
}
