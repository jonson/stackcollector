package util

import org.slf4j.{LoggerFactory, Logger}

/**
 * A mixin trait which provides subclasses with a Log instance keyed to the
 * subclass's name.
 */
trait Logging {
  protected lazy val log: Logger = LoggerFactory.getLogger(getClass)
}