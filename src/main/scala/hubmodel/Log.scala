package hubmodel

import ch.qos.logback.classic.{Level, Logger, LoggerContext}
import ch.qos.logback.classic.encoder.PatternLayoutEncoder
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.FileAppender
import org.slf4j.LoggerFactory

/** Creates a logger with the specified name. All variables are private as only accessing the logger is needed, the
  * other variables are not required to use the logger.
  *
  * @param name name of the logger
  * @param level level to use, default is set DEBUG
  */
class Log(name: String, level: Level = Level.DEBUG) {
  private val lc: LoggerContext = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]
  private val ple = new PatternLayoutEncoder
  ple.setPattern("%date %level [%thread] %logger{10} [%file:%line] %msg%n")
  ple.setContext(lc)
  ple.start()
  private val fileAppender = new FileAppender[ILoggingEvent]
  fileAppender.setFile(name+".log")
  fileAppender.setEncoder(ple)
  fileAppender.setContext(lc)
  fileAppender.start()
  val logger: Logger = LoggerFactory.getLogger(name).asInstanceOf[Logger]
  logger.addAppender(fileAppender)
  logger.setLevel(level)
  logger.setAdditive(false) /* set to true if root should log too */
}