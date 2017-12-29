import akka.actor.{ActorSystem, _}
import com.typesafe.akka.extension.quartz.QuartzSchedulerExtension
import controllers.{ProcessBlogsView, WordpressController}
import net.liftweb.json.{parse => liftParse}

object Boot extends App {
  implicit val system = ActorSystem("Blogger-Scheduler")

  val blogScheduler = system.actorOf(Props(new WordpressController()))

  val scheduler = QuartzSchedulerExtension(system)

  scheduler.schedule("EveryMonth", blogScheduler, ProcessBlogsView)

}
