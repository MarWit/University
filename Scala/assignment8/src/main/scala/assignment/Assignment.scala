package assignment

import scala.util.Using
import scala.concurrent.{Future, Await}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

import com.restfb.DefaultFacebookClient
import com.restfb.Version
import com.restfb.Parameter
import com.restfb.types.User

object FacebookAdapter {
  private val appSecret = "<appSecret>"

  case class FacebookAccessToken(val value: String)

  class FacebookClient(currentAccessToken: FacebookAccessToken)
    extends DefaultFacebookClient(
              currentAccessToken.value,
              appSecret,
              Version.VERSION_5_0) {

    def getUser(id: String) =
      fetchObject(id, classOf[User], Parameter `with`("fields", "id,name,likes.limit(0).summary(true)"))

    def compareLikes(logFile: String, user1: String, user2: String): Unit = {
      val users: Future[(User, User)] = {
        val f1 = Future { getUser(user1) }
        val f2 = Future { getUser(user2) }

        for { v1 <- f1; v2 <- f2 } yield (v1, v2)
      }

      val result = users map { case (user1, user2) =>
        println(s"${user1.getName}, likes: ${user1.getLikes.getTotalCount} vs. ${user2.getName}, likes: ${user2.getLikes.getTotalCount}")

        Future {
          val CURRENT_DATE = java.util.Calendar.getInstance().getTime()
          Using(new java.io.FileWriter(logFile, true))(_.write(s"$CURRENT_DATE $user1 $user2\n"))
        }
      }

      Await.ready(result, Duration.Inf)
    }
  }

  object FacebookClient {
    def apply()(implicit currentAccessToken: FacebookAccessToken) =
      new FacebookClient(currentAccessToken)
  }
}


object Assignment extends App {
  import FacebookAdapter._

  implicit val accessToken = FacebookAccessToken("<accessToken>")

  val facebookClient = FacebookClient()
  facebookClient compareLikes("/path/to/file", "me", "me")
}
