package clicker2.networking

import java.net.InetSocketAddress
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.io.{IO, Tcp}
import akka.util.ByteString
import play.api.libs.json.{JsValue, Json}



case object UpdateGames

case object AutoSave

case class GameState(gameState: String)

class ClickerServer extends Actor {

  import Tcp._
  import context.system

  IO(Tcp) ! Tcp.Bind(self, new InetSocketAddress("localhost", 8000))

  var actorMap: Map[String, ActorRef] = Map()
  var clients: Set[ActorRef] = Set()

  override def receive: Receive = {

//    Example of adding an actor with this actor as its supervisor
//    Note that we use the context of this actor and do not create a new actor system
//    val childActor = context.actorOf(Props(classOf[GameActor], username))

    case b: Bound => println("Listening on port: " + b.localAddress.getPort)
    case c: Connected =>
      println("Client Connected: " + c.remoteAddress)
      this.clients = this.clients + sender()
      sender() ! Register(self)
    case PeerClosed =>
      println("Client Disconnected: " + sender())
      this.clients = this.clients - sender()
    case r: Received =>
      println("Received: " + r.data.utf8String)
      val parsed: JsValue = Json.parse(r.data.utf8String)
      val uname: String = (parsed\"username").as[String]
      val action: String = (parsed\"action").as[String]
      if (action == "buyEquipment") {
        val eqID: String = (parsed\"equipmentID").as[String]
        println("ClickServer: buyEquipment " + eqID)
        actorMap(uname) ! BuyEquipment(eqID)
      } else if (action == "connected") {
        println("ClickServer: connected")
        val newActor = system.actorOf(Props(classOf[GameActor], uname))
        if(!actorMap.keySet.contains(uname)) {
          actorMap = actorMap + (uname -> newActor)
          newActor ! Setup
        } else {
          println("Exception: existed username")
        }
      } else if (action == "disconnected") {
        println("ClickServer: disconnected")
        actorMap(uname) ! Save
        actorMap = actorMap - uname
      } else if (action == "clickGold") {
        println("ClickServer: clickGold")
        actorMap(uname) ! ClickGold
      }

    case UpdateGames =>
      actorMap.keySet.foreach((username: String) => actorMap(username) ! Update)
    case AutoSave =>
      actorMap.keySet.foreach((username: String) => actorMap(username) ! Save)
    case gs: GameState =>
      val delimiter = "~"
      this.clients.foreach((client: ActorRef) => client ! Write(ByteString(gs.gameState + delimiter)))

  }
}


object ClickerServer {

  def main(args: Array[String]): Unit = {


    val actorSystem = ActorSystem()

    import actorSystem.dispatcher

    import scala.concurrent.duration._

    val server = actorSystem.actorOf(Props(classOf[ClickerServer]))

    actorSystem.scheduler.schedule(0 milliseconds, 100 milliseconds, server, UpdateGames)
    actorSystem.scheduler.schedule(0 milliseconds, 5000 milliseconds, server, AutoSave)
  }
}
