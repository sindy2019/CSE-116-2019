package clicker2.networking

import akka.actor.Actor

import clicker2.networking.Database._
import clicker2.Game

case object Update

case object ClickGold

case object Save

case object Setup

case class BuyEquipment(equipmentID: String)

class GameActor(username: String) extends Actor {

  var gm : Game = new Game(username)
  setupTable()
  override def receive: Receive = {
    case Setup =>
      if (!playerExists(username)) {
        println("user does not exists")
        createPlayer(username)
      } else {
        println("user exists")
        loadGame(username, gm)
      }
    case Update =>
      gm.update(System.nanoTime())
      sender() ! GameState(gm.toJSON())
    case Save =>
      saveGame(username,
               gm.gold,
               gm.equipment("shovel").numberOwned,
               gm.equipment("excavator").numberOwned,
               gm.equipment("mine").numberOwned,
               gm.lastUpdateTime)

    case ClickGold =>
      gm.clickGold()
    case buy: BuyEquipment =>
      gm.buyEquipment(buy.equipmentID)
  }
}
