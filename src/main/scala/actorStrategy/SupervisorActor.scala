package actorStrategy

import java.io.PrintWriter

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import models.{Player, Position}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

trait Action
case class InitMap(data: Array[String]) extends Action
case object DisplayMap extends Action
case object StartMove extends Action
case object AllPlayersMovesMade extends Action
case object Error extends Action

class SupervisorActor extends Actor with ActorLogging {
  import SupervisorActor._

  override def receive: Receive = {
    case InitMap(data) =>
      data(0) match {
        case "C " => // {C comme Carte} - {largeur} - {hauteur}
          map = mutable.ArrayBuffer.fill[(Char, String)](data(2).toInt, data(1).toInt)(('.', "o"))
          outputStr += s"C  - ${data(2)} - ${data(1)}\n"
        case "M " =>
          map(data(2).toInt)(data(1).toInt) = ('M', "X")
          outputStr += s"M  - ${data(2)} - ${data(1)}\n"
        case "T " =>
          map(data(2).toInt)(data(1).toInt) = ('T', data(3))
        case "A " =>
          initPlayer(Player(data(1), Position(data(2).toInt, data(3).toInt), data(4), data(5), 0))
          println("init")
      }
    case StartMove =>
      log.info("Start move")
      players.head._2 ! MakeMove(players, moveList, map)
    case DisplayMap =>
      map.foreach(c => {
        c.foreach(l =>
          print(l + " "))
        println(" ")
      })
    case AllPlayersMovesMade =>
      println("All players moves made!")
      mkOutput
    case Error =>
      println("ERROR => SOMETHING HAPPENED")
  }


  private def initPlayer(player: Player) = {
    val playerActor = context.actorOf(Props(new PlayerActor(player, self)))
    players += (player -> playerActor)
    moveList += (player -> playerActor)
    map(player.position.y)(player.position.x) = ('A', s"${player.name(0)}")
  }

  private def mkOutput = {
    printWriter.write(outputStr)
    players.foreach { p =>
      val player = p._1
      for {
        y <- 0 to map.length-1
        x <- 0 to map(y).length-1
      } yield {
        map(y)(x) match {
          case ('T', nbTresors) =>
            printWriter.write("T - " + x + " - " + y + " - " + nbTresors + "\n")
          case _ =>
        }
      }

      printWriter.write("A - " + player.name + " - " + player.position.x + " - " + player.position.y + " - " + player.direction + " - " + player.nbTresors + "\n")
    }
    printWriter.close()
  }
}

object SupervisorActor {
  def props = Props(new SupervisorActor)
  var map: ArrayBuffer[ArrayBuffer[(Char, String)]] = ArrayBuffer.empty
  var players = ListBuffer[(Player, ActorRef)]()
  var moveList = ListBuffer[(Player, ActorRef)]()

  var outputStr = ""
  val printWriter = new PrintWriter("output_actorStrategy")

}
