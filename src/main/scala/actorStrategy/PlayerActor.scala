package actorStrategy

import akka.actor.{Actor, ActorRef, Props}
import models.Player
import utils.Tools

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


case class ResponseMove(moveAction: Char)
case class MakeMove(players: ListBuffer[(Player, ActorRef)], moveList: mutable.ListBuffer[(Player, ActorRef)], map: mutable.ArrayBuffer[mutable.ArrayBuffer[(Char, String)]])

class PlayerActor(player: Player, supervisor: ActorRef) extends Actor {
  import PlayerActor._

  override def receive: Receive = {
    case MakeMove(players, moveList, map) =>
      val current = moveList.head
      if(current._1.name == player.name && !current._1.move.isBlank) {
        println("move request - " + moveList.head._1.name + " - " + moveList.head._1.move)
        moveList.foreach(println)
        movePlayer(map)

        val pos = players.find((item) => item._1.name == player.name).get._1.position
        pos.x = player.position.x
        pos.y = player.position.y

        // removing the head of the ListBuffer and pushing it back in the end to preserve the order
        moveList.remove(0)
        if(!current._1.move.isBlank) {
          moveList += current
        }
      }

      println("still " + moveList.size + " to move")

      if(moveList.isEmpty)
        supervisor ! AllPlayersMovesMade
      else
        moveList.headOption match {
          case Some((_, playerRef)) => playerRef ! MakeMove(players, moveList, map)
          case None => supervisor ! Error
      }
  }

  def movePlayer(map: mutable.ArrayBuffer[mutable.ArrayBuffer[(Char, String)]]) = {
    val nextMove = player.move(0)
    val direction = player.direction
    player.move = player.move.substring(1)

    var moveDone = true
    direction match {
      case "N" => {
        nextMove match {
          case 'G' => player.direction = "O"
          case 'D' => player.direction = "E"
          case 'A' => moveDone = false
        }
      }
      case "S" => {
        nextMove match {
          case 'G' => player.direction = "E"
          case 'D' => player.direction = "O"
          case 'A' => moveDone = false
        }
      }
      case "E" => {
        nextMove match {
          case 'G' => player.direction = "N"
          case 'D' => player.direction = "S"
          case 'A' => moveDone = false
        }
      }
      case "O" => {
        nextMove match {
          case 'G' => player.direction = "S"
          case 'D' => player.direction = "N"
          case 'A' => moveDone = false
        }
      }
    }

    if(!moveDone) {
      val previousX = player.position.x
      val previousY = player.position.y

      println("[" + player.name + "] =========== [" + previousX + "," + previousY + "] - " + currentPositionSymbol)


      direction match {
        case "N" => player.position.y -= 1
        case "S" => player.position.y += 1
        case "E" => player.position.x += 1
        case "O" => player.position.x -= 1
      }

      if(Tools.isAccessible(map, player.position.x, player.position.y)) {
        // change back the previous position of the player on the map
        map(previousY)(previousX) = currentPositionSymbol

        // store the next position symbol
        currentPositionSymbol = map(player.position.y)(player.position.x) match {
          case ('T', nTresors) =>
            player.nbTresors += 1
            val nbTresor = Integer.valueOf(nTresors)
            if(nbTresor > 1) {
              println("[" + player.name + "] ===========> found treasure")
              ('T', (nbTresor-1).toString)
            } else {
              println("[" + player.name + "] ===========> last treasure taken")
              ('.', "o")
            }
          case emptyPosition => emptyPosition
        }

        // update the new position of the player on the map
        map(player.position.y)(player.position.x) = ('A', s"${player.name(0)}")

        println("[" + player.name + "] =========== [" + previousX + "," + previousY + "] => moving to [" + player.position.x + "," + player.position.y + "]")
      }
      else {
        player.position.x = previousX
        player.position.y = previousY
        println("[" + player.name + "] =========== cannot go there, staying on same position || [" + player.position.x + "," + player.position.y + "]")
      }
    }
    else
      println("[" + player.name + "] =========== [" + player.position.x + "," + player.position.y + "] => turning " + nextMove + " - DIR: " + player.direction)

    println("[" + player.name + "] =========== " + player.move)
    println
  }
}

object PlayerActor {
  def props(player: Player, supervisor: ActorRef) = Props(new PlayerActor(player, supervisor))
  var currentPositionSymbol = ('.', "o")
}
