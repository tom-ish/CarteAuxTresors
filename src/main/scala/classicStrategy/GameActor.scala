package classicStrategy

import java.io.PrintWriter

import akka.actor.{Actor, Props}
import akka.util.Timeout
import models.{Player, Position}
import utils.Tools

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._


case object DisplayMap
case object DisplayPlayers
case class InitMap(data: Array[String])
case object StartMove

class GameActor extends Actor {
  import GameActor._

  implicit val timeout: Timeout = 3 seconds
  implicit val executionContext: ExecutionContext = context.dispatcher

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
      }
    case DisplayMap =>
      map.foreach(c => {
        c.foreach(l =>
          print(l + " "))
        println(" ")
      })
    case DisplayPlayers =>
      players.foreach(println)
    case StartMove =>
      handleMove
      mkOutput
      printWriter.close()
  }

  def initPlayer(player: Player) = {
    players += player
    moveList += player
    map(player.position.y)(player.position.x) = ('A', s"${player.name(0)}")
  }

  /**
   * IMPORTANT PART
   */
  def handleMove() = {
    while(moveList.nonEmpty) {
      moveList.foreach { player =>
        movePlayer(player)
        if(player.move.isBlank)
          moveList -= player
      }
    }
  }

  def movePlayer(player: Player) = {
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

  def mkOutput = {
    printWriter.write(outputStr)
    players.foreach { player =>
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
  }
}

object GameActor {
  var map : mutable.ArrayBuffer[mutable.ArrayBuffer[(Char,String)]] = mutable.ArrayBuffer.empty
  var outputStr = ""

  /**
   * Explanations: ListBuffer is important because this ensures each player's ordering.
   * When a player moves, the ListBuffer data structure makes sure to make the move according
   * to each player's turn.
   */
  var players = ListBuffer[Player]()
  var moveList = ListBuffer[Player]()
  var currentPositionSymbol: (Char, String) = ('.', "o")
  val printWriter = new PrintWriter("output")
  def props = Props(new GameActor)
}