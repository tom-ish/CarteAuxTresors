package classicStrategy

import akka.actor.{ActorSystem, Props}

import scala.io.Source

object Main extends App {

  implicit val system = ActorSystem()

  val filename = "test"
  val SEPARATOR = " - "

  val src = Source.fromFile(filename)

  val gameSupervisor = system.actorOf(Props[GameActor])

  var lines = src.getLines()
    .map(line => line.split(SEPARATOR))

  var mapLines = lines
    .foreach(data => {
      gameSupervisor ! InitMap(data)
    })


  gameSupervisor ! DisplayMap

//  gameSupervisor ! DisplayPlayers

  gameSupervisor ! StartMove

  gameSupervisor ! DisplayMap

  src.close()
}
