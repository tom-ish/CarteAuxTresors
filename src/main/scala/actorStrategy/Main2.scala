package actorStrategy

import akka.actor.{ActorSystem, Props}


import scala.io.Source

object Main2 extends App {

  implicit val system = ActorSystem()

  println("start")

  val filename = "test"
  val SEPARATOR = " - "

  val src = Source.fromFile(filename)

  val supervisorActor = system.actorOf(Props[SupervisorActor])

  var lines = src.getLines()
    .map(line => line.split(SEPARATOR))

  var mapLines = lines.foreach(data => {
    supervisorActor ! InitMap(data)
  })

  supervisorActor ! DisplayMap

  supervisorActor ! StartMove

  src.close()
}
