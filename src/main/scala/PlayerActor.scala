import akka.actor.{Actor, ActorRef, Props}


case class Position(var x: Int, var y: Int)
case class Player(name: String, position: Position, var direction: String, var move: String, var nbTresors: Int)

case object MakeMove
case class ResponseMove(moveAction: Char)

class PlayerActor(player: Player, supervisor: ActorRef) extends Actor {
  override def receive: Receive = {
    case MakeMove =>
      println("move request - " + player.name)
      val nextMove = player.move.charAt(0)
      player.move = player.move.substring(1)
      sender ! ResponseMove(nextMove)
  }
}

object PlayerActor {
  def props(player: Player, supervisor: ActorRef) = Props(new PlayerActor(player, supervisor))
}
