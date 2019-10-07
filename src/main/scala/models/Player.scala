package models


case class Position(var x: Int, var y: Int)
case class Player(name: String, position: Position, var direction: String, var move: String, var nbTresors: Int)
