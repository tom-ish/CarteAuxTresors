import scala.collection.mutable.ArrayBuffer

object Tools {
  def isAccessible(map: ArrayBuffer[ArrayBuffer[(Char, String)]], x: Int, y: Int) = {
    ((y >= 0 && y < map.length) && (x >= 0 && x < map(y).length)) && (map(y)(x)._1 == '.' || map(y)(x)._1 == 'T')
  }

}
