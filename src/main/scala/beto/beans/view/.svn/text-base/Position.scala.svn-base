package beto.beans.view

import java.awt.geom.Point2D
import scala.math

/**
 * Created by IntelliJ IDEA.
 * User: jtomin
 * Date: 23.06.11
 * Time: 13:45
 * To change this template use File | Settings | File Templates.
 */

class Position(val x: Int, val y: Int) {


  def scaledX: Int = Position.scaleX(x)

  def scaledY: Int = Position.scaleY(y)

  def distance(o: Position) = math.sqrt(math.pow(o.x - x, 2) + math.pow(o.y - y, 2))

}

object Position {
  var scaleX = (a: Int) => a
  var scaleY = (b: Int) => b

}
