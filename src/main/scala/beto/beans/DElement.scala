package beto.beans

import _root_.beto.log.Logger
import java.awt.Shape
import com.vividsolutions.jts.awt.ShapeWriter
import com.vividsolutions.jts.geom.{Coordinate, GeometryFactory, Geometry}

/**
 * Created by IntelliJ IDEA.
 * User: jtomin
 * Date: 23.09.11
 * Time: 20:08
 * To change this template use File | Settings | File Templates.
 */

object DElement {

  lazy val shapeWriter = new ShapeWriter

  lazy val geomfact = new GeometryFactory

  /* Durchschnittsradius berechnen (harmonischer Mitterlwert)*/
  var meanRadius: Double = _

  var minRadius: Double = _

  def toCircle(x: Int, y: Int, radius: Double): Geometry = {

    val sides = 32

    var coords = (0 to sides).map{
      i =>
        val angle = (i.toDouble / sides.toDouble) * math.Pi * 2.0
        val dx = math.cos(angle) * radius
        val dy = math.sin(angle) * radius
        new Coordinate(x + dx, y + dy)
    }

    coords = coords :+ coords.head

    geomfact.createPolygon(geomfact.createLinearRing(coords.toArray), null)
  }
}

trait DElement extends Logger {

  import DElement._

  def geometry: Geometry

  val areaOptimal: Double

  def print
}