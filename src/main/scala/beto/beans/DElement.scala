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

  /* Durchschnittsradius berechnen (harmonischer Mitterlwert)*/
  var meanRadius: Double = _

  var minRadius: Double = _


}


trait DElement extends Logger {

  import DElement._
  import DGeometry._

  def geometry: Geometry

  val areaOptimal: Double

  def touches(o: DElement): Boolean = geometry.touches(o.geometry)

  def touches(o: Geometry): Boolean = geometry.touches(geometry)

  def touches(c: Coordinate): Boolean = geometry.touches(point(c))

  def union(o: DElement): Geometry = geometry.union(o.geometry)

  def union(o: Geometry): Geometry = geometry.union(geometry)

  def intersects(o: DElement): Boolean = geometry.intersects(o.geometry)

  def intersects(o: Geometry): Boolean = geometry.intersects(geometry)

  def coordinates: List[Coordinate] = geometry.getCoordinates.toList

  def getArea: Double = geometry.getArea

  def distance(o: DElement): Double = {
    //debug("%s - %s t: %s  i: %s".format(this.toString, o.toString, this.touches(o), this.intersects(o)))
    if (this.touches(o) || this.intersects(o))
      0
    else
      (for (i <- coordinates; j <- o.coordinates) yield (i.distance(j))).min
  }

  def contains(c: Coordinate): Boolean = geometry.contains(point(c))

}