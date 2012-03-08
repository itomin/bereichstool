package beto.beans

import _root_.beto.log.Logger
import java.awt.Shape
import com.vividsolutions.jts.awt.ShapeWriter
import com.vividsolutions.jts.geom.{Coordinate, GeometryFactory, Geometry}
import marching.{Vertex, Cell}
import view.{ANode, Drawable}

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

  var cellSize: Double = _


  /*def closestCellPair(that: DElement, other: DElement): Pair[Cell, Cell] = {
    val pairs = (for (i <- that.basicCells; j <- other.basicCells) yield (i, j))
    pairs.minBy{
      case (a, b) => a.geom.distance(b.geom)
    }
  }*/
}


trait DElement {

  import DElement._
  import DGeometry._

  var parent: Option[DElement] = None

  def view: Drawable

  var isActiv = false

  val name = view.name

  def raster: Raster

  def basicCells: Set[Cell]

  def basicVertices: Set[Vertex]

  def basicGeometry: Geometry

  def enable: Set[Cell]

  def disable: Unit

  lazy val isLeaf = true

  def distance(other: DElement): Double = {
    val (from, to) = startEnd(other)
    from.data.distance(to.data)
    /*(this, other) match {
      case (a: PointModel, b: PointModel) => a.center.distance(b.center)

      case (a: PointModel, b: DRange) => (for (i <- b.astarCells) yield {
        i.data.getCentroid.distance(a.center)
      }).min

      case (a: DRange, b: PointModel) => (for (i <- a.astarCells) yield {
        i.data.getCentroid.distance(b.center)
      }).min

      case (a: DRange, b: DRange) => (for (i <- a.astarCells; j <- b.astarCells) yield {
        i.data.getCentroid.distance(j.data.getCentroid)
      }).min
    }*/

  }

  def startEnd(other: DElement): Pair[ANode, ANode] = {
    (this, other) match {
      case (a: PointModel, b: PointModel) => (a.centerCell, b.centerCell)

      case (a: PointModel, b: DRange) =>
        val cZ = (for (i <- b.astarCells) yield ((i.data.getCentroid.distance(a.center), i))).minBy(_._1)
        (cZ._2, a.centerCell)

      case (a: DRange, b: PointModel) =>
        val cZ = (for (i <- a.astarCells) yield ((i.data.getCentroid.distance(b.center), i))).minBy(_._1)
        (cZ._2, b.centerCell)

      case (a: DRange, b: DRange) =>
        val cZ = (for (i <- a.astarCells; j <- b.astarCells) yield {
          (i.data.getCentroid.distance(j.data.getCentroid), i, j)
        }).minBy(_._1)
        (cZ._2, cZ._3)
    }
  }

  def isActivate = isActiv

  /*def geometry: Geometry

def puffer: Geometry

def ring: Geometry*/

  //var visited: Boolean = false

  //var tagged = view.isSelected


  //val areaOptimal: Double


  /* def touches(o: DElement): Boolean = geometry.touches(o.geometry)

def touches(o: Geometry): Boolean = geometry.touches(geometry)

def touches(c: Coordinate): Boolean = geometry.touches(point(c))

def union(o: DElement): Geometry = geometry.union(o.geometry)

def union(o: Geometry): Geometry = geometry.union(geometry)

def intersects(o: DElement): Boolean = geometry.intersects(o.geometry)

def intersects(o: Geometry): Boolean = geometry.intersects(geometry)

def coordinates: List[Coordinate] = geometry.getCoordinates.toList

def getArea: Double = geometry.getArea*/

  /*def distance(o: DElement): Double = {
    //debug("%s - %s t: %s  i: %s".format(this.toString, o.toString, this.touches(o), this.intersects(o)))
    if (this.touches(o) || this.intersects(o))
      0
    else
      (for (i <- coordinates; j <- o.coordinates) yield (i.distance(j))).min
  }*/

  /*def contains(c: Coordinate): Boolean = geometry.contains(point(c))*/

  /* def isCovered = parent match {
    case Some(x) => true
    case None => false
  }*/

}