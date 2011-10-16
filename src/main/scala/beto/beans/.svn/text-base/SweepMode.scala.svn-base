/*
package beto.beans

import _root_.beto.log.Logger
import view.{Position, Network, Element, SPoint}
import java.awt.geom.GeneralPath
import com.vividsolutions.jts.geom.{LineString, Geometry, GeometryFactory, Coordinate}
import java.awt.Shape
import com.vividsolutions.jts.awt.ShapeWriter

/**
 * Created by IntelliJ IDEA.
 * User: jtomin
 * Date: 30.08.11
 * Time: 14:17
 * To change this template use File | Settings | File Templates.
 */
class SweepMode(list: List[Element]) extends Logger {

  import Network._

  /* Sortiert die Element nach steigender X-Koordinate*/
  lazy val listSorted = list.sorted((a: Element, b: Element) => a.p.x compare b.p.x)

  /* Ein Hilfsobjekt für die geometrische Problemstellungen */
  lazy val geomfact = new GeometryFactory

  lazy val shapeWriter = new ShapeWriter


  def envelope(el: List[Element]): Shape = {
    shapeWriter.toShape(toPolygon(el))
  }


  def toPolygon(el: List[Element]) = {

    val max_x = xs(el).last
    val min_x = xs(el).head
    val max_y = ys(el).last
    val min_y = ys(el).head

    val subSet = filtered(max_x, min_x, min_y, max_y)
    val steps = toSweep(subSet)

    debug("Filtered: ")
    subSet.foreach(e => debug("%s radius: %s".format(e, e.radius)))

    val valid: List[Geometry] = el.map(_.bufferedArea)
    val invalid = subSet.map(_.bufferedArea).diff(valid)

    debug("Steps: ")
    steps.foreach(s => debug("%s".format(s)))


    debug("Max Sweep Y: %s, Min Sweep Y: %s".format(max_y, min_y))

    var upperHull = List[Coordinate]()
    var lowerHull = List[Coordinate]()

    steps.foreach{
      s =>
        debug("Sweep Line: %s".format(s.x))

        if (upperHull.isEmpty && lowerHull.isEmpty) {
          upperHull = upperHull :+ s
          lowerHull = lowerHull :+ s
        } else {

          // Sweep Line initialisieren
          val sweepLine: LineString = geomfact.createLineString(
            Array(new Coordinate(s.x, max_y + 5), new Coordinate(s.x, min_y - 5)))

          // Schnittpunkte mit der Sweep Line
          val intersectedCoo: List[Coordinate] = subSet.map{
            // Welche Geometrien schneiden sich mit der SweepLine
            el =>
              val geo = sweepLine.intersection(el.bufferedArea)
              val coos: Array[Coordinate] = geo.getCoordinates

              if (sweepLine.touches(el.bufferedArea))
                List(coos(0))
              else
                coos.toList

          }.flatten.sortWith((a, b) => a.y < b.y)

          // Fall 1: Keine fremde Elemente in der ausgewählten Menge
          val maxCoo = intersectedCoo.last
          val minCoo = intersectedCoo.head

          debug("maxCoo: %s  minCoo: %s".format(maxCoo, minCoo))

          lowerHull = if (lowerHull.last.y <= maxCoo.y)
            lowerHull :+ maxCoo
          else
            lowerHull :+ new Coordinate(maxCoo.x, (lowerHull.last.y - maxCoo.y) / 2 + maxCoo.y)

          upperHull = if (upperHull.last.y >= minCoo.y)
            upperHull :+ minCoo
          else
            upperHull :+ new Coordinate(minCoo.x, (minCoo.y - upperHull.last.y) / 2 + upperHull.last.y)


          /*   debug("lowerHull: ")
        lowerHull.foreach(s => debug("%s".format(s)))
        debug("upperHull:")
        upperHull.foreach(s => debug("%s".format(s)))*/
          // Fall 2: Ein fremdes Element seitwärts in der Menge
          // Fall 3: Ein fremdes Element zwischen den gewählten Elementen
        }
    }

    val lr = geomfact.createLinearRing((lowerHull ::: upperHull.reverse).toArray)
    geomfact.createPolygon(lr, null)
  }


  def toSweep(set: List[Element]): List[Coordinate] = set.map{
    e => List(new Coordinate(e.p.x - e.radius, e.p.y),
      new Coordinate(e.p.x, e.p.y),
      new Coordinate(e.p.x + e.radius, e.p.y))
  }.flatten


  def filtered(max_x: Int, min_x: Int, min_y: Int, max_y: Int): List[Element] = {

    val coos = Array(new Coordinate(min_x, min_y), new Coordinate(min_x, max_y),
      new Coordinate(max_x, max_y), new Coordinate(min_x, max_y),
      new Coordinate(min_x, min_y))
                                                     val lr = geomfact.createLinearRing(coos)
    val poly = geomfact.createPolygon(lr, null)

    listSorted.filter(e => poly.intersects(e.bufferedArea))
  }

  def ys(el: List[Element]) = el.map{
    e => List(e.p.y - e.radius, e.p.y + e.radius)
  }.flatten.sortWith((a, b) => a < b)

  def xs(el: List[Element]) = el.map{
    e => List(e.p.x - e.radius, e.p.x + e.radius)
  }.flatten.sortWith((a, b) => a < b)

}

*/
