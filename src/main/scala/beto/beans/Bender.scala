package main.scala.beto.beans

import _root_.beto.beans.{DGeometry, DelaunayGraph}
import com.vividsolutions.jts.geom.{Coordinate, Geometry}

/**
 * Created by IntelliJ IDEA.
 * User: jtomin
 * Date: 22.10.11
 * Time: 22:45
 * To change this template use File | Settings | File Templates.
 */

object Bender {

  import DGeometry._
  type G = Geometry

  var dGraph: DelaunayGraph = _
  lazy val dpoints = dGraph.dpoints
  lazy val dranges = dGraph.dranges

  def bend(a: G, b: G): G = {
    

    /* Verbindunslinie zwischen zwei konvexen Hüllen */
    val cLine = connection(a, b)

    /*
    * Zwei konvexen Hüllen zun einer Hülle vereinigen,
    * um den Raum zwischen den Hüllen zu bestimmen
    */
    val abConvHull = a union b convexHull

    /* Fremde Punkte */
    val fo = foreign(a, b) filter (p => cLine intersects p.puffer)


    /*
    * Koordinaten aller Polygone der fremden Punkte bestimmen,
    * die der Verschmelzung im Wege stehen
    */
    val foCoords: Array[Coordinate] = fo flatMap (p => p.puffer.getCoordinates) toArray

    /*
     * Konvexe Hülle aus aller fremden Polygone bestimmen
     */
    val foConvHull = new ConvexHull(foCoords ++ cLine.getCoordinates, geomfact) getConvexHull


    val diff = foConvHull difference abConvHull

    val bLine = bendLine(diff, start(diff.getCoordinates.tail, cLine.getCoordinates))

    bLine.buffer(minRadius, 1, 2)

  }
}