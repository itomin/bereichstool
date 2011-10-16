package beto.beans

import _root_.beto.log.Logger
import scala.collection.JavaConversions._
import com.vividsolutions.jts.triangulate.ConstraintVertex
import com.vividsolutions.jts.triangulate.quadedge.QuadEdge
import view.SPoint
import com.vividsolutions.jts.geom.{LineString, GeometryFactory, Geometry, Coordinate}


class DPoint(val spoint: SPoint)
  extends ConstraintVertex(new Coordinate(spoint.p.scaledX, spoint.p.scaledY)) with DElement {

  import DElement._

  lazy val geometry = toPolygon(x, y)
  lazy val puffer = toCircle(x, y, minRadius)
  lazy val areaOptimal = toCircle(x, y, meanRadius).getArea

  lazy val x = spoint.p.scaledX
  lazy val y = spoint.p.scaledY
  lazy val coordinate = new Coordinate(x, y)
  lazy val geoPoint = geomfact.createPoint(coordinate)

  var edges: List[DEdge] = List[DEdge]()

  implicit def edgeToLineString(e: QuadEdge): LineString = {
    geomfact.createLineString(Array(e.orig.getCoordinate, e.dest.getCoordinate))
  }

  def eachEdge(f: DEdge => Unit) = edges.foreach(e => f(e))

  def addEdge(e: DEdge) = {
    edges ::= e
  }

  def area(r: Double): Double = math.pow(r, 2) * math.Pi

  def print = {
    edges.foreach{
      e =>
        debug("%s -> %s".format(e.toString, e.getLNext(this).toString))
    }
  }

  override def toString = "<%s, %s>".format(x, y)

  private def toPolygon(x: Double, y: Double): Geometry = {

    /* Kanten nach Winkel aufsteigend sortiert */
    val sortedEdges: List[DEdge] = edges.sortWith((a, b) => a.angle(this) < b.angle(this))

    /* Kreis ist in Wirklichkeit ein Polygon mit 32 Seiten */
    val sides = 24

    /* Minimaler Winkel fungiert als Laufindex */
    val deltaPhi: Int = 360 / sides

    var coords = sortedEdges.flatMap{
      edge =>

        val startPhi = edge.angle(this)
        val nEdge = edge.getLNext(this)
        val endPhi: Int = nEdge.angle(this)

        val startRadius: Double = edge.radius
        val endRadius: Double = nEdge.radius

        val phiDiff = if (sortedEdges.last == edge)
          360 - (startPhi - endPhi).abs
        else
          (startPhi - endPhi).abs

        val steps: Int = (sides / 360.0 * phiDiff).toInt
        val deltaRadius: Double = if (steps != 0) (startRadius - endRadius) / steps else 0

        /* debug("%s (%s, %s) ".format(edge, edge.length, startPhi))
           debug("%s (%s, %s) ".format(nEdge, nEdge.length, endPhi))
           debug("startRadius: %s".format(startRadius))
           debug("endRadius: %s".format(endRadius))
           debug("startPhi: %s".format(startPhi))
           debug("endPhi: %s".format(endPhi))
           debug("steps: %s".format(steps))
           debug("deltaRadius: %s".format(deltaRadius))*/

        var radius = startRadius
        var phi = startPhi

        (0 to steps).map{
          i =>
            val dx = math.cos(-phi * math.Pi / 180) * radius
            val dy = math.sin(-phi * math.Pi / 180) * radius
            radius = radius - deltaRadius
            phi = if ((phi + deltaPhi) > 360) phi + deltaPhi - 360 else phi + deltaPhi
            new Coordinate(x + dx, y + dy)
        }
    }

    coords = coords ++ List(coords.head)

    geomfact.createPolygon(geomfact.createLinearRing(coords.toArray), null)
  }


}
