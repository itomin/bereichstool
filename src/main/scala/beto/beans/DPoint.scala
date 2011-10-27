package beto.beans

import _root_.beto.log.Logger
import scala.collection.JavaConversions._
import com.vividsolutions.jts.triangulate.ConstraintVertex
import com.vividsolutions.jts.triangulate.quadedge.QuadEdge
import view.SPoint
import com.vividsolutions.jts.geom.{LineString, GeometryFactory, Geometry, Coordinate}


class DPoint(val spoint: SPoint)
  extends ConstraintVertex(new Coordinate(spoint.p.scaledX, spoint.p.scaledY)) with DElement {

  import DGeometry._
  import DElement._

  lazy val geometry = optimize(neighbours, toPolygon(x, y))
  lazy val puffer = circle(x, y, minRadius)
  lazy val areaOptimal = circle(x, y, meanRadius).getArea
  lazy val ring = circle(x, y, 2 * minRadius)

  lazy val x = spoint.p.scaledX
  lazy val y = spoint.p.scaledY
  lazy val coordinate = new Coordinate(x, y)
  lazy val geoPoint = geomfact.createPoint(coordinate)
  lazy val neighbours = edges map (e => e.otherPoint(this)) collect {
    case d: DPoint => d
  }

  var edges: List[DEdge] = List[DEdge]()

  implicit def edgeToLineString(e: QuadEdge): LineString = {
    geomfact.createLineString(Array(e.orig.getCoordinate, e.dest.getCoordinate))
  }

  def eachEdge(f: DEdge => Unit) = edges.foreach(e => f(e))

  def addEdge(e: DEdge) = edges ::= e


  override def toString = "<%s, %s>".format(x, y)

  private def optimize(list: List[DPoint], pol: Geometry): Geometry = list match {
    case List() => pol
    case List(_, _*) => if (pol.intersects(list.head.puffer))
      optimize(list.tail, pol.difference(list.head.puffer))
    else
      optimize(list.tail, pol)
  }

  private def toPolygon(x: Double, y: Double): Geometry = {

    /* Kanten nach Winkel aufsteigend sortiert */
    val sortedEdges: List[DEdge] = edges.sortWith((a, b) => a.angle(this) < b.angle(this))

    /* Kreis ist in Wirklichkeit ein Polygon mit 24 Seiten */
    val sides = 16

    /* Minimaler Winkel fungiert als Laufindex */
    val deltaPhi: Int = 360 / sides

    var coords = sortedEdges.flatMap{
      edge =>

        val startPhi = edge.angle(this)
        val nEdge = edge.getLNext(this)
        val endPhi: Int = nEdge.angle(this)

        val startRadius: Double = edge.radius(this)
        val endRadius: Double = nEdge.radius(this)

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
