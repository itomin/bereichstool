package beto.beans

import _root_.beto.log.Logger
import marching.{Vertex, Cell}
import scala.collection.JavaConversions._
import com.vividsolutions.jts.triangulate.ConstraintVertex
import com.vividsolutions.jts.triangulate.quadedge.QuadEdge
import com.vividsolutions.jts.geom.{LineString, GeometryFactory, Geometry, Coordinate}
import view.SPoint


class PointModel(val view: SPoint, val raster: Raster)
  extends ConstraintVertex(new Coordinate(view.p.scaledX, view.p.scaledY)) with DElement {

  import DGeometry._
  import DElement._


  private val x = view.p.scaledX
  private val y = view.p.scaledY
  val center = point(new Coordinate(x, y))
  val basicGeometry = circle(x, y, minRadius / 2)
  val wishArea = math.Pi * math.pow(meanRadius, 2)
  val centerCell = raster.nodes.find(cell => cell.data.intersects(center)).get

  val basicCells = raster.intersectCells(basicGeometry)
  val basicVertices: Set[Vertex] = basicCells.flatMap(c => c.vertices.filter(v => basicGeometry.contains(v.vertex)))


  raster.traversableOff(basicCells)
  disable


  override def enable: Set[Cell] = {
    isActiv = true
    /*println("wishArea: %s".format(wishArea))
  println("befor Area: %s".format(raster.getArea(basicCells)))*/
    //raster.activateAllVertices(basicCells)
    val cells = expandCells(basicCells)
    // println("after area: %s".format(raster.getArea(cells)))
    cells
    /*raster.activateAllVertices(basicCells)
    basicCells.foreach(s => println(s.toString))*/
    //    raster.intersectCells(basicGeometry).foreach(s => println(s.toString))
    /*  println("-----------------------------------------------")
  val set:List[Cell] = raster.cells.values.filter(cell => cell.intersects(basicGeometry)).toList
  set.foreach(s => println(s.toString))*/
    //basicCells

  }

  override def disable = {
    isActiv = false
    basicVertices.foreach(v => v.lock)
  }

  private def expandCells(cells: Set[Cell]): Set[Cell] = {

    var topArea: Double = wishArea - raster.getArea(cells)
    var lastArea: Double = 0
    basicVertices.foreach(v => v.activate)
    var topf: Set[Cell] = cells

    def expandCellsRec(cells: Set[Cell]): Set[Cell] = {
      // println("topArea: %s  lastArea: %s".format(topArea, lastArea))
      /* println(" =======  ALLE ZELLEN =======")
      cells.foreach(c => println(c.toString))*/
      // If the area didn't changed after last expand operation, give up. There are no wishArea for Jim!
      if (topArea != lastArea) {

        // memorize the last area
        lastArea = topArea

        //cells.foreach(c => c.enable)

        val randZellen: Set[Cell] = cells.filter(c => !c.isEmpty)

        /*println(" =======  RANDZELLEN =======")
        randZellen.foreach(c => println(c.toString))*/

        val nachbarn: Set[Cell] = randZellen.flatMap(c => raster.getNeighbours(c))

        /* println(" =======  NACHBARN =======")
      nachbarn.foreach(c => println(c.toString))*/

        randZellen.foreach(c => if (!c.isBusy) c.enable)

        // expanded cells
        topf = nachbarn ++ topf

        /* println(" =======  TOPF =======")
      topf.foreach(c => println(c.toString))*/

        /*
        * enable all vertices of the current cells and subtract the
        * result from the topArea
        */
        topArea = wishArea - raster.getArea(topf)

        if (topArea > 300) {
          expandCellsRec(topf)
        } else
          topf
      } else {
        topf
      }
    }

    expandCellsRec(cells)
  }

}


/*
class DPoint(val view: SPoint, val delaunay: DelaunayGraph)
  extends ConstraintVertex(new Coordinate(view.p.scaledX, view.p.scaledY)) with DElement {

  import DGeometry._
  import DElement._

  case class EndPoint(var radius: LineString, val phi: Double, var scalable: Boolean) {
    var lNext: EndPoint = _
    var rNext: EndPoint = _
    var trimmed = false

    def otherPoint(a: Coordinate): Coordinate = {
      val (c1, c2) = (radius.getCoordinateN(0), radius.getCoordinateN(1))
      if (a.x == c1.x && a.y == c1.y) c2 else c1
    }

    def trim(c: Coordinate, g: Geometry) = {
      val cs = radius.intersection(g).getCoordinates
      radius = line(c, cs.sortBy(oc => c.distance(oc)).head)
      trimmed = true
    }
  }

  private val x = view.p.scaledX
  private val y = view.p.scaledY

  val geoOptimal = polygon(edges.map(e => e.otherPoint(center)))
  val areaOptimal = geoOptimal.getArea

  lazy val puffer = circle(x, y, minRadius)
  lazy val ring = circle(x, y, minRadius + minRadius / 2)
  lazy val geometry = toGeometry(geoOptimal)
  lazy val coordinate = new Coordinate(x, y)
  lazy val geoPoint = geomfact.createPoint(coordinate).buffer(2.5)
  lazy val center = new Coordinate(x, y)
  lazy val edges: List[EndPoint] = createCirclePoints

  /* lazy val neighbours = edges map (e => e.otherPoint(this)) collect {
    case d: DPoint => d
  }*/


  implicit def edgeToLineString(e: QuadEdge): LineString = {
    geomfact.createLineString(Array(e.orig.getCoordinate, e.dest.getCoordinate))
  }


  def createCirclePoints: List[EndPoint] = {
    val sides = 16
    val points = (0 to sides - 1).map{
      i =>
        val phi = (i.toDouble / sides.toDouble) * math.Pi * 2.0
        val dx = math.cos(phi) * meanRadius
        val dy = math.sin(phi) * meanRadius
        //println("%s %s %s meanradius %s".format(phi, dx, dy, meanRadius))
        new EndPoint(line(center, new Coordinate(center.x + dx, center.y + dy)), phi, true)
    }

    points.head.rNext = points.last
    points.last.lNext = points.head

    (0 to points.size - 2).foreach(i => points(i).lNext = points(i + 1))
    (1 to points.size - 1).foreach(i => points(i).rNext = points(i - 1))

    points.toList
  }


  def toGeometry(currentGeo: Geometry, offset: Double = 1): Geometry = {

    /* println("-------------------- edges ------------------------------------")
    edges.foreach(e => println(e))
    println("---------------------------------------------------------------")*/

    var geo = polygon(edges.map(e => e.otherPoint(center)))
    val env = delaunay.intersection(geo).filterNot(e => e.tagged)

    /* println("--------------------  env  ------------------------------------")
    env.foreach(e => println(e))
    println("---------------------------------------------------------------")*/

    geo = if (env.isEmpty) {
      geo
    } else {
      edges.foreach{
        e =>
          env.find(el => el.puffer.contains(point(e.otherPoint(center)))) match {
            case Some(g) => {
              e.scalable = false
              e.rNext.scalable = false
              e.lNext.scalable = false
              e.trim(center, g.puffer)
            }
            case None => // nothing to do
          }
      }
      polygon(edges.map(e => e.otherPoint(center)))
    }


    //edges.foreach(e => println("%s  trimmed: %s".format(e, e.trimmed)))

    /* println("--------------------  actArea - areaOptimal  -----------------------")
    println(geo.getArea - areaOptimal)
    println("--------------------------------------------------------------------")*/

    if (geo.getArea - areaOptimal >= 0) {
      geo
    } else {
      val diff = edges.filter(e => e.scalable)
      /*     println("--------------------  scalable  -------------------------------")
      diff.foreach(e => println(e))
      println("---------------------------------------------------------------")*/
      if (diff.isEmpty) {
        geo
      } else {
        diff.foreach{
          e =>
            val dx = math.cos(e.phi) * (meanRadius + offset)
            val dy = math.sin(e.phi) * (meanRadius + offset)
            e.radius = line(center, new Coordinate(center.x + dx, center.y + dy))
        }
        toGeometry(geo, offset + 1)
      }
    }

    /*val other = delaunay.intersection(currentGeo)
    //println(other)

    val (geo, diff: Double) = if (other.isEmpty) {
      (currentGeo, currentGeo.getArea - areaOptimal)
    } else {
      var tempGeo = currentGeo
      other.foreach{
        o => tempGeo = tempGeo.difference(o.puffer)
      }
      tempGeo = tempGeo.buffer(-3)
      (tempGeo, tempGeo.getArea - areaOptimal)
    }

    println("optimal: %s  curr:  %s  diff: %s".format(areaOptimal, geo.getArea, diff))
    println(delaunay.intersection(geo))
    println("----------------------------------------")
    if (diff > 10) {
      geo union puffer
    } else {
      toGeometry(geo.buffer(5))
    }*/


    // Kanten nach Winkel aufsteigend sortiert
    /*    val sortedEdges: List[DEdge] = edges.sortWith((a, b) => a.angle(this) < b.angle(this))

//Kreis ist in Wirklichkeit ein Polygon mit 24 Seiten
val sides = 16

var (opened, closed) = sortedEdges.partition(e => e.isImprovable)

(closed, opened) match {
case (List(), List(_*)) => // geometry is ideal
case (List(_*), List()) => // worst case, geometry is not improvable
case (List(_, _*), List(_, _*)) => {
  var pot = closed.map(e => meanRadius - e.optimalRadius).sum
  var off = pot / opened.size
  while (!opened.isEmpty && pot > 0) {
    opened.foreach{
      e =>
        val rest = e.setOffset(off)
        pot = pot - rest
        if (!e.isImprovable) opened = opened.filterNot(o => o == e)
    }
  }
}
}

// Minimaler Winkel fungiert als Laufindex
val deltaPhi: Int = 360 / sides
var factor = 0

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

  debug("%s (%s, %s) ".format(edge, edge.length, startPhi))
  debug("%s (%s, %s) ".format(nEdge, nEdge.length, endPhi))
  debug("startRadius: %s".format(startRadius))
  debug("endRadius: %s".format(endRadius))
  debug("startPhi: %s".format(startPhi))
  debug("endPhi: %s".format(endPhi))
  debug("steps: %s".format(steps))
  debug("deltaRadius: %s".format(deltaRadius))

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

coords = coords ++ List(coords.head)*/

    // geomfact.createPolygon(geomfact.createLinearRing(coords.toArray), null)
  }

  //  def eachEdge(f: DEdge => Unit) = edges.foreach(e => f(e))

  //def addEdge(e: DEdge) = edges ::= e

  override def toString = "<%s, %s>".format(x, y)


  /*private def optimize(list: List[DPoint], pol: Geometry): Geometry = list match {
    case List() => pol
    case List(_, _*) => if (pol.intersects(list.head.puffer))
      optimize(list.tail, pol.difference(list.head.puffer))
    else
      optimize(list.tail, pol)
  }*/


}
*/
