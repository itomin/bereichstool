package beto.beans

import collection.mutable.HashMap
import com.vividsolutions.jts.geom.{GeometryFactory, Coordinate}
import com.vividsolutions.jts.triangulate.quadedge.{Vertex, QuadEdge}

/**
 * Created by IntelliJ IDEA.
 * User: jtomin
 * Date: 27.09.11
 * Time: 09:35
 * To change this template use File | Settings | File Templates.
 */


class DEdge(val origEdge: QuadEdge) {

  import DElement._
  import DGeometry._

  private lazy val angles = new HashMap[Vertex, Int]()
  private lazy val lNext: HashMap[Vertex, DEdge] = new HashMap[Vertex, DEdge]()
  private lazy val rNext: HashMap[Vertex, DEdge] = new HashMap[Vertex, DEdge]()

  lazy val improvable = meanRadius < 1.0 / 2.0 * length
  var factor: Double = 0.0


  def otherPoint(n: DPoint): Vertex = {
    if (origEdge.orig.getX == n.x && n.y == origEdge.orig.getY)
      origEdge.dest
    else
      origEdge.orig
  }

  def length = origEdge.getLength

  def angle(center: DPoint): Int = {
    angles.getOrElseUpdate(center, calcAngle(center))
  }

  def getLNext(n: DPoint) = lNext.getOrElse(n, null)

  def getRNext(n: DPoint) = rNext.getOrElse(n, null)

  def setLNext(p: Vertex, edge: DEdge) = {
    if (!lNext.contains(p)) lNext.put(p, edge)
  }

  def setRNext(p: Vertex, edge: DEdge) = {
    if (!rNext.contains(p)) rNext.put(p, edge)
  }

  def orig: Vertex = origEdge.orig

  def dest: Vertex = origEdge.dest

  def oNext: QuadEdge = origEdge.oNext

  def dNext: QuadEdge = origEdge.dNext

  def radius(center: DPoint): Double = {
    val ed = getLNext(center) :: getRNext(center) :: this :: Nil
    val ne = ed filter (e => (e.angle(center) - angle(center)).abs < 35)
    val me = ne minBy (e => e.minRadius)
    me.minRadius
  }

  def minRadius = if (improvable) meanRadius + factor else 1.0 / 2.0 * length

  override def equals(other: Any): Boolean = {
    other match {
      case e: DEdge => {
        (e.orig == this.orig && e.dest == this.dest) || (e.dest == this.orig && e.orig == this.dest)
      }
      case _ => false
    }
  }

  override def toString = "%s".format(origEdge)

  private def calcAngle(center: DPoint): Int = {
    val o = otherPoint(center)
    val x = center.x
    val y = center.y
    val adj = geomfact.createLineString(Array(new Coordinate(x, y), new Coordinate(x + (x - o.getX).abs, y))).getLength
    val opp = geomfact.createLineString(Array(new Coordinate(x, y), new Coordinate(x, y + (y - o.getY).abs))).getLength

    val phi = if (o.getX > x && o.getY == y) {
      0
    } else if (o.getX > x && o.getY < y) {
      // Q 1
      math.atan(opp / adj) * 180.0 / math.Pi
    } else if (o.getX == x && o.getY < y) {
      90
    } else if (o.getX < x && o.getY < y) {
      // Q 2
      180 - (math.atan(opp / adj) * 180.0 / math.Pi)
    } else if (o.getX < x && o.getY == y) {
      180
    } else if (o.getX < x && o.getY > y) {
      //Q 3
      180 + (math.atan(opp / adj) * 180.0 / math.Pi)
    } else if (o.getX == x && o.getY > y) {
      270
    } else {
      // Q 4
      360 - (math.atan(opp / adj) * 180.0 / math.Pi)
    }
    phi.toInt
  }

}