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

  //-------------------------------------------------------------------------------------
  //
  //                                  private
  //
  //-------------------------------------------------------------------------------------
  /*private lazy val angles = new HashMap[Vertex, Int]()
  private lazy val lNext: HashMap[Vertex, DEdge] = new HashMap[Vertex, DEdge]()
  private lazy val rNext: HashMap[Vertex, DEdge] = new HashMap[Vertex, DEdge]()
*/

  //-------------------------------------------------------------------------------------
  //
  //                                  public
  //
  //-------------------------------------------------------------------------------------
 /* lazy val optimalRadius = if (meanRadius < length / 2.0) meanRadius else length / 2
  lazy val length = origEdge.getLength
  var offset = 0.0*/


  /*(origEdge.dest, origEdge.orig) match {
    case (p1: DPoint, p2: DPoint) => {
      radii + (p1 -> startRadius)
      radii + (p2 -> startRadius)
    }
    case (p1: DPoint, _) => {
      radii + {
        p1 -> startRadius
      }
    }
    case (_, p2: DPoint) => {
      radii + {
        p2 -> startRadius
      }
    }
    case _ => // does not exist
  }*/


  /*  def currentRadius(p: DPoint): Double = radii.get(p) match {
    case Some(x) => x
    case None => throw new RuntimeException("Kante unvollständig")
  }*/


  /*def otherPoint(n: DPoint): Vertex = {
    if (origEdge.orig.getX == n.x && n.y == origEdge.orig.getY)
      origEdge.dest
    else
      origEdge.orig
  }

  def isImprovable: Boolean = optimalRadius + offset < (length / 2)

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

  def setOffset(off: Double): Double = {
    val rest = (optimalRadius + off - length / 2)
    if (rest > 0) {
      offset = (length / 2) - optimalRadius
      off - offset
    } else if (rest == 0) {
      offset = off
      off
    } else {
      offset = off
      off
    }
  }

  def orig: Vertex = origEdge.orig

  def dest: Vertex = origEdge.dest

  def oNext: QuadEdge = origEdge.oNext

  def dNext: QuadEdge = origEdge.dNext

  def radius(center: DPoint): Double = {
    val ed = getLNext(center) :: getRNext(center) :: this :: Nil
    val ne = ed filter (e => (e.angle(center) - angle(center)).abs < 35)
    val me = ne minBy (e => e.optimalRadius + e.offset)
    me.optimalRadius
  }

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
  }*/

}