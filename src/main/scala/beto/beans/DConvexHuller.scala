package beto.beans

import _root_.beto.log.Logger
import com.vividsolutions.jts.algorithm.ConvexHull
import com.vividsolutions.jts.geom.{GeometryFactory, Coordinate, Geometry}

/**
 * Created by IntelliJ IDEA.
 * User: jtomin
 * Date: 01.10.11
 * Time: 12:33
 * To change this template use File | Settings | File Templates.
 */

object DConvexHuller extends Logger {

  var dGraph: DelaunayGraph = _
  lazy val dPoints: List[DPoint] = dGraph.dpoints
  lazy val geomfact = dGraph.geomfact

  implicit def comp[A](f: (A, A) => Int) = {
    new Ordering[A] {
      override def compare(a: A, b: A): Int = {
        f(a, b)
      }
    }
  }

  def createInstance(dg: DelaunayGraph) = dGraph = dg

  def hackfleisch(set: List[DElement]): List[ConvexPolygon] = {

    val choosen: List[DPoint] = set.flatMap{
      el => el match {
        case p: DPoint => List(p)
        case r: DRange => r.leafs
      }
    }

    val convGeo: ConvexPolygon = hull(choosen)
    val sortByX = convGeo.coordinates.sortWith((a, b) => a.x < b.x)
    val sortByY = convGeo.coordinates.sortWith((a, b) => a.y < b.y)

    val maxX = sortByX.last.x
    val minX = sortByX.head.x
    val maxY = sortByY.last.y
    val minY = sortByY.head.y

    val foreignPoints: List[DPoint] = dGraph.filter(maxX, minX, maxY, minY).diff(set)

    /* Vertikale Teilung */
    def convexVertical(conv: ConvexPolygon, choosen: List[DPoint], foreign: List[DPoint]): List[ConvexPolygon] = {

      val pointsInPolygon: List[DPoint] = foreign.filter(p => conv.intersects(p))

      /* Horizontale Teilung */
      def convexHorizontal(conv: ConvexPolygon, choosen: List[DPoint], foreign: List[DPoint]): List[ConvexPolygon] = {

        val pointsOnLine: List[DPoint] = foreign.filter(p => conv.contains(p))

        if (pointsOnLine.isEmpty) {
          conv :: Nil
        } else {
          val (upper: List[DPoint], lower: List[DPoint]) = divideHorizontal(choosen, foreign.head)
          convexHorizontal(hull(upper), upper, foreign.tail) ::: convexHorizontal(hull(lower), lower, foreign.tail)
        }
      }

      if (pointsInPolygon.isEmpty) {
        conv :: Nil
      } else {
        val (left, rest, right) = divideVertical(choosen, foreign.head)
        val (leftForeign, restForeign, rightForeign) = divideVertical(foreign, foreign.head)

        val leftSet = if (!left.isEmpty) convexVertical(hull(left), left, leftForeign) else Nil
        val rightSet = if (!right.isEmpty) convexVertical(hull(right), right, rightForeign) else Nil

        val verConv = leftSet ::: rightSet
        val horConv = if (!rest.isEmpty) convexHorizontal(hull(rest), rest, restForeign) else Nil

        verConv ::: horConv
      }
    }

    convexVertical(convGeo, choosen, foreignPoints)
  }

  private def hull(set: List[DPoint]): ConvexPolygon = {
    new ConvexPolygon(set, geomfact)
  }


  private def divideHorizontal(set: List[DPoint], divPoint: DPoint): Pair[List[DPoint], List[DPoint]] = {
    set.partition(_.getY <= divPoint.getY)
  }

  private def divideVertical(set: List[DPoint], divPoint: DPoint): Tuple3[List[DPoint], List[DPoint], List[DPoint]] = {
    val (left, right) = set.partition(_.getX <= divPoint.getX)
    val rest = left.filter(_.getX == divPoint.getX)
    (left.diff(rest), rest, right)
  }


}



