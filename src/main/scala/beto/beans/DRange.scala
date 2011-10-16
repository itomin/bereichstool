package beto.beans

import view.Element
import view.Range
import java.awt.Shape
import java.awt.geom.Ellipse2D
import com.vividsolutions.jts.awt.ShapeWriter
import com.vividsolutions.jts.geom.{Coordinate, Geometry}
import com.vividsolutions.jts.algorithm.ConvexHull
import org.geotools.geometry.jts.JTS
import com.vividsolutions.jts.triangulate.quadedge.Vertex


/**
 * Created by IntelliJ IDEA.
 * User: jtomin
 * Date: 23.09.11
 * Time: 19:03
 * To change this template use File | Settings | File Templates.
 */

class DRange(val rangeView: Range) extends DElement {

  import DElement._
  import DConvexHuller._
  import DMerger._

  private var children = List[DElement]()

  /* Nicht optimierte Bereichsumgebung eines Knotens */
  lazy val range = shapeWriter.toShape(geometry)

  lazy val areaOptimal = children.map(_.areaOptimal).sum

  var geometry: Geometry = _

  def isLeaf = children.isEmpty

  def exists(e: DElement): Boolean = children.contains(e)

  def eachElement(f: DElement => Unit) = children.foreach{
    e => f(e)
  }

  def add(e: DElement): Unit = {
    if (!this.exists(e)) {
      children ::= e
    } else {
      println("DPoint: " + e + " exists")
    }
  }

  def add(es: Seq[DElement]): Unit = es.foreach(add(_))

  def leafs: List[DPoint] = children.flatMap{
    el => el match {
      case el: DPoint => List(el)
      case el: DRange => el.leafs
    }
  }


  def visualize = {
    val list = hackfleisch(children)
    geometry = merge(list)
    rangeView.deform(shapeWriter.toShape(geometry))
  }

  def print(): Unit = println("")

  private def spline(geo: Geometry): Geometry = geo


  private def merge(convexSet: List[ConvexPolygon]): Geometry = convexSet match {
    case List(a) => a.convexHull
    case List(a, b, _*) => mergePolygon(a, b) //TODO finde den nächst liegenden
    case _ => throw new Exception("")
  }

  private def optimize(origGeo: Geometry): Geometry = {
    /*
    *  Prüfe, ob sich der Bereich mit den Nachbarn scheidet
    *  und beschneide die Bereichsfläche, falls dies der Falls
    */

    var newGeo = cut(neighbours.map(_.geometry), origGeo)
    val actArea = newGeo.getArea
    val diff = actArea - areaOptimal



    /*    edges.foreach{
      e =>
        e.otherPoint(this) match {
          case p: DPoint => {
            debug("Intersects with %s %s".format(p, origGeo.intersects(p.geometry)))
          }
          case _ => // Nothing to do
        }
    }*/

    /*
    * Falls die Wunschfläche zu klein ausfällt vergrößere diese an möglichen Stellen
    */
    /* var diff = meanArea - origGeo.getArea
    if (diff > 10) {
      /*  val unimprovEdges = edges.filter(_.improvable == false)
   val improvEdges = unimprovEdges.*/
      origGeo
    } else {
      origGeo
    }*/
    newGeo
  }

  /*private def merge(convexSet: List[Geometry], set: List[DElement]): Geometry = {


    def mergeConvex(list: List[Geometry]):Geometry = {
      val src = list.head
      val target = list.tail.minBy(g => src.distance(g))
           src

    }

    def mergeAll(list: List[Geometry], geo: Geometry): Geometry = list match {
      case List(a) => a.union(geo)
      case List(a, _*) => mergeAll(list.tail, a.union(geo))
      case List() => throw new RuntimeException("Bereich hat keine Punkte und kann daher nicht visualisiert werden")
    }

    mergeAll(set.map(_.geometry), mergeConvex(convexSet))
  }*/


  private def cut(neigh: List[Geometry], geo: Geometry): Geometry = neigh match {
    case List() => geo
    case List(a, _*) => if (geo.intersects(a))
      cut(neigh.tail, geo.difference(a))
    else
      cut(neigh.tail, geo)
  }

  private def neighbours: List[DPoint] = {
    val choosen: List[DPoint] = leafs
    val all: List[DPoint] = choosen.flatMap{
      l =>
        val otherPoints: List[DPoint] = l.edges.flatMap{
          e =>
            e.otherPoint(l) match {
              case x: DPoint => List(x)
              case _ => Nil
            }
        }
        l :: otherPoints
    }
    all.toSet.diff(choosen.toSet).toList
  }


}