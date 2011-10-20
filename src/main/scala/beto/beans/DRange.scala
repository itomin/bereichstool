package beto.beans

import view.Element
import view.Range
import com.vividsolutions.jts.algorithm.ConvexHull
import com.vividsolutions.jts.geom._

/**
 * Created by IntelliJ IDEA.
 * User: jtomin
 * Date: 23.09.11
 * Time: 19:03
 * To change this template use File | Settings | File Templates.
 */

object DRange {
  implicit def geometryDistance(a: Geometry) = new {
    def distance(b: Geometry): Double = {
      if (a.touches(b) || a.intersects(b))
        0
      else
        (for (i <- a.getCoordinates; j <- b.getCoordinates) yield (i.distance(j))).min
    }
  }
}

class DRange(val rangeView: Range) extends DElement {

  import Kruskal._
  import DElement._
  import DMerger._
  import DGeometry._

  type ELL = Either[List[DPoint], List[Geometry]]

  private var children = List[DElement]()

  /* Nicht optimierte Bereichsumgebung eines Knotens */
  lazy val range = shapeWriter.toShape(geometry)

  lazy val areaOptimal = children.map(_.areaOptimal).sum

  var geometry: Geometry = _

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
      case el: DPoint => el :: Nil
      case el: DRange => el.leafs
    }
  }


  def visualize = {

    val points = children collect {
      case d: DPoint => d
    }

    geometry = if (isConvex)
      new ConvexHull(points map (p => p.coordinate) toArray, new GeometryFactory).getConvexHull
    else
      merge(points)

    rangeView.deform(shapeWriter.toShape(geometry))
  }


  private def isConvex = false

  private def spline(geo: Geometry): Geometry = geo

  private def merge(list: List[DPoint]): Geometry = {

    var lastCount = list.size

    def join(list: List[Geometry]): Geometry = {

      /*******************************************************************
       *
       *        Suche Verbindung zwischen zwei Polygonen
       *
       *******************************************************************/
      def connect(a: Geometry, b: Geometry): Geometry = {
        connection(a, b) match {
          case ls: LineString => emptyGeometry
          case a: Geometry => a
        }
      }

      /*******************************************************************
       *
       *        Verbinde rekursiv alle Polygone zu einem Bereich
       *
       *******************************************************************/
      def mergeAll(set: List[Geometry]): Geometry = set match {
        case List(a) => a
        case List(a, b, _*) => mergeAll(a.union(b) :: set.drop(2))
        case Nil => throw new RuntimeException("Diese Fehler sollte nie aufgerufen werden!" +
          "\nIrgendwie ist die Bereich verloren gegangen!")
      }

      /*******************************************************************
       *
       *  Verbinde alle Polygone paarweise
       *
       *******************************************************************/
      def mergePaired(list: List[Geometry]): List[Geometry] = {
        mst(list) map (p => p._1 union p._2 union connect(p._1, p._2))
      }

      /*******************************************************************
       *
       *  Merge alle Polygone
       *
       *******************************************************************/
      val erg = list match {
        case List(a) => a
        case List(_, _*) => mergeAll(mergePaired(list))
        case _ => throw new Exception("")
      }

      /*******************************************************************
       *
       *  Prüfe Zusammenhang der entstandenen Topologie
       *
       *******************************************************************/
      if (erg.getNumGeometries > 1)
        if (erg.getNumGeometries < lastCount) {
          lastCount = erg.getNumGeometries
          val geos = for (i <- 0 to erg.getNumGeometries - 1) yield (erg.getGeometryN(i))
          join(geos.toList)
        } else {
          erg //TODO auf die harte Tour
        }
      else
        erg


    }

    join(list map (_.geometry))


    /*******************************************************************
     *
     *  Enthält Topologie Löcher, dann schließe diese
     *
     *******************************************************************/

    /*erg match {
      case d: Polygon => debug("%s".format(polygon(get)))
      case _ => erg
    }*/

  }

  private def mst(list: List[Geometry]): List[Pair[Geometry, Geometry]] = {
    val edges = for (i <- list; j <- list if i != j) yield (Edge(i, j, i.distance(j)))
    kruskal(edges).toList.map(e => (e.v1, e.v2))
  }

  private def optimize(origGeo: Geometry): Geometry = {
    /*
    *  Prüfe, ob sich der Bereich mit den Nachbarn scheidet
    *  und beschneide die Bereichsfläche, falls dies der Falls
    */

    /*var newGeo = cut(neighbours.map(_.geometry), origGeo)
val actArea = newGeo.getArea
val diff = actArea - areaOptimal*/


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
    origGeo
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