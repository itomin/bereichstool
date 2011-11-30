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

class DRange(val view: Range, val delaunay: DelaunayGraph) extends DElement with Mergeable {

  import Kruskal._
  import DElement._
  import DGeometry._

  private var children = List[DElement]()

  /* Nicht optimierte Bereichsumgebung eines Knotens */
  lazy val areaOptimal = children.map(_.areaOptimal).sum
  lazy val raster = delaunay.raster
  override lazy val isLeaf = false

  var geometry: Geometry = emptyGeometry
  var name = view.name

  /*def increment

  def decrement*/

  def ring = puffer.buffer(20).difference(puffer)

  def puffer = geometry

  def exists(e: DElement): Boolean = children.contains(e)

  def eachElement(f: DElement => Unit) = children.foreach(e => f(e))

  def increment = geometry = geometry.buffer(5)

  def decrement = geometry = geometry.buffer(5)

  def add(es: Seq[DElement]): Unit = {
    es.foreach{
      e =>
        add(e)
        e.parent = Some(this)
    }
    review(es.toList)
  }

  def review(es: List[DElement]) = {
    println("%s, %s".format(isConvex(es), geometry.isEmpty))
    geometry = (isConvex(es), geometry.isEmpty) match {
      case (true, true) => convexHull(es).buffer(-3)
      case (true, false) => merge(geometry :: convexHull(es) :: Nil)
      case (false, true) => merge(es.map(_.geometry)) // .buffer(-3)
      case (false, false) => merge(geometry :: es.map(_.geometry))
    }
    paint
  }


  def paint: Unit = {
    val ch = children.collect{
      case r: DRange => r
    }
    if (!ch.isEmpty) increment
    view.deform(shapeWriter.toShape(geometry))
  }

  def add(e: DElement): Unit = {
    if (!this.exists(e)) {
      children ::= e
    } else {
      println("DPoint: " + e + " exists")
    }
  }

  def leafs: List[DPoint] = children.flatMap{
    el => el match {
      case el: DPoint => el :: Nil
      case el: DRange => el.leafs
    }
  }


  private def isConvex(es: List[DElement]): Boolean = {
    val list = delaunay.intersection(convexHull(es))
    println(list)
    val choosen = es.collect{
      case dp: DPoint => dp :: Nil
      case dr: DRange => dr.leafs
    }.flatten
    list.diff(this :: es ::: choosen).isEmpty
  }

  private def spline(geo: Geometry): Geometry = geo

  private def merge(list: List[Geometry]): Geometry = {


    def join(list: List[Geometry]): Geometry = {

      /*******************************************************************
       *
       *        Suche Verbindung zwischen zwei Polygonen
       *
       *******************************************************************/
      def connect(a: Geometry, b: Geometry): Geometry = {

        connection(a, b) match {
          case ls: LineString => a union b
          case c: Geometry => a union b union c
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
        mst(list) map (p => connect(p._1, p._2))
      }

      /*******************************************************************
       *
       *  Merge alle Polygone
       *
       *******************************************************************/
      list match {
        case List(a) => a
        case List(_, _*) => mergeAll(mergePaired(list))
        case _ => throw new Exception("")
      }

    }

    list.foreach(n => raster.traversableOn(n))
    val erg = join(list)
    raster.traversableOff(erg)
    erg
  }

  private def mst(list: List[Geometry]): List[Pair[Geometry, Geometry]] = {
    val edges = for (i <- list;
                     j <- list if i != j) yield (Edge(i, j, i.distance(j)))
    kruskal(edges).toList.map(e => (e.v1, e.v2))
  }

  private def optimize(origGeo: Geometry): Geometry = {
    val env = delaunay.intersection(origGeo).diff(this :: children)
    var newGeo = origGeo
    env.foreach{
      el =>
        newGeo = newGeo.difference(el.puffer)
    }
    newGeo
  }


  /*
    private def neighbours: List[DElement] = {
     all.toSet.diff(choosen.toSet).toList
    }
  */


}

/*private def merge(list: List[Geometry]): Geometry = {

  var lastCount = list.size

  def join(list: List[Geometry]): Geometry = {

    /*******************************************************************
     *
     *        Suche Verbindung zwischen zwei Polygonen
     *
     *******************************************************************/
    def connect(a: Geometry, b: Geometry): Geometry = {

      connection(a, b) match {
        case ls: LineString => a union b
        case c: Geometry => a union b union c
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
      mst(list) map (p => connect(p._1, p._2))
    }

    /*******************************************************************
     *
     *  Merge alle Polygone
     *
     *******************************************************************/
    var erg = list match {
      case List(a) => a
      case List(_, _*) => mergeAll(mergePaired(list))
      case _ => throw new Exception("")
    }

    /*******************************************************************
     *
     *  Prüfe Zusammenhang der entstandenen Topologie
     *
     *******************************************************************/
    if (erg.getNumGeometries > 1) {
      val geos = for (i <- 0 to erg.getNumGeometries - 1) yield (erg.getGeometryN(i))
      if (erg.getNumGeometries < lastCount) {
        lastCount = erg.getNumGeometries
        join(geos.toList)
      } else {
        val paired = mst(geos.toList)
        paired foreach (p => erg = erg.union(bend(p._1, p._2)))
        erg

      }
    } else {
      erg
    }
  }

  join(list)


  /*******************************************************************
   *
   *  Enthält Topologie Löcher, dann schließe diese
   *
   *******************************************************************/

  /*erg match {
    case d: Polygon => println("%s".format(polygon(get)))
    case _ => erg
  }*/

}*/