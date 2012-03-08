package beto.beans

import _root_.beto.log.Logger
import marching.{Vertex, Cell}
import com.vividsolutions.jts.algorithm.ConvexHull
import com.vividsolutions.jts.geom._
import view.{ANode, Element, Range}
import org.geotools.geometry.jts.JTS

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

class DRange(val view: Range, val delaunay: DelaunayGraph) extends DElement with Logger {

  import Kruskal._
  import DElement._
  import DGeometry._
  import Contouring._

  private var children = List[DElement]()


  /* Nicht optimierte Bereichsumgebung eines Knotens */
  //  lazy val areaOptimal = children.map(_.areaOptimal).sum
  lazy val raster = delaunay.raster
  override lazy val isLeaf = false
  var astarCells: Set[ANode] = Set[ANode]()
  var basicGeometry: Geometry = emptyGeometry
  var basicCells: Set[Cell] = Set[Cell]()
  var basicVertices: Set[Vertex] = Set[Vertex]()


  /**
   *
   */
  def leafs: List[PointModel] = {
    children.flatMap{
      el => el match {
        case el: PointModel => el :: Nil
        case el: DRange => el.leafs
      }
    }
  }


  /**
   *
   */
  def exists(e: DElement): Boolean = children.contains(e)


  /**
   *
   */
  def eachElement(f: DElement => Unit) = children.foreach(e => f(e))


  /**
   *
   */
  def add(es: Seq[DElement]): Unit = {
    es.foreach{
      e => add(e)
      e.parent = Some(this)
    }
    review(children.toList)
  }


  /**
   *
   */
  def add(e: DElement): Unit = {
    if (!this.exists(e)) {
      children ::= e
    } else {
      println("DPoint: " + e + " exists")
    }
  }

  /**
   *
   */
  def inactive = {
    basicVertices.foreach(v => v.delete)
  }


  def disable = {
    isActiv = false
    basicVertices.foreach(v => v.disable)
    children.foreach(_.disable)
  }

  def enable: Set[Cell] = {
    isActiv = true
    expandCells
  }

  def expandCells: Set[Cell] = {
    basicVertices.foreach(v => v.activate)

    val randZellen: Set[Cell] = basicCells.filter(c => !c.isEmpty)

    val nachbarn: Set[Cell] = randZellen.flatMap(c => raster.getNeighbours(c))

    randZellen.foreach(c => if (!c.isBusy) c.enable)

    basicCells ++ nachbarn

  }

  /**
   *
   */
  def update(pair: Pair[LinearRing, Array[LinearRing]]) = {
    basicVertices = rangeVertices

    val shell = JTS.smooth(pair._1, 0.0).asInstanceOf[LinearRing]
    val holes = pair._2.map(geo => JTS.smooth(geo, 0.0).asInstanceOf[LinearRing])

    basicGeometry = geomfact.createPolygon(shell, holes)
    //basicGeometry = geomfact.createPolygon(pair._1, pair._2)
    //
    //basicGeometry = shell.symDifference(holes.head)

    astarCells = raster.nodes.filter(as => basicGeometry.intersects(as.data)).toSet

    view.update(shapeWriter.toShape(basicGeometry))
  }

  def rangeVertices: Set[Vertex] = {
    //val all: Set[Vertex] = basicCells.flatMap(c => c.vertices.filter(v => geom.contains(v.vertex)))
    //val sub: Set[Vertex] = children.flatMap(ch => ch.basicVertices.toList).toSet
    //all.diff(sub)
    basicCells.flatMap(c => c.vertices.filter(v => v.isActive)) ++ children.flatMap(c => c.basicVertices)

  }

  /**
   *
   */
  private def review(es: List[DElement]) = {
    debug("review contour %s".format(es.size))
    traversableOn(this :: es)
    time("Gesamt ") {
      basicCells = basicGeometry.isEmpty match {
        case true => collectCells(es)
        case false => inactive; collectCells(es)
      }

      traversableOff(this :: es)

      time("Update") {
        update(time("Contour ") {
          contour(basicCells)
        })
      }
      disable
    }
  }

  private def collectCells(list: List[DElement]): Set[Cell] = {

    debug("collect cells")
    val cells: Set[Cell] = list match {
      case Nil => throw new Exception("Keine Elemente hinzugefügt")

      case List(a) => a.enable

      case List(_, _*) => {
        val m = time("MST ") {
          mst(list)
        }

        m.flatMap{
          //TODO cash einbauen, da knoten sich wiederholen
          case (from: DElement, to: DElement) =>
            val (startCell, endCell) = from.startEnd(to)
            val topoCells = raster.connection(startCell, endCell)
            //val connectedCells = raster.intersectCells(connectionGeom)

            val fromCells = if (!from.isActivate) from.enable else Set.empty[Cell]
            val toCells = if (!to.isActivate) to.enable else Set.empty[Cell]
            (topoCells ++ fromCells) ++ toCells
          //topoCells
        }.toSet

      }
    }
    cells
  }


  private def mst(list: List[DElement]): List[Pair[DElement, DElement]] = {

    val edges = time("DIST ") {
      for (i <- list;
           j <- list if i != j) yield (Edge(i, j, i.distance(j)))
    }
    time("KRUSKAL ") {
      kruskal(edges).toList.map(e => (e.v1, e.v2))
    }
  }

  private def traversableOn(list: List[DElement]) = {
    raster.traversableOn(list.flatMap(el => el.basicCells).toSet)
  }

  private def traversableOff(list: List[DElement]) = {
    raster.traversableOff(list.flatMap(el => el.basicCells).toSet)
  }

  /***************************************************
   *
   *
   *
   *
   *            OLD STUFF
   *
   *
   *
   *
   ***************************************************/


  /*def review(es: List[DElement]) = {
    /*  geometry = (isConvex(es), geometry.isEmpty) match {
      case (true, true) => convexHull(es).buffer(-3)
      case (true, false) => merge(geometry :: convexHull(es) :: Nil)
      case (false, true) => merge(es.map(_.geometry)) // .buffer(-3)
      case (false, false) => merge(geometry :: es.map(_.geometry))
    }
    paint*/
    geometry = contour(es.head.asInstanceOf[PointModel].enabledCells)
    paint
  }*/


  /*  def paint: Unit = {
    /* val ch = children.collect{
    case r: DRange => r
  }
  if (!ch.isEmpty) increment*/
    view.deform(shapeWriter.toShape(geometry))
  }*/


  /*private def isConvex(es: List[DElement]): Boolean = {
    val list = delaunay.intersection(convexHull(es))
    println(list)
    val choosen = es.collect{
      case dp: PointModel => dp :: Nil
      case dr: DRange => dr.leafs
    }.flatten
    list.diff(this :: es ::: choosen).isEmpty
  }*/

  private def spline(geo: Geometry): Geometry = geo

  /*  private def merge(list: List[Geometry]): Geometry = {


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
  }*/


  /*private def optimize(origGeo: Geometry): Geometry = {
    val env = delaunay.intersection(origGeo).diff(this :: children)
    var newGeo = origGeo
    env.foreach{
      el =>
        newGeo = newGeo.difference(el.puffer)
    }
    newGeo
  }*/


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