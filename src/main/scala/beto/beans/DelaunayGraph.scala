package beto.beans

import _root_.beto.log.Logger
import view.{Network, Element}
import scala.collection.JavaConversions._
import com.vividsolutions.jts.triangulate.{Segment, IncrementalDelaunayTriangulator, ConformingDelaunayTriangulator}
import com.vividsolutions.jts.awt.ShapeWriter
import beto.beans.view._
import com.vividsolutions.jts.triangulate.quadedge.{Vertex, QuadEdgeSubdivision, QuadEdge}
import collection.mutable.HashMap
import com.vividsolutions.jts.geom.{Geometry, GeometryFactory, Envelope, Coordinate}

/**
 * Created by IntelliJ IDEA.
 * User: jtomin
 * Date: 19.09.11
 * Time: 16:41
 * To change this template use File | Settings | File Templates.
 */

class DelaunayGraph(val spoints: List[SPoint], netview: Network) extends Logger {

  //DElement.minRadius = 20
  debug("DelaunayGraph: %s Punkte.".format(spoints.size))
  lazy val coos: List[Coordinate] = spoints.map(p => new Coordinate(p.p.x, p.p.y))
  lazy val dists: List[Double] = for (i <- coos; j <- coos if i != j) yield (i.distance(j))

  /* val temp = dists.sortWith((a, b) => a < b)
  var i = 0
  temp.foreach{
    e =>
      println(e)
      i += 1
    if(i == 10) System.exit(0)
  }*/

  val dMin = dists.min.toInt / 4.5
  DElement.cellSize = dMin.toInt / 3


  DElement.minRadius = dists.min.toInt
  DElement.meanRadius = DElement.minRadius + 10 // 2 + 10


  println("Closest pair: %s".format(DElement.minRadius))
  println("M %s K: %s".format(dMin, DElement.cellSize))

  val raster = new Raster(netview.w, netview.h, dMin)

  /* Simone-Punkte zu Delaunay-Punkte konvertieren */
  lazy val pointsMap = HashMap(spoints.map{
    s => (s.name, new PointModel(s, raster))
  }.toSeq: _*)

  /* */
  var rangesMap: HashMap[String, DRange] = HashMap[String, DRange]()

  lazy val dpoints = pointsMap.values.toList

  def dranges = rangesMap.values.toList

  /*/* Delaunay-Umgebung initialisieren */
  lazy val env = new Envelope

  /* Delaunay-Triangulator initialisieren */
  lazy val dt = new IncrementalDelaunayTriangulator(subdiv)

  /* DPunkte der Subdivision hinzufügen */
  lazy val subdiv = {
    dpoints.foreach(d => env.expandToInclude(d.coordinate))
    new QuadEdgeSubdivision(env, 3)
  }*/
  /* */
  lazy val geomfact = new GeometryFactory

  /* DCEL Struktur initialisieren. DelaunayGraph ist bereits trianguliert. */
  /* lazy val edges: List[DEdge] = {
    dt.insertSites(dpoints)

    val set: HashMap[QuadEdge, DEdge] = HashMap(subdiv.getEdges.map{
      e => (e.asInstanceOf[QuadEdge], new DEdge(e.asInstanceOf[QuadEdge]))
    }.toSeq: _*)

    //set.keys.foreach(k => debug(k.toString))

    set.foreach{
      e =>
        val orig = e._1.orig
        val dest = e._1.dest
        val origE = e._1.oNext
        val destE = e._1.dNext

        val origWE = set.find{
          p => (p._1.orig == origE.orig && p._1.dest == origE.dest) ||
            (p._1.dest == origE.orig && p._1.orig == origE.dest)
        }.get._2

        val destWE = set.find{
          p => (p._1.orig == destE.orig && p._1.dest == destE.dest) ||
            (p._1.dest == destE.orig && p._1.orig == destE.dest)
        }.get._2

        origWE.setLNext(orig, e._2)
        destWE.setLNext(dest, e._2)
        e._2.setRNext(orig, origWE)
        e._2.setRNext(dest, destWE)
    }
    set.values.toList
  }*/

  /*/* Kanten auf die entsprechenden Knoten verteilen */
  edges.foreach{
    e =>
    //debug("orig: %s  dest: %s  length: %s".format(e.orig, e.dest, e.length))
      (e.orig, e.dest) match {
        case (p: DPoint, q: DPoint) => p.addEdge(e); q.addEdge(e)
        case (p: DPoint, _) => p.addEdge(e)
        case (_, q: DPoint) => q.addEdge(e)
        case _ => // some trash
      }
  }*/

  debug("Delaunay Graph initialisiert")

  /* Durchschnittsradius berechnen (harmonischer Mitterlwert)*/
  //lazy val relEdges = edges.filter(e => e.orig.isInstanceOf[DPoint] && e.dest.isInstanceOf[DPoint])

  /*DElement.meanRadius = 30 //(relEdges.size / relEdges.map(e => 1.0d / e.length.toInt).sum) / 2
  DElement.minRadius = 20*/
  //relEdges.map(e => e.length).min / 2 // DElement.meanRadius / 2 //if (DElement.meanRadius / 3 < 20) 20 else
  //DMerger.createInstance(this)


  //dpoints.foreach(g => raster.traversableOff(g.puffer))


  /*  */
  def toRange(el: Seq[Element], vrange: Range) = {
    debug("update contour: %s".format(vrange.name))
    val drange = ranges(vrange)
    drange.add(el.map(e => elements(e.name)))
  }

  /*def intersection(geo: Geometry): List[DElement] = {
    (dpoints ::: dranges)
      .filter(el => !el.isCovered)
      .filter(el => geo.intersects(el.puffer) && !geo.touches(el.puffer))
  }*/

  private def ranges(vrange: Range): DRange = {
    rangesMap.getOrElseUpdate(vrange.name, new DRange(vrange, this))
  }

  private def elements(key: String): DElement = (pointsMap ++ rangesMap).get(key) match {
    case Some(x) => x
    case None => throw new RuntimeException("Element %s konnte nicht gefunden werden!".format(key))
  }

}
