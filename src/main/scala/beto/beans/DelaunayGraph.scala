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

  /* Simone-Punkte zu Delaunay-Punkte konvertieren */
  lazy val dpoints: List[DPoint] = spoints.map(s => s.dElement)

  /* */
  lazy val dranges: HashMap[String, DRange] = HashMap[String, DRange]()

  /* Delaunay-Umgebung initialisieren */
  lazy val env = new Envelope

  /* Delaunay-Triangulator initialisieren */
  lazy val dt = new IncrementalDelaunayTriangulator(subdiv)

  /* DPunkte der Subdivision hinzufügen */
  lazy val subdiv = {
    dpoints.foreach(d => env.expandToInclude(d.coordinate))
    new QuadEdgeSubdivision(env, 0)
  }
  /* */
  lazy val geomfact = new GeometryFactory

  /* DCEL Struktur initialisieren. DelaunayGraph ist bereits trianguliert. */
  lazy val edges: List[DEdge] = {
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
  }

  /* Kanten auf die entsprechenden Knoten verteilen */
  edges.foreach{
    e =>
    //debug("orig: %s  dest: %s  length: %s".format(e.orig, e.dest, e.length))
      (e.orig, e.dest) match {
        case (p: DPoint, q: DPoint) => p.addEdge(e); q.addEdge(e)
        case (p: DPoint, _) => p.addEdge(e)
        case (_, q: DPoint) => q.addEdge(e)
        case _ => // some trash
      }
  }

  /* Durchschnittsradius berechnen (harmonischer Mitterlwert)*/
  lazy val relEdges = edges.filter(e => e.orig.isInstanceOf[DPoint] && e.dest.isInstanceOf[DPoint])

  DElement.meanRadius = (relEdges.size / relEdges.map(e => 1.0d / e.length.toInt).sum) / 2
  DElement.minRadius = if (DElement.meanRadius / 3 < 20) 20 else DElement.meanRadius / 3

  DMerger.createInstance(this)

  /*  */
  def toRange(el: Seq[DElement], range: DRange) = {
    range.add(el)
    range.visualize
  }



  def getBounds(geo: Geometry): Tuple4[Double, Double, Double, Double] = {
    val sortByX = geo.getCoordinates.sortWith((a, b) => a.x < b.x)
    val sortByY = geo.getCoordinates.sortWith((a, b) => a.y < b.y)
    (sortByX.last.x, sortByX.head.x, sortByY.last.y, sortByY.head.y)
  }
}
