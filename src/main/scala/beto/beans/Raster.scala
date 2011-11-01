package beto.beans

import _root_.beto.log.Logger
import view.ANode
import com.vividsolutions.jts.geom.{LineString, Point, Geometry, Coordinate}

/**
 * Created by IntelliJ IDEA.
 * User: jtomin
 * Date: 01.11.11
 * Time: 02:57
 * To change this template use File | Settings | File Templates.
 */


class Raster(width: Int, height: Int, size: Double) extends Logger {

  type C = Coordinate

  import DGeometry._

  //  Anzahl Zellen in die Breite
  private lazy val indexWidth = width / size

  //  Anzahl Zellen in die Höhe
  private lazy val indexHeight = height / size

  //  Darstellungfläche rastern ( 2D - Array )
  lazy val raster = Array.range(0, math.round(indexHeight).toInt).map{
    r => Array.range(0, math.round(indexWidth).toInt).map{
      c =>

      //  jede Zelle ist ein JTS-Rechteck
        val data = tetragon(
          new C(c * size, r * size),
          new C((c + 1) * size, r * size),
          new C((c + 1) * size, (r + 1) * size),
          new C(c * size, (r + 1) * size))

        //  A* - Knoten
        val n = node(data)

        //  Zeile und Spalte setzen, damit A* die Nachbarn der Zelle bestimmen kann
        n.row = r
        n.col = c

        n
    }
  }
  /*debug("width: %s  height  %s  size  %s".format(width, height, size))
  raster.foreach{
    r =>
      debug("--------------------------------------")
      r.foreach{
        c =>
          debug(c.toString)
      }
  }*/

  lazy val nodes: List[ANode[Geometry]] = raster.flatten.toList


  def traversableOn(g: Geometry) = {
    filter(g).foreach(n => n.traversable = true)
  }

  def traversableOff(g: Geometry) = {
    filter(g).foreach(n => n.traversable = false)
  }

  def find(p: Point): ANode[Geometry] = {
    nodes.find(seg => seg.data.contains(p)) match {
      case Some(x) => x
      case None => nodes.find(seg => seg.data.touches(p)) match {
        case Some(x) => x
        case None => throw new RuntimeException("Punkt %s keinem Segment zugeordnet werden".format(p))
      }
    }
  }

  def pathToLine(path: List[ANode[Geometry]]): LineString = {
    line(path.map(n => n.data.getCentroid.getCoordinate))
  }


  /************************************************************************
   *    private
   ***********************************************************************/

  private def filter(g: Geometry): List[ANode[Geometry]] = {
    nodes.filter(n => g.intersects(n.data))
  }

  //  Erstellt einen A* - Knoten
  private def node(cell: Geometry) = {
    ANode(
      cell.getCentroid.getX.toInt,
      cell.getCentroid.getY.toInt,
      cell)
  }

}