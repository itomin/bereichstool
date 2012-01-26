package beto.beans


import _root_.beto.log.Logger
import marching.Cell
import com.vividsolutions.jts.geom.{LineString, Point, Geometry, Coordinate}
import collection.mutable.HashMap
import view.{AStar, ANode}

/**
 * Created by IntelliJ IDEA.
 * User: jtomin
 * Date: 01.11.11
 * Time: 02:57
 * To change this template use File | Settings | File Templates.
 */

object Raster {

  import DGeometry._
  import DElement._

}

class Raster(width: Int, height: Int, nodeSize: Double) extends Logger {

  type C = Coordinate

  import DElement._
  import Raster._
  import DGeometry._

  /**
   *
   */
  implicit def pathToLine(path: List[ANode]): Geometry = {
    path match {
      case List() => emptyGeometry
      case List(a) => a.data
      case List(_, _*) => line(path.map(n => n.data.getCentroid.getCoordinate)).buffer(nodeSize)
    }
  }


  //  Anzahl Zellen in die Breite
  private lazy val indexWidth = width / nodeSize

  //  Anzahl Zellen in die Höhe
  private lazy val indexHeight = height / nodeSize

  //  Darstellungfläche rastern ( 2D - Array )
  private lazy val raster: Array[Array[ANode]] =
    Array.range(0, math.round(indexHeight).toInt).map{
      r => Array.range(0, math.round(indexWidth).toInt).map{
        c =>
        //  jede Zelle ist ein JTS-Rechteck
          val data = tetragon(
            new C(c * nodeSize, r * nodeSize),
            new C((c + 1) * nodeSize, r * nodeSize),
            new C((c + 1) * nodeSize, (r + 1) * nodeSize),
            new C(c * nodeSize, (r + 1) * nodeSize))

          //  A* - Knoten
          val n = node(data, nodeSize)

          //  Zeile und Spalte setzen, damit A* die Nachbarn der Zelle bestimmen kann
          n.row = r
          n.col = c

          n
      }
    }

  private lazy val cells = {
    val map = HashMap[Pair[Int, Int], Cell]()
    raster.foreach{
      nRow => nRow.foreach{
        node => node.cells.foreach(c => map += (c.row, c.col) -> c)
      }
    }
    val w = math.round(indexWidth * nodeSize / cellSize).toInt
    val h = math.round(indexHeight * nodeSize / cellSize).toInt

    for (r <- 0 to h - 1; c <- 0 to w - 1) {
      map.get((r, c)) match {
        case Some(curr) =>
        // set vertices for right neighbour
          map.get((r, c + 1)) match {
            case Some(right) => right.ul = curr.ur; right.bl = curr.br
            case None => // no right neighbour
          }
          // set vertices for bottom neighbour
          map.get((r + 1, c)) match {
            case Some(bottom) => bottom.ul = curr.bl; bottom.ur = curr.br
            case None => // no bottom neighbour
          }
        case None => // nothing to do
      }
    }
    map
  }

  /*  // TEST
 val c0 = cells.getOrElse((10, 51), throw new Exception("bla"))
 println(c0)
 c0.enable
 println(c0)
 println("neighbours:")
 getNeighbours(c0).foreach(c => println(c))
 // TEST*/

  lazy val nodes: List[ANode] = raster.flatten.toList

  lazy val aSTAR = new AStar(nodes)


  def traversableOn(cells: List[Cell]) = {
    cells.foreach(cells => cells.parent.traversable = true)
  }

  def traversableOff(cells: List[Cell]) = {
    cells.foreach(cells => cells.parent.traversable = false)
  }

  def intersectCells(g: Geometry): List[Cell] = filterNodes(g).flatMap{
    n => n.cells.filter(cell => cell.intersects(g))
  }

  def lockVertices(cells: List[Cell], g: Geometry) = cells.foreach{
    cell => cell.vertices.filter(v => g.contains(v.vertex)).foreach(v => v.lock)
    //TODO besser cell.lock und cell.lock(geom) (verdecken der Datenstruktur)
  }

  def activateVertices(g: Geometry) = intersectCells(g).foreach{
    cell => cell.vertices.filter(v => g.contains(v.vertex)).foreach(v => v.activate)
  }

  def activateVertices(cells: List[Cell], g: Geometry) = cells.foreach{
    cell => cell.vertices.filter(v => g.contains(v.vertex)).foreach(v => v.activate)
  }

  def enableVertices(g: Geometry) = intersectCells(g).foreach{
    cell => cell.vertices.filter(v => g.contains(v.vertex)).foreach(v => v.enable)
  }

  def enableVertices(cells: List[Cell], g: Geometry) = cells.foreach{
    cell => cell.vertices.filter(v => g.contains(v.vertex)).foreach(v => v.enable)
  }

  def disableVertices(g: Geometry) = intersectCells(g).foreach{
    cell => cell.vertices.filter(v => g.contains(v.vertex)).foreach(v => v.disable)
  }

  def disableVertices(cells: List[Cell], g: Geometry) = cells.foreach{
    cell => cell.vertices.filter(v => g.contains(v.vertex)).foreach(v => v.disable)
  }

  def enableAllVertices(cells: List[Cell]) = cells.foreach{
    cell => cell.vertices.foreach(v => v.enable)
  }

  def activateAllVertices(cells: List[Cell]) = cells.foreach{
    cell => cell.vertices.foreach(v => v.activate)
  }

  def disableAllVertices(cells: List[Cell]) = cells.foreach{
    cell => cell.vertices.foreach(v => v.disable)
  }

  def inactiveVertices(cells: List[Cell], g: Geometry) = cells.foreach{
    cell => cell.vertices.filter(v => g.contains(v.vertex)).foreach(v => v.delete)
  }

  def getArea(cells: List[Cell]) = cells.map{
    cell => cell.getArea
  }.sum

  /*  def find(p: Point): ANode[Geometry] = {
    nodes.find(segment => segment.data.contains(p)) match {
      case Some(x) => x
      case None => nodes.find(segment => segment.data.touches(p)) match {
        case Some(x) => x
        case None => throw new RuntimeException("Punkt %s kann nicht einem Segment zugeordnet werden".format(p))
      }
    }
  }*/

  /**
   *
   */
  def getNeighbours(cell: Cell): List[Cell] = {
    List((cell.row - 1, cell.col - 1),
      (cell.row - 1, cell.col),
      (cell.row - 1, cell.col + 1),
      (cell.row, cell.col - 1),
      (cell.row, cell.col + 1),
      (cell.row + 1, cell.col - 1),
      (cell.row + 1, cell.col),
      (cell.row + 1, cell.col + 1)).flatMap{
      key => cells.get(key) match {
        case Some(x) => x :: Nil
        case None => Nil
      }
    }

  }


  def findConnection(startCell: Cell, endCell: Cell): Geometry = {
    aSTAR.findPath(startCell.parent, endCell.parent)
  }

  def connection(startCell: Cell, endCell: Cell): List[Cell] = {
    val path = aSTAR.findPath(startCell.parent, endCell.parent)

    path match {
      case List() => Nil
      case List(a) => a.cells
      case List(_, _*) => path.flatMap(pC => pC.cells)
    }
  }


  /************************************************************************
   *    private
   ***********************************************************************/

  private def filterNodes(g: Geometry): List[ANode] = {
    nodes.filter(n => g.intersects(n.data) && !g.touches(n.data))
  }

  //  Erstellt einen A* - Knoten
  private def node(cell: Geometry, size: Double) = {
    ANode(
      cell.getCentroid.getX.toInt,
      cell.getCentroid.getY.toInt,
      size,
      cell)
  }

}