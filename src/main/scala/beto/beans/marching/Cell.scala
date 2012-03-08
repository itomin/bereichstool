package beto.beans.marching

import _root_.beto.beans.DElement
import _root_.beto.beans.view.ANode
import beto.beans.DGeometry._
import com.vividsolutions.jts.geom.{Coordinate, Geometry}
import LookUpTable._

/**
 * Created by IntelliJ IDEA.
 * User: jtomin
 * Date: 01.12.11
 * Time: 10:16
 * To change this template use File | Settings | File Templates.
 */

object Mode extends Enumeration {
  type Mode = Value
  val LOCKED, ACTIVE, INACTIVE, BUSY = Value
}


case class Vertex(val x: Double, val y: Double, parent: Cell) {


  import Mode._

  val vertex = point(new Coordinate(x, y))

  private var mode = INACTIVE


  def isActive = mode == ACTIVE

  def isLocked = mode == LOCKED

  def isBusy = mode == BUSY

  /**
   *
   */
  def activate = mode = ACTIVE

  /**
   *
   */
  def lock = mode = LOCKED

  /**
   *
   */
  def delete = mode match {
    case LOCKED => // not allowed
    case _ => mode = INACTIVE
  }

  /**
   *
   */
  def enable = mode match {
    case INACTIVE => mode = ACTIVE
    case _ => // not allowed
  }

  /**
   *
   */
  def disable = mode match {
    case ACTIVE => mode = BUSY
    case _ => // not allowed
  }

  /**
   *
   */
  def |(other: Vertex): Int = bit & other.bit


  /**
   *
   */
  def bit: Int = mode match {
    case ACTIVE => 1
    case _ => 0
  }

  def bitArea: Int = mode match {
    case ACTIVE => 1
    case LOCKED => 1
    case _ => 0
  }
}


class Cell(val geom: Geometry, val parent: ANode) {

  import Mode._
  import DElement._

  private val center = geom.getCentroid.getCoordinate
  private val offset = cellSize / 2
  private val OOOI = 1
  private val OOIO = 2
  private val OIOO = 4
  private val IOOO = 8
  private val OOOO = 0
  private val IIII = 15

  val x = center.x
  val y = center.y
  var visited = false
  var row: Int = _
  var col: Int = _

  // Bottom left
  var bl = Vertex(x - offset, y + offset, this)

  // Upper left
  var ul = Vertex(x - offset, y - offset, this)

  // Upper  right
  var ur = Vertex(x + offset, y - offset, this)

  // Bottom right
  var br = Vertex(x + offset, y + offset, this)

  // vertex set
  lazy val vertices: Array[Vertex] = Array(bl, ul, ur, br)


  def activate = vertices.foreach(v => v.activate)

  def disable = vertices.foreach(v => v.disable)

  def lock = vertices.foreach(v => v.lock)

  def enable = {
    vertices.foreach(v => v.enable)
    if (bit == 5 || bit == 10)
      vertices.foreach(v => v.disable)
  }

  def get: List[Pair[C, C]] = lookUp(this)

  def bit: Int = (bl.bit | (ul.bit << 3)) | ((ur.bit << 2) | (br.bit << 1))

  def bitArea: Int = (bl.bitArea | (ul.bitArea << 3)) | ((ur.bitArea << 2) | (br.bitArea << 1))

  def intersects(other: Geometry): Boolean = geom.intersects(other)

  def isEmpty = bit == OOOO || bit == IIII

  def isActive = bit == OOOO || bit == IIII

  def isAmbiguous = bit == 5 || bit == 10

  def isLocked = vertices.exists(v => v.isLocked)

  def isBusy = vertices.exists(v => v.isBusy)

  def activeVertices: Int = vertices.filter(v => v.isActive).size

  def getArea: Double = lookUpArea(this)

  override def toString = "(%s,  %s)  (%s | %s | %s | %s)".format(x, y, bl.bit, ul.bit, ur.bit, br.bit)

  override def equals(that: Any): Boolean = that match {
    case other: Cell => x == other.x && y == other.y
    case _ => false
  }

  //override def hashCode = x
}