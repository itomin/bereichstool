package beto.beans.marching

import _root_.beto.beans.DElement
import collection.immutable.HashMap
import beto.beans.DGeometry._
import com.vividsolutions.jts.geom.{Coordinate, Geometry}

/**
 * Created by IntelliJ IDEA.
 * User: jtomin
 * Date: 30.11.11
 * Time: 20:31
 * To change this template use File | Settings | File Templates.
 */

object LookUpTable {
  type C = Coordinate
  type G = Geometry
  type F = Function1[Cell, Pair[C, C]]

  import DElement._

  private val offset = cellSize / 2
  val ONE_VERTEX = math.pow(cellSize, 2) / 8
  val HALF = math.pow(cellSize, 2) / 2
  val TWO_VERTICES = 3 * math.pow(cellSize, 2) / 4
  val THREE_VERTICES = 7 * math.pow(cellSize, 2) / 8
  val EMPTY = 0
  val WHOLE = math.pow(cellSize, 2)

  private val table = Array[F](
    (c: Cell) => (new C(0, 0), new C(0, 0)), // 0
    (c: Cell) => (new C(c.x - offset, c.y), new C(c.x, c.y + offset)), // 1
    (c: Cell) => (new C(c.x, c.y + offset), new C(c.x + offset, c.y)), // 2
    (c: Cell) => (new C(c.x - offset, c.y), new C(c.x + offset, c.y)), // 3
    (c: Cell) => (new C(c.x + offset, c.y), new C(c.x, c.y - offset)), // 4
    (c: Cell) => (new C(0, 0), new C(0, 0)), //TODO MultiString
    (c: Cell) => (new C(c.x, c.y + offset), new C(c.x, c.y - offset)), // 6
    (c: Cell) => (new C(c.x - offset, c.y), new C(c.x, c.y - offset)), // 7
    (c: Cell) => (new C(c.x, c.y - offset), new C(c.x - offset, c.y)), // 8
    (c: Cell) => (new C(c.x, c.y - offset), new C(c.x, c.y + offset)), // 9
    (c: Cell) => (new C(0, 0), new C(0, 0)), //TODO MultiString
    (c: Cell) => (new C(c.x, c.y - offset), new C(c.x + offset, c.y)), // 11
    (c: Cell) => (new C(c.x + offset, c.y), new C(c.x - offset, c.y)), // 12
    (c: Cell) => (new C(c.x + offset, c.y), new C(c.x, c.y + offset)), // 13
    (c: Cell) => (new C(c.x, c.y + offset), new C(c.x - offset, c.y)), // 14
    (c: Cell) => (new C(0, 0), new C(0, 0)) // 15
  )

  private val tableArea = Array[Double](
    EMPTY, // 0
    ONE_VERTEX, // 1
    ONE_VERTEX, // 2
    HALF, // 3
    ONE_VERTEX, // 4
    TWO_VERTICES, // 5
    HALF, // 6
    THREE_VERTICES, // 7
    ONE_VERTEX, // 8
    HALF, // 9
    TWO_VERTICES, //10
    THREE_VERTICES, // 11
    HALF, // 12
    THREE_VERTICES, // 13
    THREE_VERTICES, // 14
    WHOLE // 15
  )

  def lookUp(c: Cell): Pair[C, C] = {
    //table(c.bit)(c)
  }

  def lookUpArea(c: Cell): Double = tableArea(c.bitArea)
}