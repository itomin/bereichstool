package beto.beans

import com.vividsolutions.jts.geom.{GeometryFactory, LineString, Geometry, Coordinate}

/**
 * Created by IntelliJ IDEA.
 * User: jtomin
 * Date: 15.10.11
 * Time: 17:49
 * To change this template use File | Settings | File Templates.
 */

object DGeometry {

  val geomfact = new GeometryFactory

  /**
   *
   */
  def tetragon(a: Coordinate, b: Coordinate, c: Coordinate, d: Coordinate): Geometry = {
    geomfact.createPolygon(geomfact.createLinearRing(Array(a, b, c, d, a)), null)
  }

  /**
   *
   */
  def polygon(list: List[Coordinate]): Geometry = {
    geomfact.createPolygon(geomfact.createLinearRing((list.last :: list).toArray), null)
  }

  /**
   *
   */
  def point(c: Coordinate): Geometry = geomfact.createPoint(c)

  /**
   *
   */
  def line(a: Coordinate, b: Coordinate): LineString = {
    geomfact.createLineString(Array(a, b))
  }
}