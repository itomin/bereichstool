package beto.beans

import _root_.beto.log.Logger
import com.vividsolutions.jts.geom.{Coordinate, Geometry}
import com.vividsolutions.jts.math.Vector2D

/**
 * Created by IntelliJ IDEA.
 * User: jtomin
 * Date: 22.10.11
 * Time: 22:45
 * To change this template use File | Settings | File Templates.
 */

object Bender extends Logger {

  import DGeometry._

  type D = Double
  type DE = DElement
  type G = Geometry
  type C = Coordinate
  type LDP = List[DPoint]
  type LDE = List[DElement]
  type LG = List[Geometry]
  type LC = List[Coordinate]

  var dGraph: DelaunayGraph = _
  lazy val dpoints = dGraph.dpoints
  lazy val dranges = dGraph.dranges

  def createInstance(dg: DelaunayGraph) = {
    dGraph = dg
  }




  def shortestPath(from: C, to: C, g: G): LC = {
    val cs = g.getCoordinates.tail.toList

    val (p, q) = cs splitAt (cs indexOf (from))
    val (p1, t1 :: _) = (q ::: p) splitAt (cs indexOf (to))
    val path1 = p1 :+ t1
    val path1Dist = for (i <- 0 to path1.size - 2) yield (path1(i).distance(path1(i + 1)))

    val (k, l) = cs splitAt (cs indexOf (to))
    val (p2, t2 :: _) = (l ::: k) splitAt (cs indexOf (from))
    val path2 = p2 :+ t2
    val path2Dist = for (i <- 0 to path2.size - 2) yield (path2(i).distance(path2(i + 1)))

    if (path1Dist.sum <= path2Dist.sum) path1 else path2
  }

  /**
   *
   */
  /* private def next(f: LDP, c: C): DElement = f minBy (p => c.distance(p.coordinate))*/



  /**
   * Liefert alle nicht ausgewählten Punkte innerhalb der Boundary von
   * Geometrien a und b
   */
  private def foreign(a: G, b: G, offset: Int = 0): LDP = {
    val all = env(a, b, offset)
    val choosen = all filter (p => a.contains(p.geoPoint) || b.contains(p.geoPoint))
    all diff choosen
  }

  /**
   *
   */
  private def env(a: G, b: G, off: Int = 0): LDP = filter(bounds(a, b, off))

  /**
   *
   */
  private def bounds(a: G, b: G, off: Int = 0): G = {
    val coords = (a.getCoordinates ++ b.getCoordinates)
    val sX = coords sortWith ((a, b) => a.x < b.x)
    val sY = coords sortWith ((a, b) => a.y < b.y)
    val maxX = sX.last.x + off
    val minX = sX.head.x - off
    val maxY = sY.last.y + off
    val minY = sY.head.y - off
    tetragon(new C(minX, minY), new C(minX, maxY), new C(maxX, maxY), new C(maxX, minY))
  }

  /**
   *
   */
  private def filter(bounds: G): LDP = dpoints filter (p => bounds.intersects(p.puffer))

}