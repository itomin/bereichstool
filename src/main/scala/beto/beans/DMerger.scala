package beto.beans

import _root_.beto.log.Logger
import com.vividsolutions.jts.geom.{Coordinate, Geometry, LineString}


/**
 * Created by IntelliJ IDEA.
 * User: jtomin
 * Date: 03.10.11
 * Time: 22:43
 * To change this template use File | Settings | File Templates.
 */

object DMerger extends Logger {

  import DElement._
  import DGeometry._

  var dGraph: DelaunayGraph = _
  lazy val dpoints = dGraph.dpoints
  lazy val dranges = dGraph.dranges

  def createInstance(dg: DelaunayGraph) = {
    dGraph = dg
  }


  def mergePolygon(a: ConvexPolygon, b: ConvexPolygon): Geometry = {

    /*    val (fp1, fp2) = facingPoints(a, b)
    val (from, to) = connection((a, fp1), (b, fp2))
    val foreign = foreignPoints(a, b)

    val geo = (from, to) match {
      case (List(a), List(b)) => line(a, b)
      case (List(_, _*), List(_, _*)) => tetragon(from.head, from.last, to.last, to.head)
      case (_, _) => throw new Exception("Verbindung zwischen den konvexen Hüllen konnte nicht gefunden werden!")
    }*/

    val geo = connection(a, b)
    val foreign = foreignPoints(a, b)

    if (foreign.exists(p => geo.intersects(p.geoPoint) || geo.touches(p.geoPoint)))
      bend(geo, foreign)
    else
      geo
  }


  private def bend(geo: Geometry, foreign: List[DPoint]): Geometry = {
    geo
  }

  /**
   * Sucht nach allen umgebenen Punkten, die zwischen zwei konvexen Polygonen liegen
   */
  private def foreignPoints(a: ConvexPolygon, b: ConvexPolygon): List[DPoint] = {
    val choosen = a.dpoints.union(b.dpoints)
    val (maxX: Double, minX: Double, maxY: Double, minY: Double) = dGraph.getBounds(a.convexHull.union(b.convexHull))
    val all: List[DPoint] = dGraph.filter(maxX, minX, maxY, minY)
    all.diff(choosen)
  }


  /**
   * Sucht optimale Verbindungsstelle zwischen zwei konvexen Polygonen
   */
  def connection(a: ConvexPolygon, b: ConvexPolygon): Geometry = {

    val foreign: List[DPoint] = foreignPoints(a, b)
    val (aRand, bRand) = a.randPoints(b)

    def div(c1: Coordinate, c2: Coordinate): Pair[Pair[Coordinate, Coordinate], Pair[Coordinate, Coordinate]] = {
      val l = line(c1, c2)
      ((c1, l.getCentroid.getCoordinate), (l.getCentroid.getCoordinate, c2))
    }

    def find(upper: Pair[Coordinate, Coordinate],
             lower: Pair[Coordinate, Coordinate]): Pair[Option[Geometry], Option[Geometry]] = {

      debug("upper: %s %s".format(upper._1, upper._2))
      debug("lower: %s %s".format(lower._1, lower._2))

      val area = tetragon(upper._1, upper._2, lower._2, lower._1)
      val diff = foreign.filter(e => area.intersects(e.puffer))
      val upperDist = upper._1.distance(upper._2)
      val lowerDist = lower._1.distance(lower._2)

      debug("diff: %s ;  %s  &&  %s".format(diff.isEmpty, upperDist >= minRadius, lowerDist >= minRadius))

      if (!diff.isEmpty) {
        if (upperDist >= minRadius && lowerDist >= minRadius) {
          val (lu, ru) = div(upper._1, upper._2)
          val (ll, rl) = div(lower._1, lower._2)

          val left: Option[Geometry] = find(lu, ll) match {
            case (Some(a), Some(b)) => Some(List(a, b).minBy(_.getArea))
            case (Some(a), None) => Some(a)
            case (None, Some(b)) => Some(b)
            case (None, None) => None
          }

          val right: Option[Geometry] = find(ru, rl) match {
            case (Some(a), Some(b)) => Some(List(a, b).minBy(_.getArea))
            case (Some(a), None) => Some(a)
            case (None, Some(b)) => Some(b)
            case (None, None) => None
          }
          (left, right)
        } else {
          (None, None)
        }
      } else {
        (Some(area), None)
      }
    }

    find(aRand, bRand) match {
      case (Some(a), Some(b)) => List(a, b).minBy(_.getArea)
      case (Some(a), None) => a
      case (None, Some(b)) => b
      case (None, None) => List(line(aRand._1, bRand._1), line(aRand._2, bRand._2)).minBy(_.getLength)
    }

  }


  /*************************************************************************************
   *
   *                                       TRASH
   *
   *************************************************************************************/


  /* /**
   * Findet in den Polygonen a und b zugewandten Eckpunkte
   */
  private def facingPoints(a: ConvexPolygon, b: ConvexPolygon): Pair[List[Coordinate], List[Coordinate]] = {

    /**
     * Lokalisiert die zugewandten Punkte zweier konvexen Polygone, um später eine
     * optimale Verknüpfungsstelle zu bestimmen.
     */
    def locate(a: ConvexPolygon, b: ConvexPolygon): Pair[List[Coordinate], List[Coordinate]] = {

      // Verschmelze die konvexen Hüllen zu einer konvexen Hülle
      val conv = new ConvexHull((a.coordinates.toList ::: b.coordinates.toList).toArray, geomfact).getConvexHull

      // Den Raum zwischen Polygonen a und b bestimmen
      val inner = conv.difference(a.union(b))

      // Gib alle Koordinaten der Eckpunkte
      val coords = conv.getCoordinates

      // Bilde daraus Kanten
      val edges = for (i <- 0 to coords.size - 2) yield ((coords(i), coords(i + 1)))

      // Bestimme Verschmelzungspunkte der Polygone a und b.
      val erg = edges.toList.filter{
        e => (a.contains(e._1) && b.contains(e._2)) || (b.contains(e._1) && a.contains(e._2))
      }

      /*
      * Konvexe Polygone dürfe nur an max. zwei Stellen verschmolzen werden, um wieder
      * eine konvexe Form zu bilden.
      */
      if (erg.size != 2) throw new RuntimeException("Fehler bei der Ermittlung von Verbindungstangenten der Polygone")

      // Randpunkte der jeweiligen Polygone zuordnen
      val aRand: List[Coordinate] = erg flatMap (e => List(e._1, e._2)) filter (c => a.contains(c))
      val bRand: List[Coordinate] = erg flatMap (e => List(e._1, e._2)) filter (c => b.contains(c))

      // Punkte, die mehrfach vorkommen elimieren
      val innerCoos = inner.getCoordinates.toSet.toList

      // Zugewandte Punkte zweier Polygone bestimmen
      val aFaces = innerCoos.filter(c => a.touches(point(c))).diff(aRand)
      val bFaces = innerCoos.filter(c => b.touches(point(c))).diff(bRand)

      ((aRand.head :: aFaces ::: aRand.tail), (bRand.head :: bFaces ::: bRand.tail))
    }

    /**
     * Bildet aus einer Menge der zugewandten Ecken eine geordnete Liste
     *
     */
    def sort(faces: List[Coordinate], cp: ConvexPolygon): List[Coordinate] = {

      /**
       * Lokalisiert den Startpunkt, ab dem die verkettete Liste der
       * zugewandten Eckpunkte gebildet wird.
       */
      def start(from: Int, to: Int): Int = {
        val from_i = if (from + 1 == cp.coordinates.size) 0 else from + 1
        val to_i = if (to + 1 == cp.coordinates.size) 0 else to + 1
        val next_from = cp.coordinates(from_i)
        val next_to = cp.coordinates(to_i)

        //debug("%s != %s  =  %s && %s".format(next_from.toString, cp.coordinates(to).toString, next_from != cp.coordinates(to), faces.contains(next_from)))
        if (next_from != cp.coordinates(to) && faces.contains(next_from))
          from
        else
          to
      }

      /**
       * Bildet eine geordnete Liste der zugewandten Eckpunkten
       */
      def span(start: Int): List[Coordinate] = {
        val bound = cp.coordinates.size
        val iseq: IndexedSeq[Coordinate] = for (i <- 0 to faces.size - 1) yield {
          val index = if (start + i >= bound) (start + i) - bound else start + i
          cp.coordinates(index)
        }
        iseq.toList
      }

      span(start(cp.coordinates.indexOf(faces.head), cp.coordinates.indexOf(faces.last)))
    }

    /**
     *  Klappt die geordnete Liste der zugewandten Punkten um, falls diese
     * über Kreuz liegen.
     */
    def turn(a: List[Coordinate], b: List[Coordinate]): Pair[List[Coordinate], List[Coordinate]] = {

      val begin = line(a.head, b.head)
      val end = line(a.last, b.last)

      if (begin.intersects(end))
        (a, b.reverse)
      else
        (a, b)
    }

    // gegenüberliegende Eckpunkte der Polygone a und b
    val (aFaces, bFaces) = locate(a, b)
    val aFacesSorted = sort(aFaces, a)
    val bFacesSorted = sort(bFaces, b)
    turn(aFacesSorted, bFacesSorted)
  }


  /**
   * Sucht optimale Verbindungsstelle zwischen zwei konvexen Polygonen
   */
  private def connection(p1: Pair[ConvexPolygon, List[Coordinate]],
                         p2: Pair[ConvexPolygon, List[Coordinate]]): Pair[List[Coordinate], List[Coordinate]] = {

    val foreign: List[DPoint] = foreignPoints(p1._1, p2._1)

    def optimalConnection(upper: List[Coordinate],
                          lower: List[Coordinate]): Pair[List[Coordinate], List[Coordinate]] = {

      def div[A](list: List[A]): Pair[List[A], List[A]] = list match {
        case List(_, _) => (list, list)
        case _ => {
          val (a, b) = list.splitAt(list.size / 2)
          (a :+ b.head, b)
        }
      }

      def goLeft(upper: List[Coordinate],
                 lower: List[Coordinate]): Pair[List[Coordinate], List[Coordinate]] = {
        val area = tetragon(upper.head, upper.last, lower.last, lower.head)
        val diff = foreign.filter(e => area.contains(e.geoPoint) || area.touches(e.geoPoint))

        if (!diff.isEmpty) {
          (upper, lower) match {
            case (List(a1, a2), List(b1, b2)) => (List(a1), List(b1))
            case (List(a1, a2, _*), List(b1, b2, _*)) => goLeft(upper.dropRight(1), lower.dropRight(1))
            case (_, _) => throw new Exception("Fehler bei der Ermittlung der optimalen Verbindungsstelle")
          }
        } else {
          (upper, lower)
        }
      }

      def goRight(upper: List[Coordinate],
                  lower: List[Coordinate]): Pair[List[Coordinate], List[Coordinate]] = {

        val area = tetragon(upper.head, upper.last, lower.last, lower.head)
        val diff = foreign.filter(e => area.contains(e.geoPoint) || area.touches(e.geoPoint))

        if (!diff.isEmpty) {
          (upper, lower) match {
            case (List(a1, a2), List(b1, b2)) => (List(a2), List(b2))
            case (List(a1, a2, _*), List(b1, b2, _*)) => goRight(upper.tail, lower.tail)
            case (_, _) => throw new Exception("Fehler bei der Ermittlung der optimalen Verbindungsstelle")
          }
        } else {
          (upper, lower)
        }
      }

      val area = tetragon(upper.head, upper.last, lower.last, lower.head)
      val diff = foreign.filter(e => area.contains(e.geoPoint) || area.touches(e.geoPoint))

      if (!diff.isEmpty) {

        val (lu, ru) = div(upper)
        val (ll, rl) = div(lower)
        val left = goLeft(lu, ll)
        val right = goRight(ru, rl)

        (left, right) match {

          case ((List(a1), List(a2)), (List(b1), List(b2))) => {
            val pair = List((a1, a2), (b1, b2)).minBy(p => line(p._1, p._2).getLength)
            (List(pair._1), List(pair._2))
          }

          case ((List(_), List(_)), (List(_, _*), List(_, _*))) => right

          case ((List(_, _*), List(_, _*)), (List(_), List(_))) => left

          case ((List(_, _*), List(_, _*)), (List(_, _*), List(_, _*))) => {
            List(left, right).minBy{
              list => polygon(list._1 ::: list._2.reverse).getArea
            }
          }
          case (_, _) => throw new Exception("Fehler bei der Ermittlung der zugewandten Eckpunkten")
        }
      } else {
        (upper, lower)
      }
    }

    optimalConnection(p1._2, p2._2)
  }*/


  /*def merge(a: ConvexPolygon, b: ConvexPolygon) = {

    val conLine = linebyCentroid(a, b)
    val orthoA = orthogonal(conLine, a.getCentroid)
    val orthoB = orthogonal(conLine, b.getCentroid)

    var intersecA = a.intersection(orthoA)
    var intersecB = b.intersection(orthoB)

    while (intersecA.getCoordinates.size < 2) {
      //scale
    }

    while (intersecB.getCoordinates.size < 2) {
      //scale
    }

    val shortestConn = {

      val (c1, c2) = intersecA.getCoordinates.map{
        ac => intersecB.getCoordinates.map((ac bc))
      }.minBy((p, q) => c1.distance(c2))

      line(c1, c2)
    }

    connect(a, b, shortestConn)
  }



  /**
   * Liefert eine Gerade, welche die Zentren der beiden
   * Polygone verbindet
   */
  def linebyCentroid(a: Geometry, b: Geometry): LineString = {
    line(a.getCentroid.getCoordinate, b.getCentroid.getCoordinate)
  }




  /**
   * Liefert eine orthogonale Linie zu einer übergebenen Linie
   */
  def orthogonal(l: LineString, ancorPoint: Coordinate): LineString = {
    val rotated90 = rotate(l, 90, ancorPoint)
    val rotateNeg90 = rotate(l, -90, ancorPoint)
    val start = rotated90.getCoordinates.find(c => c != ancorPoint).get
    val end = rotated90.getCoordinates.find(c => c != ancorPoint).get
    line(start, end)
  }


  /**
   *
   */
  def rotate(line: LineString, theta: Double, ancorPoint: Coordinate): Geometry = {
    val affineTransform = AffineTransform.getRotateInstance(theta, ancorPoint.x, ancorPoint.y)
    val mathTransform = new AffineTransform2D(affineTransform)
    JTS.transform(line, mathTransform);
  }*/

}

/*def goLeft(pivot: Int): List[Coordinate] = {
        var done = false
        var list = List[Coordinate]()
        for (i <- 0 to cp.coordinates.size - 1 if !done) yield {
          var index = if (pivot - i >= 0) pivot - i else pivot - i + cp.coordinates.size
          val coo = cp.coordinates(index)
          if (faces.contains(coo))
            list = coo :: list
          else
            done = true
        }
        list
      }

      def goRight(pivot: Int): List[Coordinate] = {
        var done = false
        var list = List[Coordinate]()
        for (i <- pivot to cp.coordinates.size - 1 if !done) yield {
          val coo = cp.coordinates(i)
          if (faces.contains(coo))
            list = list ::: coo :: Nil
          else
            done = true
        }
        list
      }*/

/*@throws(classOf[java.lang.RuntimeException])
def pivotPoint(faces: Set[Coordinate], cp: ConvexPolygon): Int = {
  cp.coordinates.find(c => faces.contains(c)) match {
    case Some(x) => cp.coordinates.indexOf(x)
    case None => throw new RuntimeException("Pivot der zugewandten Eckpunkte konnte nicht ermittelt werden!")
  }
}*/


/*def findPoints(a: ConvexPolygon, b: ConvexPolygon): Pair[Set[Coordinate], Set[Coordinate]] = {
  var cs1 = Set[Coordinate]()
  var cs2 = Set[Coordinate]()
  a.coordinates.foreach{
    c1 =>
      b.coordinates.foreach{
        c2 =>
          val l = line(c1, c2)
          if (a.touches(l) && b.touches(l)) {
            cs1 += c1
            cs2 += c2
          }
      }
  }
  (cs1, cs2)
}*/