package beto.beans

import _root_.beto.log.Logger
import com.vividsolutions.jts.algorithm.ConvexHull
import com.vividsolutions.jts.geom._
import com.vividsolutions.jts.operation.buffer.BufferOp
import com.vividsolutions.jts.math.Vector2D

/**
 * Created by IntelliJ IDEA.
 * User: jtomin
 * Date: 03.10.11
 * Time: 22:43
 * To change this template use File | Settings | File Templates.
 */

object DMerger extends Logger {

  type PCC = Pair[Coordinate, Coordinate]
  type D = Double
  type DE = DElement
  type G = Geometry
  type C = Coordinate
  type LDP = List[DPoint]
  type LDE = List[DElement]
  type LG = List[Geometry]
  type LC = List[Coordinate]


  import DElement._
  import DGeometry._

  var dGraph: DelaunayGraph = _
  lazy val dpoints = dGraph.dpoints
  lazy val dranges = dGraph.dranges


  def createInstance(dg: DelaunayGraph) = {
    dGraph = dg
  }

  //TODO Gesamtkonzept dieser Klasse müsste überarbeitet werden.

  /**
   * Verbindung c zwischen Geometrien a und b muss
   * der Umgebung entsprechend verbogen werden.
   */
  def bend(a: G, b: G): G = {

    val (lTang, rTang) = randPoints(a, b)
    val cTang = line(a.getCentroid.getCoordinate, b.getCentroid.getCoordinate)

    debug("lTang: %s".format(lTang))
    debug("rTang: %s".format(rTang))
    debug("center: %s".format(cTang))

    val left = findBendConnection(a, b, line(lTang._1, lTang._2))
    val center = findBendConnection(a, b, cTang)
    val right = findBendConnection(a, b, line(rTang._1, rTang._2))

    val opt = List(left, center, right).collect{
      case Some(x) => x
    }
    if (opt.isEmpty)
      emptyGeometry
    else {
      val min = opt.minBy(l => length(l))
      debug("Min: %s".format(min))
      min.buffer(minRadius / 2)
    }

    /*val c = (left, center, right) match {
      case (None, None, None) => emptyGeometry
      case (None, Some(x)) => x
      case (Some(x), None) => x
      case (Some(x), Some(y)) => List(x, y).minBy(l => length(l))
    }*/

  }

  def length(l: LineString): Double = {
    val coos = l.getCoordinates
    val dist = for (i <- 0 to coos.size - 2) yield (coos(i).distance(coos(i + 1)))
    dist.sum
  }

  def findBendConnection(a: G, b: G, tangente: LineString): Option[LineString] = {
    debug("======================== find bend connection begin ======================== ")
    debug("tangente %s".format(tangente.toString))

    /*def findBendOutside(a: G, b: G, tangente: LineString, sub: LG): Option[LineString] = {
      debug("======================== find bend outside begin ======================== ")
      val distCoos = sub.flatMap(d => d.getCoordinates)
      val mainDisturber = new ConvexHull(distCoos.toArray, geomfact).getConvexHull
      val deathZone = foreign(a :: b :: sub, minRadius.toInt).map(d => d.ring)
      debug("sub:")
      printGeo(sub)
      debug("deathZone: %s".format(foreign(a :: b :: sub, minRadius.toInt)))
      val outsideParts = splitPolygon(mainDisturber, tangente)
      val outside = outsideParts map (b => addBendToTangente(tangente, mainDisturber, b))

      val erg = outside match {
        case Nil => None
        case List(_, _*) => {
          optimal(outside, deathZone, sub) match {
            case Right(lg) => findBendOutside(a, b, tangente, lg ::: sub)
            case Left(a) => Some(a)
          }
        }
      }
      debug("======================== find bend outside end ======================== ")
      erg
    }*/

    def findBendInside(a: G, b: G, tangente: LineString, toSplit: G): Option[LineString] = {
      debug("======================== find bend inside begin ======================== ")
      debug("toSplit contains: ")
      printGeo(toSplit)
      debug("deathZone: %s".format(foreign(a :: b :: toSplit :: Nil, minRadius.toInt)))
      val foreignElements = foreign(a :: b :: toSplit :: Nil, minRadius.toInt)
      val deathZone = foreignElements.map(d => d.ring)
      var bend = tangente

      //  dist.ring to convex
      //  if convex contains tangente.getCooridnate(0) ||  convex.contains.getCoordinate(1)
      //  split(dist,ring) else split(convex)
      val erg = optimal(splitPolygon(toSplit, tangente), deathZone) match {
        case Left(x) => debug("Left"); Some(addBendToTangente(bend, toSplit, x))
        case Right(disturber) => {
          debug("Right")
          printGeo(disturber)
          disturber match {
            case List() => None
            case List(_*) => {
              val mergDisturber = disturber.map{
                g =>
                  connection(toSplit, g) match {
                    case x: LineString => toSplit union g
                    case x: Geometry => toSplit union g union x
                  }

              }
              val newToSplit = unionAll(mergDisturber)
              for (el <- dpoints) (if (newToSplit.contains(el.geoPoint)) el.visited = true)
              findBendInside(a, b, bend, geomfact.createGeometry(newToSplit))
            }
          }
        }
      }

      debug("======================== find bend inside end ======================== ")
      erg

    }

    def optimal(options: List[LineString], deathZone: LG): Either[LineString, LG] = {
      debug("======================== optimal begin ======================== ")
      debug("options %s".format(options.size))
      val erg = if (options.isEmpty) {
        Right(Nil)
      } else {
        val paths = options filter (l => !deathZone.exists(f => f.intersects(l.buffer(minRadius / 2))))
        val disturber = deathZone.filter(g => options.exists(l => g.intersects(l.buffer(minRadius / 2))))

        paths match {
          case List() => debug("List()"); Right(disturber)

          case List(a) => {
            debug("List(a)")
            val worst = options.sortBy(l => length(l)).last
            worst == a match {
              case true => Right(disturber)
              case false => Left(a)
            }
          }
          case List(_, _*) => debug("List(_;_*)"); Left(paths.sortBy(g => length(g)).head)
        }
      }
      debug("======================== optimal end ======================== ")
      erg
    }

    def addBendToTangente(tang: LineString, ring: G, bend: LineString): LineString = {

      val (from, to) = farthestPair(tang.intersection(ring).getCoordinates.toList)
      val diffTang = tang.difference(ring).getCoordinates.toList
      val (p1, _) = diffTang.splitAt(diffTang.indexOf(from))
      val (_, _ :: p2) = diffTang.splitAt(diffTang.indexOf(to))
      val bends = bend.getCoordinates.toList

      if (p1.last.distance(bends.head) < p1.last.distance(bends.last))
        line(p1 ::: bends ::: p2)
      else
        line(p1 ::: bends.reverse ::: p2)
    }

    val sub = intersection(foreign(a :: b :: Nil, minRadius.toInt), tangente)
    debug("Subset: %s".format(sub.toString))

    var insideTang = tangente
    var isPassable = true
    for (el <- sub if !el.visited && isPassable) {
      el.visited = true
      findBendInside(a, b, insideTang, el.ring) match {
        case Some(x) => insideTang = x
        case None => isPassable = false
      }
    }
    deinitialize

    val ch = new ConvexHull(sub.flatMap(el => el.ring.getCoordinates).toArray, geomfact)
    val outside = findBendInside(a, b, tangente, ch.getConvexHull)
    deinitialize
    // val outside = findBendOutside(a, b, tangente, sub)

    debug("======================== find bend connection end ======================== ")
    val inside = isPassable match {
      case true => Some(line(insideTang.getCoordinates))
      case false => None
    }

    (outside, inside) match {
      case (None, None) => None
      case (Some(x), None) => Some(x)
      case (None, Some(x)) => Some(x)
      case (Some(x), Some(y)) => Some(List(x, y).sortBy(g => length(g)).head)
    }

  }

  def splitPolygon(g: G, splitter: G): List[LineString] = {
    debug("======================== split polygon begin ======================== ")
    debug("splitter %s".format(splitter))
    /*debug("touches %s".format(splitter.touches(g)))
    debug("intersects %s".format(splitter.intersects(g)))*/

    def split(from: C, to: C, g: G): List[LineString] = {
      debug("======================== split begin ======================== ")
      debug("from  %s  to  %s".format(from, to))


      val cs = g.getCoordinates.tail.toList
      debug(cs.toString)
      val (p, q) = cs splitAt (cs indexOf (from))
      var turned = (q ::: p)
      val (part1, part2) = turned splitAt (turned indexOf (to))
      val path1 = part1 :+ part2.head

      val (k, l) = cs splitAt (cs indexOf (to))
      turned = (l ::: k)
      val (part11, part12) = (l ::: k) splitAt (turned indexOf (from))
      val path2 = part11 :+ part12.head
      debug("======================== split end ======================== ")
      List(line(path1), line(path2))
    }
    if (splitter.getCoordinates.exists(c => g.contains(point(c)))) {
      Nil
    } else {
      val splitted = g.intersection(splitter)
      val iCoos = splitted.getCoordinates
      val toSplit = g.difference(splitter)

      val erg = iCoos match {
        case Array(a, b) => split(iCoos(0), iCoos(1), toSplit)
        case Array(_, _*) => {
          val (from, to) = farthestPair(iCoos.toList)
          split(from, to, toSplit)
        }
        case Array() => Nil
      }
      debug("======================== split polygon end ======================== ")
      erg
    }
  }


  def farthestPair(list: List[Coordinate]): Pair[C, C] = {
    val paired = for (i <- list;
                      j <- list) yield (i, j)
    val sorted = paired sortWith ((p1, p2) => p1._1.distance(p1._2) > p2._1.distance(p2._2))
    sorted.head
  }

  def scale(a: G, b: G): Pair[PCC, PCC] = {
    val (lTan: PCC, rTan: PCC) = randPoints(a, b)

    val ac = a.getCentroid.getCoordinate
    val bc = b.getCentroid.getCoordinate

    val ac_lo = new Vector2D(ac, lTan._1)
    val ac_lu = new Vector2D(bc, lTan._2)
    val bc_ro = new Vector2D(ac, rTan._1)
    val bc_ru = new Vector2D(bc, rTan._2)

    val oFac = 2 * minRadius / line(lTan._1, rTan._1).getLength
    val uFac = 2 * minRadius / line(lTan._2, rTan._2).getLength

    val lo = new Vector2D(lTan._1.x, lTan._1.y).add(ac_lo.multiply(oFac))
    val lu = new Vector2D(lTan._2.x, lTan._2.y).add(ac_lu.multiply(uFac))
    val ro = new Vector2D(rTan._1.x, rTan._1.y).add(bc_ro.multiply(oFac))
    val ru = new Vector2D(rTan._2.x, rTan._2.y).add(bc_ru.multiply(uFac))

    ((new C(lo.getX, lo.getY), new C(ro.getX, ro.getY)),
      (new C(lu.getX, lu.getY), new C(ru.getX, ru.getY)))
  }


  /*  def shortestPath(a: G, conn: G): G = {
    val rest = a.difference(conn)

    if (rest.getNumGeometries == 1) {
      a.intersection(conn)
    } else if (rest.getNumGeometries == 2) {
      Seq(rest.getGeometryN(0), rest.getGeometryN(1)).minBy(g => g.getArea)
    } else {
      a
    }
  }*/

  /**
   *
   */
  private def intersects(f: LDP, g: G): Boolean = !intersection(f, g).isEmpty

  /**
   *  Alle Punkte, die der Verbindung im Wege liegen
   */
  private def intersection(f: LDP, g: G): LDP = f filter (p => g.intersects(p.ring))

  /**
   * Sucht optimale Verbindungsstelle zwischen zwei konvexen Polygonen
   */
  def connection(a: Geometry, b: Geometry): Geometry = {

    val (aRand: PCC, bRand: PCC) = randPoints(a, b) // time("randPoints: ",

    find((aRand._1, bRand._1), (aRand._2, bRand._2), foreign(a :: b :: Nil)) match {
      case (Some(a), Some(b)) => if (a touches b) a union b else List(a, b).maxBy(_.getArea)
      case (Some(a), None) => a
      case (None, Some(b)) => b
      case (None, None) => List(line(aRand._1, aRand._2), line(bRand._1, bRand._2)).minBy(_.getLength) //TODO wähle eine Linie aus
    }

  }

  def printGeo(list: LG) = {
    val p = dpoints filter (dp => list.exists(g => g.contains(dp.puffer)))
    debug(p.toString)
  }

  def printGeo(g: G) = {
    val p = dpoints filter (dp => g.contains(dp.puffer))
    debug(p.toString)
  }

  private def find(upper: PCC, lower: PCC, fo: LDP): Pair[Option[Geometry], Option[Geometry]] = {

    def div(c1: Coordinate, c2: Coordinate): Pair[PCC, PCC] = {
      val l = line(c1, c2)
      ((c1, l.getCentroid.getCoordinate), (l.getCentroid.getCoordinate, c2))
    }
    /*debug("upper: %s %s".format(upper._1, upper._2))
 debug("lower: %s %s".format(lower._1, lower._2))*/

    val area = tetragon(upper._1, upper._2, lower._2, lower._1)
    val diff = fo.filter(e => area.intersects(e.puffer))
    val upperDist = upper._1.distance(upper._2)
    val lowerDist = lower._1.distance(lower._2)

    /*      debug("diff: %s ;  %s  &&  %s".format(diff.isEmpty, upperDist >= minRadius, lowerDist >= minRadius))*/

    if (!diff.isEmpty) {
      if (upperDist >= minRadius && lowerDist >= minRadius) {
        val (lu, ru) = div(upper._1, upper._2)
        val (ll, rl) = div(lower._1, lower._2)

        val left: Option[Geometry] = find(lu, ll, fo) match {
          case (Some(a), Some(b)) => Some(List(a, b).maxBy(_.getArea))
          case (Some(a), None) => Some(a)
          case (None, Some(b)) => Some(b)
          case (None, None) => None
        }

        val right: Option[Geometry] = find(ru, rl, fo) match {
          case (Some(a), Some(b)) => Some(List(a, b).maxBy(_.getArea))
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

  /**
   *
   */
  /* private def getBounds(a: Geometry, b: Geometry): Tuple4[Double, Double, Double, Double] = {
    val coords = (a.getCoordinates ++ b.getCoordinates)
    val sortByX = coords sortWith ((a, b) => a.x < b.x)
    val sortByY = coords sortWith ((a, b) => a.y < b.y)
    (sortByX.last.x, sortByX.head.x, sortByY.last.y, sortByY.head.y)
  }*/

  /**
   *
   */
  /* private def filter(maxX: Double, minX: Double, maxY: Double, minY: Double): LDP = {
    val boundArea = tetragon(new C(minX, minY), new C(minX, maxY), new C(maxX, maxY), new C(maxX, minY))
    dpoints.filter(p => boundArea.contains(p.puffer) || boundArea.intersects(p.puffer))
  }*/

  /**
   * Sucht nach allen umgebenen Punkten, die zwischen zwei konvexen Polygonen liegen
   */
  /*private def foreign(a: Geometry, b: Geometry): LDP = {
    val (maxX: Double, minX: Double, maxY: Double, minY: Double) = getBounds(a, b)
    val all = filter(maxX, minX, maxY, minY)
    val choosen = all filter (p => a.contains(p.geoPoint) || b.contains(p.geoPoint))
    all diff choosen
  }*/


  /**TODO unperformant */
  private def randPoints(a: Geometry, b: Geometry): Pair[PCC, PCC] = {

    /**
     * Lokalisiert die zugewandten Punkte zweier konvexen Polygone, um später eine
     * optimale Verknüpfungsstelle zu bestimmen.
     */
    def locate(a: Geometry, b: Geometry): Pair[PCC, PCC] = {

      /* Gib alle Koordinaten der Eckpunkte */
      val cs = a.union(b).convexHull.getCoordinates

      val acs = a.getCoordinates
      val bcs = b.getCoordinates

      /* Bilde daraus Kanten. */
      val edges = for (i <- 0 to cs.size - 2) yield ((cs(i), cs(i + 1)))

      /* Bestimme Verschmelzungspunkte der Polygone a und b. */
      val erg = edges.filter{
        e => (acs.contains(e._1) && bcs.contains(e._2)) ||
          (bcs.contains(e._1) && acs.contains(e._2))
      }

      /*
      * Konvexe Polygone dürfe nur an max. zwei Stellen verschmolzen werden, um wieder
      * eine konvexe Form zu bilden.
      */
      if (erg.size != 2) throw new RuntimeException("Fehler bei der Ermittlung von Verbindungstangenten der Polygone")

      // Randpunkte der jeweiligen Polygone zuordnen
      /*val ergc = erg flatMap (e => List(e._1, e._2))
   val aRand = ergc filter (c => a.touches(c))
   val bRand = ergc filter (c => b.touches(c))

   ((aRand.head.getCoordinate, aRand.last.getCoordinate), (bRand.head.getCoordinate, bRand.last.getCoordinate))*/
      val t1 = (erg.head._1, erg.head._2)
      val t2 = (erg.last._1, erg.last._2)
      (t1, t2)
    }

    /**
     *  Klappt die geordnete Liste der zugewandten Punkten um, falls diese
     * über Kreuz liegen.
     */
    def turn(a: PCC, b: PCC): Pair[PCC, PCC] = {

      val begin = line(a._1, b._1)
      val end = line(a._2, b._2)

      if (begin.intersects(end))
        (a, (b._2, b._1))
      else
        (a, b)
    }

    // gegenüberliegende Eckpunkte der Polygone a und b
    val (aRand, bRand) = locate(a, b)
    turn(aRand, bRand)
  }

  private def unionAll(set: List[Geometry]): Geometry = set match {
    case List(a) => a
    case List(a, b, _*) => unionAll(a.union(b) :: set.drop(2))
    case Nil => throw new RuntimeException("Diese Fehler sollte nie aufgerufen werden!" +
      "\nIrgendwie ist die Bereich verloren gegangen!")
  }

  private def foreign(list: LG, offset: Int = 0): LDP = {

    def env(list: LG, off: Int = 0): LDP = filter(bounds(list, off))

    def bounds(list: LG, off: Int = 0): G = {
      val coords = list flatMap (g => g.getCoordinates)
      val sX = coords sortWith ((a, b) => a.x < b.x)
      val sY = coords sortWith ((a, b) => a.y < b.y)
      val maxX = sX.last.x + off
      val minX = sX.head.x - off
      val maxY = sY.last.y + off
      val minY = sY.head.y - off
      //debug("bounds: maxX %s minX %s maxY %s minY %s".format(maxX, minX, maxY, minY))
      tetragon(new C(minX, minY), new C(minX, maxY), new C(maxX, maxY), new C(maxX, minY))
    }
    def filter(bounds: G): LDP = dpoints filter (p => bounds.intersects(p.ring))

    val all = env(list, offset)
    val choosen = all filter (p => list.exists(g => g.contains(p.puffer)))
    all diff choosen
  }


  def deinitialize = dpoints foreach {
    p => p.visited = false
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


/*
if (isTight) {
      val (aRand, bRand) = scale(a, b)
      connGeo = line(aRand._1, bRand._1)

    } else {

      val sorted = allDisturber.sortBy{
        p =>
          val i = conn.intersection(p.puffer)
          val coos = i.getCoordinates.sortBy(c => aCoo.distance(c))
          aCoo.distance(coos.head)
      }

      debug("all dist: ")
      allDisturber foreach (a => debug(a.toString))

      allDisturber.foreach{
        el =>
          val shPath = shortestPath(el.ring, connGeo)
          connGeo = connGeo.difference(el.puffer).union(a.intersection(el.ring)).union(shPath)
      }
    }
    connGeo*/


/*
     * Prüfe, ob auf der Strecke zwei Element so nahe bei einander liegen, dass
     * deren Abstand kleiner ist, als die Mindestbreite des Bereichs
     */
/*val isTight = allDisturber.exists{
  e1 => allDisturber.exists(e2 => e1 != e2 && e1.ring.intersects(e2.ring))
}*/