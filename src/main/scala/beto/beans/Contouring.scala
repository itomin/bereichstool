package beto.beans

import _root_.beto.log.Logger
import marching.Cell
import collection.mutable.HashMap
import com.vividsolutions.jts.geom.{LinearRing, Coordinate, MultiLineString, Geometry}

/**
 * Created by IntelliJ IDEA.
 * User: jtomin
 * Date: 01.12.11
 * Time: 17:56
 * To change this template use File | Settings | File Templates.
 */

object Contouring extends Logger {

  import DGeometry._


  /**
   *
   */
  def contour(cells: Set[Cell]): Pair[LinearRing, Array[LinearRing]] = {
    val nonEmpty = cells.filter(c => !c.isEmpty).toSeq
    val coosMap = HashMap(nonEmpty.flatMap(c => c.get): _*)
    contourRec(coosMap, coosMap.head._1 :: Nil, Nil)
    //join(nonEmpty.flatMap(c => c.get).toList, emptyGeometry)
  }

  private def join(cells: List[Pair[Coordinate, Coordinate]],
                   geo: Geometry): Geometry = cells match {
    case List(a) => line(a._1, a._2)
    case (List(_, _*)) => line(cells.head._1, cells.head._2).union(join(cells.tail, geo))
  }


  /**
   *
   */
  private def contourRec(cells: HashMap[Coordinate, Coordinate],
                         coos: List[Coordinate], geo: List[List[Coordinate]]): Pair[LinearRing, Array[LinearRing]] = {

    var fromStart = coos.last
    val current = coos.head
    //println("Current: %s".format(current))
    /*println("Rest: %s".format(cells.size))
    println("fromStart: %s   current: %s".format(fromStart, current))*/
    cells.get(current) match {
      case Some(to) => {
        //println("to: %s".format(to))
        if (to == fromStart) {
          val g = spline(to :: current :: coos)
          val rest = cells.filterNot(c => c._1 == current)
          //polygon(current :: coos)
          if (rest.isEmpty) {
            createForm(g :: geo)
          } else {
            contourRec(rest, rest.head._1 :: Nil, g :: geo)
          }
        } else {
          contourRec(cells.filterNot(c => c._1 == current), to :: coos, geo)
        }
      }
      case None => throw new RuntimeException("Linienzug konnte nicht geschlossen werden!") // polygon(current :: coos)
    }
  }

  private def createForm(geos: List[List[Coordinate]]): Pair[LinearRing, Array[LinearRing]] = {
    val geosArr: Array[LinearRing] = geos.filter(list => list.size >= 4).map{
      list =>
        geomfact.createLinearRing(list.toArray)
    }.toArray

    val shell = geosArr.maxBy(geo => geo.getLength)
    val holes = geosArr.diff(shell :: Nil)
    //(shell, holes)
    (shell, Array[LinearRing]())
  }

  private def spline(list: List[Coordinate]): List[Coordinate] = {
    var index = 0

    val splined = list.flatMap{
      el =>
        index match {
          case 2 => index = 0;
          el :: Nil
          case _ => index += 1;
          Nil
        }
    }
    splined.last :: splined
  }

}