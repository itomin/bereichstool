package beto.beans

import marching.Cell
import collection.immutable.HashMap
import com.vividsolutions.jts.geom.{Coordinate, MultiLineString, Geometry}

/**
 * Created by IntelliJ IDEA.
 * User: jtomin
 * Date: 01.12.11
 * Time: 17:56
 * To change this template use File | Settings | File Templates.
 */

object Contouring {

  import DGeometry._


  /**
   *
   */
  def contour(cells: List[Cell]): Geometry = {
    //println("CONTOUR")
    val nonEmpty = cells.filter(c => !c.isEmpty).toSet.toSeq
    val coosMap = HashMap(nonEmpty.map(c => c.get): _*)
    //coosMap.foreach(p => println("%s  -  %s".format(p._1, p._2)))
    //println("---------------")
    val g = contourRec(coosMap, coosMap.head._1 :: Nil)
    //val g = join(nonEmpty.map(c => c.get).toList, emptyGeometry)
    //println("CONTOUR DONE")
    g
  }

  private def join(cells: List[Pair[Coordinate,Coordinate]],
                   geo: Geometry): Geometry = cells match {

    case List(a) => line(a._1, a._2)
    case (List(_, _*)) => line(cells.head._1, cells.head._2).union(join(cells.tail, geo))
  }


  /**
   *
   */
  private def contourRec(cells: HashMap[Coordinate, Coordinate],
                 coos: List[Coordinate]): Geometry = {

    val fromStart = coos.last
    val current = coos.head
    //println("Current: %s".format(current))
    cells.get(current) match {
      case Some(to) => {
        if (to == fromStart)
          polygon(current :: coos)
        else {
          contourRec(cells, to :: coos)
        }
      }
      case None => throw new RuntimeException("Linienzug konnte nicht geschlossen werden!") // polygon(current :: coos)
    }
  }
}