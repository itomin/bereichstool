package main.scala.beto.beans.view

import com.vividsolutions.jts.geom.{Geometry, Coordinate}

/**
 * Created by IntelliJ IDEA.
 * User: jtomin
 * Date: 16.09.11
 * Time: 13:19
 * To change this template use File | Settings | File Templates.
 */

/*
trait Hull {

  protected var coordinates = List[Coordinate]()
  protected var sinked = false

  def add(c: Coordinate) = {
    coordinates ::= if (!sinked) {
      sink(c)
    } else
      c
  }

  def sink
}

class LowerHull extends Hull {

}

class UpperHull extends Hull {

  def sink(c: Coordinate) = {
    new Coordinate(c.x, (c.y - coordinates.head.y) / 2 + coordinates.head.y)
  }


}

}*/
