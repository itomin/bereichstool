package beto.event

import _root_.beto.beans.view.{Network}

/**
 * Created by IntelliJ IDEA.
 * User: jtomin
 * Date: 04.07.11
 * Time: 19:29
 * To change this template use File | Settings | File Templates.
 */

class RangeControl(val graph: Network) {

  def newRange(x: Int, y: Int) = graph.defaultRange(x, y)
}