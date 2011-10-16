package beto.beans

import view.Element


/**
 * Created by IntelliJ IDEA.
 * User: jtomin
 * Date: 28.06.11
 * Time: 17:16
 * To change this template use File | Settings | File Templates.
 */

class Selection {

  private var selected = Seq[Element]()

  def select(e: Element): Unit = e.isSelected match {
    case true => {
      e.deselect
      remove(e)
    }
    case false => {
      e.select
      put(e)
    }
  }

  def select(e: Seq[Element]): Unit = e.foreach{
    n => select(n)
  }

  def getAll: Seq[Element] = selected

  def clear = {
    selected.foreach(_.deselect)
    selected = Seq[Element]()
  }

  def isEmpty = selected.isEmpty

  private def remove(e: Element) = {
    val (a, b) = selected.splitAt(selected.indexOf(e))
    selected = a ++: b.tail
  }

  private def put(e: Element) = selected +:= e

  private def exists(e: Element): Boolean = selected.contains(e)


  def print = println(selected)
}