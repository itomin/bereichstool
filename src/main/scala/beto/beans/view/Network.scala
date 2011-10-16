/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package beto.beans.view

import _root_.beto.beans.{DelaunayGraph}
import _root_.beto.log.Logger
import _root_.beto.view.BeToFrame
import collection.mutable.HashMap
import geometry.Point


object Network {

  /**
   *
   */
  implicit def comp[A](f: (A, A) => Int) = {
    new Ordering[A] {
      override def compare(a: A, b: A): Int = {
        f(a, b)
      }
    }
  }

  /**
   *
   */
  def fetch[A, B](hash: HashMap[A, B], n: A): B = {
    hash.get(n) match {
      case Some(s) => s
      case None => throw new IllegalArgumentException(
        "Element " + n + " ist in der Tabelle nicht vorhanden")
    }
  }

  /* Maximale X-Koordinate eines Punktes */
  def maxX(set: List[Element]): Int = set.max((a: Element, b: Element) => a.p.x compare b.p.x).p.x

  /* Maximale Y-Koordinate eines Punktes */
  def maxY(set: List[Element]): Int = set.max((a: Element, b: Element) => a.p.y compare b.p.y).p.y

  /* Minimale X-Koordinate eines Punktes */
  def minX(set: List[Element]): Int = set.min((a: Element, b: Element) => a.p.x compare b.p.x).p.x

  /* Minimale Y-Koordinate eines Punktes */
  def minY(set: List[Element]): Int = set.min((a: Element, b: Element) => a.p.y compare b.p.y).p.y

}

case class Network(val appl: BeToFrame,
                   val name: String,
                   val points: HashMap[String, Element],
                   val edges: HashMap[Array[Element], SEdge]) extends Logger {


  debug("Graph initialisiert")

  import Network._

  /*
  * Merkt sich die Anzahl neu erzeugter Bereiche,
  * um z.B die Default-Namen ( Unbekannt_1) zu vergeben
  */
  var rangeCount = 0

  lazy val w = appl.editorWidth
  lazy val h = appl.editorHeight


  val delaunGraph = new DelaunayGraph(allPoints.map(_.asInstanceOf[SPoint]), this)

  /*
   * Darstellung des Graphen. Bei Änderung der Daten
   * wird ein Update auf die Repräsentation der Daten durchgeführt.
   */
  lazy val graphEditor = appl.getGraphEditor

  /* Für den Fall, dass man durch alle Knoten iterieren muss */
  lazy val allPoints = points.values.toList

  /*
  * Koordinaten der Punkte müssen zunächst auf das gegebene Koordinatensystem
  * des Darstellungspanels skaliert werden
  */
  if (appl.editorWidth < maxX(allPoints)) {
    Position.scaleX = (a: Int) => (a - minX(allPoints)) * (appl.editorWidth - 0) / (maxX(allPoints) - minX(allPoints))
    Position.scaleY = (a: Int) => (a - minY(allPoints)) * (appl.editorHeight - 0) / (maxY(allPoints) - minY(allPoints))
  }

  /*
  * Für jeden Knoten nächsten Nachbarn bestimmen
  */
  /*  allPoints.foreach{
    fromEl =>
      val (a, _ :: b) = allPoints.splitAt(allPoints.indexOf(fromEl))
      val list: List[Element] = a ::: b
      val lPair: List[Pair[Double, Element]] = list.map(toEl => (fromEl.distance(toEl), toEl))
      val neighbour = lPair.min((a: Pair[Double, Element], b: Pair[Double, Element]) => a._1 compare b._1)._2
      fromEl.neighbour = neighbour
  }*/

  /****************************************************************************************************************
                                          Öffentliche Methoden
   ****************************************************************************************************************/

  /**
   * Iteriert durch jeden Knoten
   */
  def eachNode(f: Element => Unit) = allPoints.foreach(e => f(e))

  /**
   * Erzeugt einen neuen Bereich und ordnet ihn in den Bereichsbaum ein
   */
  def defaultRange(x: Int, y: Int) = {
    val range = newRange(new Position(x, y))
    graphEditor.addElement(range)
  }


  def toRange(el: Seq[Element], in: Range) = {

    delaunGraph.toRange(el.map(_.dElement), in.dElement)
  }


  /**
   *
   */
  private def nameIndex: Int = {
    rangeCount += 1
    rangeCount
  }

  /**
   *  Erzeugt ein neues Range-Objekt
   */
  private def newRange(p: Position) = new Range(("Unbekannt_%s").format(rangeCount), p)

}



