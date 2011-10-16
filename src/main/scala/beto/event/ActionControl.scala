/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package beto.event

import _root_.beto.beans.History
import _root_.beto.beans.view.Network
import _root_.beto.log.Logger
import _root_.beto.view.BeToFrame


class ActionControl(appl: BeToFrame) extends Logger {
  debug("ActionControl initialisiert")

  /************************************************************************
   *
   *                Hier werden alle Subcontrols definiert
   *
   ************************************************************************/
  private val ioControl = new IO(appl)
  private var rangeControl: RangeControl = _


  /**
   * Erzeugt ein neues Szenario für die Bereichseinteilung
   */
  def newScenario(path: String): Network = {
    debug("Öffne " + path)
    val g = ioControl.loadGraph(path)
    rangeControl = new RangeControl(g)
    g
  }

  /**
   *
   */
  def loadHistory: History = {
    debug("Lade History")
    ioControl.loadHistory
  }

  /**
   *
   */
  def saveHistory(hi: History) = {
    debug("Speicher History")
    ioControl.saveHistory(hi)
  }


  /**
   * Sichert die aktuelle Bereichseinteilung als XML
   */
  def saveGraph = {
    println("Don't supported yet")
  }


  /**
   * Öffnet eine bestehende Bereichseinteilung zur Weiterbearbeitung
   */
  def openGraph = {
    println("Don't supported yet")
  }

  /**
   *
   */
  def exportGraph = {
    println("Don't supported yet")
  }

  /**
   *
   */
  def undo = {
    println("Don't supported yet")
  }

  /**
   *
   */
  def redo = {
    println("Don't supported yet")
  }

  /**
   *
   */
  def resolveRange = {
    println("Don't supported yet")
  }

  /**
   *
   */
  def newRange = {
    println("Don't supported yet")
  }

  /**
   *
   */
  def newRange(x: Int, y: Int) = {
    rangeControl.newRange(x, y)
  }


  /**
   *
   */
  def moveElement = {
    println("Don't supported yet")
  }

  /**
   *
   */
  def chooseColor = {
    println("Don't supported yet")
  }


}
