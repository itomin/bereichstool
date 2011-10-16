/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package beto.view

import _root_.beto.beans.Selection
import _root_.beto.beans.view._
import _root_.beto.event._
import _root_.beto.log.Logger
import edu.umd.cs.piccolo.{PCanvas, PLayer}
import java.awt.Cursor

class GraphEditor(val aControl: ActionControl) extends PCanvas with Logger {

  debug("GraphEditor initialisiert")

  /* Knotenebene enthält nur die Range und die Bereiche */
  private lazy val knotenEbene = new PLayer

  /* kantenEbene enthält nur die Kanten */
  private lazy val kantenEbene = new PLayer

  private lazy val rangeLayer = getLayer

  private lazy val selection = new Selection

  private lazy val popupMenu = new EditorPopupMenu(aControl)

  private lazy val popupMenuHandler = new EditorPopupMenuHandler(this)

  private lazy val cursorAllowed = new Cursor(Cursor.CROSSHAIR_CURSOR)

  private var dragHandler: EditorDragEventHandler = _
  private var elementHandler: ElementMouseEventHandler = _
  private var graph: Network = _

  /*
  * Kantenebene wird entkoppelt von der Knotenebene verwaltet
  * um eine saubere Darstellung zu ermöglichen. Ausserdem sollen
  * die Knoten über den Kanten liegen.
  */
  getRoot.addChild(kantenEbene)
  getRoot.addChild(knotenEbene)
  getCamera.addLayer(kantenEbene)
  getCamera.addLayer(knotenEbene)

  add(popupMenu)
  addInputEventListener(popupMenuHandler)
  removeInputEventListener(getPanEventHandler)
  removeInputEventListener(getZoomEventHandler)


  /*
    setPanEventHandler(new EditorPanEventHandler)           //TODO Maus kaufen
   setZoomEventHandler(new EditorZoomEventHandler)
  */

  /**
   *
   */
  def drawGraph(graph: Network) = {
    /****************************************************************************

                              Range zeichnen

     ****************************************************************************/
    debug("Knoten zeichnen")

    dragHandler = new EditorDragEventHandler(this, graph, selection)
    elementHandler = new ElementMouseEventHandler(this, graph, selection)
    this.graph = graph

    graph.eachNode{
      n =>
      /* Position setzen */
        n.setBounds(n.p.scaledX, n.p.scaledY, 5, 5)

        /* Element der Knotenebene zuweisen */
        knotenEbene.addChild(n)

        /* ToolTip des Elements der Kamera zuweisen */
        knotenEbene.addChild(n.tooltip)
    }

    knotenEbene.addInputEventListener(elementHandler)
    rangeLayer.addInputEventListener(elementHandler)
    getCamera.addInputEventListener(dragHandler)
  }

  def getPopupMenu = popupMenu

  def addElement(e: Element) = {
    rangeLayer.addChild(e)
    rangeLayer.addChild(e.tooltip)
  }
}

