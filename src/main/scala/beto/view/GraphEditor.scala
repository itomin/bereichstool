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
import edu.umd.cs.piccolo.nodes.PPath
import java.awt.{Color, BasicStroke}

class GraphEditor(val aControl: ActionControl) extends PCanvas with Logger {


  /* Knotenebene enthält nur die Range und die Bereiche */
  private lazy val knotenEbene = new PLayer

  /* kantenEbene enthält nur die Kanten */
  private lazy val kantenEbene = getLayer

  private lazy val rangeLayer = new PLayer

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
  getRoot.addChild(rangeLayer)
  getRoot.addChild(knotenEbene)
  getCamera.addLayer(1, knotenEbene)
  getCamera.addLayer(0, rangeLayer)
  //knotenEbene.addChild(rangeLayer)
  //knotenEbene.addChild(rangeLayer)

  add(popupMenu)
  addInputEventListener(popupMenuHandler)

  removeInputEventListener(getPanEventHandler)
  // removeInputEventListener(getZoomEventHandler)


  def clear = {
    kantenEbene.removeAllChildren
    knotenEbene.removeAllChildren
  }

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
    debug("zeichne den graphen...")
    dragHandler = new EditorDragEventHandler(this, graph, selection)
    elementHandler = new ElementMouseEventHandler(this, graph, selection)
    this.graph = graph
    val tempColor = new Color(200, 200, 200)

    graph.eachEdge{
      e =>
        val edge = PPath.createLine(e.beginNode.p.scaledX, e.beginNode.p.scaledY, e.endNode.p.scaledX, e.endNode.p.scaledY)
        edge.setStroke(new BasicStroke(1.5f))
        edge.setPaint(tempColor)
        edge.setStrokePaint(tempColor)
        kantenEbene.addChild(edge)
    }


    graph.eachNode{
      n =>
      /* Element der Knotenebene zuweisen */
        knotenEbene.addChild(n)

        /* ToolTip des Elements der Kamera zuweisen */
        //knotenEbene.addChild(n.tooltip)
    }

    knotenEbene.addInputEventListener(elementHandler)
    rangeLayer.addInputEventListener(elementHandler)
    getCamera.addInputEventListener(dragHandler)
    debug("zeichnen abgeschlossen")
  }

  def getPopupMenu = popupMenu

  def addElement(e: Element) = {
    rangeLayer.addChild(e)
   // rangeLayer.addChild(e.tooltip)
  }


}

