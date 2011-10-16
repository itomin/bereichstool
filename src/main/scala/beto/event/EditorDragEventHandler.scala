package beto.event

/**
 * Created by IntelliJ IDEA.
 * User: jtomin
 * Date: 27.06.11
 * Time: 22:21
 * To change this template use File | Settings | File Templates.
 */


import _root_.beto.beans.Selection
import _root_.beto.beans.view.{Element, Network}
import _root_.beto.view.GraphEditor
import edu.umd.cs.piccolo.nodes.PPath
import edu.umd.cs.piccolo.util.PBounds
import java.awt.geom.{Rectangle2D, Point2D}
import edu.umd.cs.piccolo.event.{PInputEventFilter, PBasicInputEventHandler, PInputEvent}
import java.awt.event.InputEvent
import java.awt.{Cursor, Color, BasicStroke}

class EditorDragEventHandler(val editor: GraphEditor, val graph: Network, val selected: Selection)
  extends PBasicInputEventHandler {

  private lazy val filter = new PInputEventFilter
  private lazy val layer = editor.getLayer
  private lazy val cursorElementIn = new Cursor(Cursor.CROSSHAIR_CURSOR)
  private lazy val cursorDefault = new Cursor(Cursor.DEFAULT_CURSOR)

  var rectangle: PPath = _
  var pressPoint: Point2D = _
  var dragPoint: Point2D = _

  filter.setOrMask(InputEvent.BUTTON1_MASK | InputEvent.CTRL_MASK)
  setEventFilter(filter)


  override def mouseMoved(e: PInputEvent) = {
    e.isControlDown match {
      case true => editor.setCursor(cursorElementIn)
      case false => editor.setCursor(cursorDefault)
    }
  }

  override def mousePressed(e: PInputEvent) = {

    if (!e.isControlDown) {
      val layer = e.getComponent.asInstanceOf[GraphEditor].getLayer

      pressPoint = e.getPosition();
      dragPoint = pressPoint

      rectangle = new PPath

      val stroke = new BasicStroke((1 / e.getCamera().getViewScale).toFloat,
        BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER, 10.0f, Array[Float](10.0f), 0.0f)

      rectangle.setStrokePaint(Color.cyan)
      rectangle.setStroke(stroke)

      layer.addChild(rectangle)
      updateRectangle
    } else {
      layer.removeChild(rectangle)
    }

  }

  override def mouseDragged(e: PInputEvent) = {
    if (!e.isControlDown) {
      dragPoint = e.getPosition
      updateRectangle
    } else {
      layer.removeChild(rectangle)
    }
  }

  override def mouseReleased(e: PInputEvent) = {
    if (!e.isControlDown) {
      dragPoint = e.getPosition
      updateRectangle
      selected.select(getPickedNodes)
      layer.removeChild(rectangle)
      editor.repaint()
    }
  }

  def updateRectangle() = {
    val b = new PBounds
    b.add(pressPoint)
    b.add(dragPoint)
    rectangle.setPathTo(b)
  }

  def getPickedNodes: Seq[Element] = {
    graph.allPoints.filter{
      n => n.intersects(new Rectangle2D.Double(rectangle.getX,
        rectangle.getY, rectangle.getWidth, rectangle.getHeight))
    }.toSeq
  }

}