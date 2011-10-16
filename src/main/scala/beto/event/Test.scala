package beto.event

import _root_.beto.view.GraphEditor
import edu.umd.cs.piccolo.event.{PBasicInputEventHandler, PInputEvent}
import java.awt.event.{MouseEvent, MouseAdapter}

/**
 * Created by IntelliJ IDEA.
 * User: jtomin
 * Date: 01.07.11
 * Time: 15:15
 * To change this template use File | Settings | File Templates.
 */


class PP(val editor: GraphEditor) extends MouseAdapter {

  private lazy val popupMenu = editor.getPopupMenu

  override def mousePressed(e: MouseEvent) = {
    showPopup(e)
  }

    override def mouseReleased(e: MouseEvent) = {
    showPopup(e)
  }

  def showPopup(e: MouseEvent) = {
    if (e.isPopupTrigger) {
      popupMenu.show(editor, e.getX, e.getY)
    }
  }
}