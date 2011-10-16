package beto.event

import _root_.beto.view.GraphEditor
import java.awt.event.{InputEvent}
import edu.umd.cs.piccolo.event.{PBasicInputEventHandler, PInputEventFilter, PInputEvent}

/**
 * Created by IntelliJ IDEA.
 * User: jtomin
 * Date: 30.06.11
 * Time: 20:34
 * To change this template use File | Settings | File Templates.
 */

class EditorPopupMenuHandler(val editor: GraphEditor) extends PBasicInputEventHandler {

  val filter = new PInputEventFilter
  filter.setOrMask(InputEvent.BUTTON3_MASK)
  setEventFilter(filter)

  private lazy val popupMenu = editor.getPopupMenu

  override def mouseClicked(e: PInputEvent) = {
    showPopup(e)
  }

  def showPopup(e: PInputEvent) = {
    popupMenu.show(editor, e)
  }
}