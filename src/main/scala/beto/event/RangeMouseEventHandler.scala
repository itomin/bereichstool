package beto.event

import java.awt.Cursor
import edu.umd.cs.piccolo.event.{PInputEvent, PBasicInputEventHandler}

/**
 * Created by IntelliJ IDEA.
 * User: jtomin
 * Date: 05.07.11
 * Time: 23:45
 * To change this template use File | Settings | File Templates.
 */

class RangeMouseEventHandler extends PBasicInputEventHandler {
 /* val cursorElementIn = new Cursor(Cursor.CROSSHAIR_CURSOR)

  override def mouseEntered(e: PInputEvent) {
    super.mouseEntered(e)
    e.getButton match {

      case _ => println("Entered: " + e.getButton)
    }
  }

  override def mouseExited(e: PInputEvent) {
    super.mouseExited(e)
    e.getButton match {
      case MouseEvent.NOBUTTON => e.getPickedNode match {
        case e: Element => e.focusExit
        case _ => // Sonst gibts nichts
      }
      case _ => println("Exited: " + e.getButton)
    }

  }*/
}