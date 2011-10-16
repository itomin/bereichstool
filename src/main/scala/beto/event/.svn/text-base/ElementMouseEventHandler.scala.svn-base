package beto.event

import _root_.beto.beans.view._
import _root_.beto.beans._
import _root_.beto.view.GraphEditor
import edu.umd.cs.piccolo.event.{PBasicInputEventHandler, PInputEventFilter, PInputEvent}
import java.awt.Cursor
import java.awt.event.{KeyEvent, InputEvent, MouseEvent}


class ElementMouseEventHandler(val editor: GraphEditor, val graph: Network,
                               val selected: Selection)
  extends PBasicInputEventHandler {

  private lazy val filter = new PInputEventFilter

  filter.setOrMask(InputEvent.BUTTON1_MASK)
  setEventFilter(filter)


  /**
   * Methode wird aufgerufen, sobald die Maus einen Element betritt.
   * Als Reaktion darauf ändert das betroffene Element seine Eigenschaft,
   * um die Fokussierung zu signalisieren.
   *
   * @param e : ausgelöster Event, bringt das betroffene Element mit
   */
  override def mouseEntered(e: PInputEvent) = e.getButton match {
    case MouseEvent.NOBUTTON => e.getPickedNode match {
      case e: Element => e.focusEnter
      case _ => // Sonst gibts nichts
    }
    case _ => // Tue sonst nichts
  }

  /**
   * Methode wird aufgerufen, sobald die Maus einen Element verlässt.
   * Als Reaktion darauf wird die geänderte Eigenschaft des Elements
   * zurückgesetzt.
   *
   * @param e : ausgelöster Event, bringt das betroffene Element mit
   */
  override def mouseExited(e: PInputEvent) = e.getButton match {
    case MouseEvent.NOBUTTON => e.getPickedNode match {
      case e: Element => e.focusExit
      case _ => // Sonst gibts nichts
    }
    case _ => // Tue sonst nichts
  }


  /**
   * Methode wird aufgerufen, sobald ein Element angeklickt wird.
   * Das betroffene Element wird entsprechend aus- oder abgewählt
   * Das ausgewählte Element ändert dabei seine Ursprungsfarbe.
   * Das abgewählte Element nimmt seine Ursprungsfarbe wieder an.
   *
   * @param e : ausgelöster Event, bringt das betroffene Element mit
   */
  override def mouseClicked(e: PInputEvent) = e.isControlDown match {
    case true => toRange(e)
    case false => chooseElement(e)
  }

  /**
   *
   */
  def chooseElement(e: PInputEvent) = e.getButton match {
    case MouseEvent.BUTTON1 => e.getPickedNode match {
      case e: Element => selected.select(e)
      case _ => // Sonst gibts nichts
    }
    case MouseEvent.BUTTON3 => // Bereich im Bereich anlegen
  }

  /**
   *
   */
  def toRange(e: PInputEvent) = e.getButton match {
    case MouseEvent.BUTTON1 => e.getPickedNode match {
      case d: Range => if (selected.isEmpty) {
        println("Für diese Aktion müssen Sie zunächst einen Knoten oder ein Bereich auswählen")
      } else {
        graph.toRange(selected.getAll, d)
        selected.clear
        editor.repaint()
      }
      case _ => println("Sie sollten besser zielen! Elemente können nur Bereichen hinzugefügt werden")
    }
    case _ => // EventFilter verhindert, dass dieser Fall eintritt
  }
}