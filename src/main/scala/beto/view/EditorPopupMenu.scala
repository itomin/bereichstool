package beto.view


import _root_.beto.event.ActionControl
import javax.swing.{JMenuItem, JPopupMenu}
import java.awt.event.{MouseEvent, ActionEvent, ActionListener}
import edu.umd.cs.piccolo.event.PInputEvent
import java.awt.geom.Point2D

/**
 * Created by IntelliJ IDEA.
 * User: jtomin
 * Date: 29.06.11
 * Time: 20:36
 * To change this template use File | Settings | File Templates.
 */

class EditorPopupMenu(val aControl: ActionControl) extends JPopupMenu {

  private var p: Point2D = new Point2D.Float
  private val rangeGenerateMenuItem = new JMenuItem("Bereich erzeugen")

  rangeGenerateMenuItem.addActionListener(new ActionListener() {
    override def actionPerformed(a: ActionEvent) = {
      aControl.newRange(p.getX.toInt, p.getY.toInt)
    }
  })

  add(rangeGenerateMenuItem)

  def show(editor: GraphEditor, e: PInputEvent) = {
    p = e.getPosition
    super.show(editor, p.getX.toInt, p.getY.toInt)
  }

}
