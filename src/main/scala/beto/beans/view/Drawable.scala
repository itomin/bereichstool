package beto.beans.view

import edu.umd.cs.piccolo.nodes.{PPath, PText}
import edu.umd.cs.piccolo.util.PPaintContext
import java.awt.geom.{Ellipse2D, Rectangle2D}
import java.awt.{Shape, BasicStroke, Color}
import com.vividsolutions.jts.awt.PolygonShape


/**
 * Created by IntelliJ IDEA.
 * User: jtomin
 * Date: 20.07.11
 * Time: 12:44
 * To change this template use File | Settings | File Templates.
 */

trait Drawable extends PPath {

  var isSelected = false
  val name: String
  val p: Position

  val tooltip = new PText("%s (%s, %s)".format(name, p.x, p.y))


  protected var form: Shape
  protected lazy val colorSelected = new Color(255, 116, 0)
  protected lazy val colorUnselected = new Color(64, 150, 238)

  tooltip.setPickable(false)
  tooltip.setVisible(false)


  /**
   * Methode wird aufgerufen, sobald  sich die Maus über dem Range
   * befindet. Sorgt dafür, dass der Name des Knotens als ToolTip
   * erscheint und vergrößert den Range um 2 Einheiten, damit klar ist
   * über welchen Range sich der Mauszeiger befindet
   *
   * @return Unit
   */
  def focusEnter: Unit = {
    showToolTip(true)
    tooltip.setOffset(getX + 8, getY - 8)
  }

  /**
   * Methode wird aufgerufen, sobald  sich die Maus den Range
   * verlässt. ToolTip wird dabei auf unsichtbar geschaltet und
   * der Range nimmt wieder die Ursprungsgröße an.
   *
   * @return Unit
   */
  def focusExit: Unit = showToolTip(false)

  def showToolTip(isVisible: Boolean) = tooltip.setVisible(isVisible)

  def select = {
    isSelected = true
    setPaint(colorSelected)
  }

  def deselect = {
    isSelected = false
    setPaint(colorUnselected)
  }

  override def intersects(other: Rectangle2D): Boolean = {
    form.intersects(other)
  }

  /**
   * Diese Methode legt die Position und die Dimension
   * des Knotens fest.
   *
   * @param x : x-Koordinate
   * @param y : y-Koordinate
   * @param w : Breite der Ellipse
   * @param h : Höhe der Ellipse
   *
   * @return Konnten die Parameter korrekt gesetzt werden ?
   *
   */
  override def setBounds(x: Double, y: Double, w: Double, h: Double): Boolean = {
    if (super.setBounds(x, y, w, h)) {
      form match {
        case e: Ellipse2D.Double => e.setFrame(x, y, w, h)
        case _ => //nichts zu tun
      }
      true
    } else {
      false
    }
  }


  /**
   * @param aPaintContext
   */
  override def paint(aPaintContext: PPaintContext) = {
    val g2 = aPaintContext.getGraphics
    g2.setPaint(getPaint)
    g2.fillOval(getX.toInt, getY.toInt, 5, 5)
    g2.setStroke(new BasicStroke(0))
  }
}