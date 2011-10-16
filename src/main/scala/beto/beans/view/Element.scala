/*
  * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package beto.beans.view

import _root_.beto.beans.{DRange, DElement, DPoint}
import edu.umd.cs.piccolo.util.PPaintContext
import edu.umd.cs.piccolo.nodes.{PPath, PText}
import java.awt.geom.{Area, Rectangle2D, Ellipse2D}
import java.awt._
import scala.math
import com.vividsolutions.jts.geom.{GeometryFactory, Coordinate, Geometry}

object Element {

  val geomfact = new GeometryFactory

  def toCircle(x: Int, y: Int, radius: Int): Geometry = {

    val sides = 32

    var coords = (0 to sides).map{
      i =>
        val angle = (i.toDouble / sides.toDouble) * math.Pi * 2.0
        val dx = math.cos(angle) * radius
        val dy = math.sin(angle) * radius
        new Coordinate(x + dx, y + dy)
    }

    coords = coords :+ coords.head

    /*
        coords.foreach(println(_))

        println("----------------------")
    */
    geomfact.createPolygon(geomfact.createLinearRing(coords.toArray), null)
  }
}

abstract class Element(val name: String,
                       val p: Position) extends Drawable {

  import Element._

  val dElement: DElement

  override def toString: String = name + " (" + p.scaledX + ", " + p.scaledY + ")"
}


/**
 *
 */
case class Range(override val name: String,
                 override val p: Position) extends Element(name, p) {

  protected override lazy val colorUnselected = new Color(176f / 255f, 43f / 255f, 44f / 255f, 0.3f)
  protected override lazy val colorSelected = new Color(176f / 255f, 43f / 255f, 44f / 255f, 0.6f)
  override val dElement = new DRange(this)
  var painter = (g2: Graphics2D) => {
    g2.setPaint(getPaint)
    g2.draw(form)
    g2.setStroke(new BasicStroke(2))
  }

  setPaint(colorUnselected)
  setBounds(p.x, p.y, 20, 20)

  override def paint(aPaintContext: PPaintContext) = {
    painter(aPaintContext.getGraphics)
  }

  def deform(form: Shape) = {
    painter = (g2: Graphics2D) => {
      g2.setPaint(getPaint)
      g2.setStroke(new BasicStroke(2))
      g2.draw(form)
    }
  }


}


/**
 *
 */
case class SPoint(override val name: String,
                  override val p: Position) extends Element(name, p) {
  override val dElement = new DPoint(this)
  setPaint(colorUnselected)
}

