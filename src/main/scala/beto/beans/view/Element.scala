/*
  * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package beto.beans.view

import _root_.beto.beans.{DRange, DElement}
import edu.umd.cs.piccolo.util.PPaintContext
import edu.umd.cs.piccolo.nodes.{PPath, PText}
import java.awt.geom.{Area, Rectangle2D, Ellipse2D}
import java.awt._
import scala.math
import com.vividsolutions.jts.geom.{GeometryFactory, Coordinate, Geometry}
import java.util.Random


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

  def updateBounds

  override def toString: String = name + " (" + p.scaledX + ", " + p.scaledY + ")"
}


/**
 *
 */
case class Range(override val name: String,
                 override val p: Position) extends Element(name, p) {


  val rnd = new Random

  val r = rnd.nextInt(255)
  val g = rnd.nextInt(255)
  val b = rnd.nextInt(255)
  println("%s %s  %s".format(r, g, b))
  /*val r = 255f / math.abs(rnd.nextFloat + 255f)
  val g = 255f / math.abs(rnd.nextFloat + 255f)
  val b = 255f / math.abs(rnd.nextFloat + 255f)*/


  protected override lazy val colorUnselected = new Color(r, g, b, 255)
  protected override lazy val colorSelected = new Color(r, g, b, 77)
  protected var form: Shape = new Ellipse2D.Double

  setPaint(colorUnselected)
  setBounds(p.x, p.y, 20, 20)

  override def updateBounds = {
    /*nothing todo */
  }

  override def paint(aPaintContext: PPaintContext) = {
    val g2 = aPaintContext.getGraphics
    g2.setPaint(getPaint)
    g2.draw(form)
    if (isSelected) g2.fill(form)
    g2.setStroke(new BasicStroke(1))
  }

  def update(f: Shape) = {
    form = f
    setBounds(f.getBounds2D.getX, f.getBounds2D.getY, f.getBounds2D.getWidth, f.getBounds2D.getHeight)
  }


}


/**
 *
 */
case class SPoint(override val name: String,
                  override val p: Position) extends Element(name, p) {

  protected var form: Shape = new Ellipse2D.Double
  setBounds(p.scaledX, p.scaledY, 5, 5)
  setPaint(colorUnselected)

  def updateBounds = setBounds(p.scaledX - 2, p.scaledY - 2, 5, 5)
}

