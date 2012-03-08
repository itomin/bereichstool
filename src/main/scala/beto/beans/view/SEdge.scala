package beto.beans.view

import edu.umd.cs.piccolo.nodes.PPath
import edu.umd.cs.piccolo.util.PPaintContext
import java.awt.geom.{Ellipse2D, Rectangle2D}

/**
 * Created by IntelliJ IDEA.
 * User: jtomin
 * Date: 23.06.11
 * Time: 13:44
 * To change this template use File | Settings | File Templates.
 */


/**
 *
 */
abstract class SEdge(val name: String,
                     val beginNode: Element,
                     val endNode: Element,
                     val dia: Float = 1) extends PPath {


 /* override def intersects(other: Rectangle2D): Boolean = {
    println("inter")
    false
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
   println("bounds")
    if (super.setBounds(x, y, w, h)) {
      true
    } else {
      false
    }
  }


  override def paint(aPaintContext: PPaintContext) = {
    println("paint edge")
    moveTo(beginNode.p.scaledX, beginNode.p.scaledY)
    lineTo(endNode.p.scaledX, endNode.p.scaledY)
    setPaint(new Color(50, 50, 50))
    setStroke(new BasicStroke(2))
  }*/
}


/**
 *
 */
case class Compressor(override val name: String,
                      override val beginNode: Element,
                      override val endNode: Element,
                      override val dia: Float = 1) extends SEdge(name, beginNode, endNode, dia) {}


/**
 *
 */
case class Valve(override val name: String,
                 override val beginNode: Element,
                 override val endNode: Element,
                 override val dia: Float = 1) extends SEdge(name, beginNode, endNode, dia)


/**
 *
 */
case class Resistor(override val name: String,
                    override val beginNode: Element,
                    override val endNode: Element,
                    override val dia: Float = 1)
  extends SEdge(name, beginNode, endNode, dia)


/**
 *
 */
case class ControlValve(override val name: String,
                        override val beginNode: Element,
                        override val endNode: Element,
                        override val dia: Float = 1)
  extends SEdge(name, beginNode, endNode, dia)


/**
 *
 */
case class Shortcut(override val name: String,
                    override val beginNode: Element,
                    override val endNode: Element)
  extends SEdge(name, beginNode, endNode)


/**
 *
 */
case class Storage(override val name: String,
                   override val beginNode: Element,
                   override val endNode: Element)
  extends SEdge(name, beginNode, endNode)


/**
 *
 */
case class Pipe(override val name: String,
                override val beginNode: Element,
                override val endNode: Element,
                override val dia: Float = 1)
  extends SEdge(name, beginNode, endNode, dia)


