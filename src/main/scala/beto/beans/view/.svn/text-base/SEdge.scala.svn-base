package beto.beans.view

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
                    val dia : Float = 1)  {
}


/**
 *
 */
case class Compressor(override val name: String,
                      override val beginNode: Element,
                      override val endNode: Element,
                      override val dia: Float = 1) extends SEdge(name, beginNode, endNode, dia){}


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


