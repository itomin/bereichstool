/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package beto.handle
import _root_.beto.log.Logger
import _root_.beto.view.BeToFrame
import java.io.{FileNotFoundException, File}
import collection.mutable.HashMap
import _root_.beto.beans.view._



abstract class XMLParser[Bean](appl: BeToFrame) extends Logger {

  def fromXML(path: String): Bean

  def toXML(path: String, obj: Bean)

}


class ScenarioParser(appl: BeToFrame) extends XMLParser[Network](appl) {

  debug("ScenarioParser initialisiert")

  final val COMPRESSOR = "compressor"
  final val SHORTCUT = "shortcut"
  final val PIPE = "pipe"
  final val SUPPLY_NODE = "supply_node"
  final val NODE = "node"
  final val RESISTOR = "resistor"
  final val CONTROL_VALVE = "control_valve"
  final val VALVE = "valve"
  final val STORAGE = "storage"

  /**
   *
   */
  override def fromXML(path: String): Network = {
    debug("Parse " + path + " von XML")

    val file = new File(path)

    /* Dateieingabe verifizieren */
    if (!verify(file)) {
      throw new FileNotFoundException("Datei konnte nicht geöffnet werden!\n"
        + "Bitte überprüfen Sie Ihre Eingaben\n"
        + file.getAbsoluteFile)
    }

    /* XML parsen */
    parseFromXML(file.getAbsolutePath)

  }

  protected def verify(file: File): Boolean = file.exists


  /**
   *
   */
  private def parseFromXML(file: String): Network = {

    val nodes = HashMap[String, Element]()
    val edges = HashMap[Array[Element], SEdge]()
    var name = ""

    val root = xml.XML.loadFile(file)

    /* Name des Netzes */
    root match {
      case <simone>{network}</simone> => name = (network \ "@name").text
    }

    /* Knoten parsen */
    root match {
      case <simone><network>{elemList@_*}</network></simone> =>
        elemList.filter{
          n => n.label == SUPPLY_NODE || n.label == NODE
        }.foreach{  n =>

            val name = (n \ "@name").text
            val p = new Position((n \ "@X").text.toInt,
              (n \ "@Y").text.toInt)

            nodes.put(name, new SPoint(name, p))

        }
      case _ => throw new Exception("XML entspricht nicht dem geforderten Format!\n") //TODO siehe Beispiel
    }

    root match {
      case <simone><network>{elemList@_*}</network></simone> =>
        elemList.foreach{
          n =>

            if (n.label == SHORTCUT || n.label == PIPE
              || n.label == COMPRESSOR || n.label == RESISTOR
              || n.label == CONTROL_VALVE || n.label == VALVE
              || n.label == STORAGE) {

              val name = (n \ "@name").text
              val beginNode = get((n \ "begin_node").text)
              val endNode = get((n \ "end_node").text)

              val e = n.label match {

                case SHORTCUT => new Shortcut(name, beginNode, endNode)

                case PIPE => new Pipe(name, beginNode, endNode)

                case COMPRESSOR => new Compressor(name, beginNode, endNode)

                case RESISTOR => new Resistor(name, beginNode, endNode)

                case CONTROL_VALVE => new ControlValve(name, beginNode, endNode)

                case VALVE => new Valve(name, beginNode, endNode)

                case STORAGE => new Storage(name, beginNode, endNode)

                case _ => throw new Exception("XML-Element: " + n.label + "unbekannt\n") //TODO siehe Beispiel
              }
              edges.put(Array(beginNode, endNode), e)
            }
        }
      case _ => throw new Exception("XML entspricht nicht dem geforderten Format!\n")
    }

    def get(n: String): Element = {

      nodes.get(n) match {
        case Some(s) => s
        case None => throw new IllegalArgumentException(
          "Knoten " + n + " ist in der Tabelle nicht vorhanden")

      }
    }

    new Network(appl, name, nodes, edges)
  }


  def toXML(path: String, obj: Network) = println("Not supported yet")

}

// end of XMLHandle



