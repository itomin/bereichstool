package beto

import org.eclipse.swt.widgets._

import view.BeToFrame

object Main {

  def main(args: Array[String]): Unit = {
    val display = Display.getCurrent
    val window = new BeToFrame(display)
    window.setBlockOnOpen(true)
    window.open
  }

 /* def main(args: Array[String]): Unit = {

    5 match {
      case 1 => println("1")
      case 2 => println("2")
      case x if x > 2 => println(x)
      case _ => println("else")
    }

  }*/

}

