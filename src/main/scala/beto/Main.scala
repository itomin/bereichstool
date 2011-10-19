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

    val a: Array[String] = Array[String]("1", "2", "3", "4",  "5")

    for(i <- a; j <- a if j != i) println(i + "-" + j )
  }*/

}

