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


}

