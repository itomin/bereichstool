package beto.view

import org.eclipse.jface.action.Action
import org.eclipse.jface.resource.ImageDescriptor
import java.io.File

/**
 * Created by IntelliJ IDEA.
 * User: jtomin
 * Date: 03.07.11
 * Time: 19:01
 * To change this template use File | Settings | File Templates.
 */

class BeToAction(val name: String, val imgPath: String, exec: => Unit) extends Action(name) {

  val imgFile = new File(imgPath)

  if (imgFile.exists) {
    setImageDescriptor(ImageDescriptor.createFromFile(null, imgFile.getAbsolutePath))
  }

  override def run = exec
}