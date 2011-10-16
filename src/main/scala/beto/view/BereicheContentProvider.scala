/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package beto.view

import org.eclipse.swt.graphics.Image
import org.eclipse.jface.viewers.{ILabelProviderListener, Viewer, ILabelProvider, IStructuredContentProvider}

class BereicheContentProvider extends IStructuredContentProvider {


  /**
   * Gets the files in the specified directory
   *
   * @param arg0
   *            a String containing the directory
   */
  override def getElements(arg0 : Object) : Array[Object] = {
    arg0.asInstanceOf[Array[Object]]
  }

  def dispose = {   }

  def inputChanged(arg0 : Viewer, arg1 : Object, arg2 : Object) { }

}


class MyLabelProvider extends ILabelProvider {

  def getImage(arg0 : Object) : Image = { null }

  def getText(arg0 : Object) : String = {
    "" + arg0
  }

  def addListener(arg0 : ILabelProviderListener) {
  }

  def dispose() {
  }

  def isLabelProperty(arg0 : Object, arg1 : String) :Boolean = {
    return false;
  }

  def removeListener(arg0 : ILabelProviderListener) = {
    
  }
}