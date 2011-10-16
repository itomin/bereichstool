/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package beto.handle

import _root_.beto.beans.History
import _root_.beto.beans.view.Network
import java.io._
import _root_.beto.log.Logger
import _root_.beto.view.BeToFrame


abstract class IOHandle[Bean](appl: BeToFrame) extends Logger {
      
      def load(path: String): Bean
      
      def save(obj: Bean, path: String)
      
}


class ScenarioHandle(appl: BeToFrame) extends IOHandle[Network](appl) {
      
      lazy val parser = new ScenarioParser(appl)
      
      override def load(path: String): Network = parser.fromXML(path)
      
      override def save(obj: Network, path: String) = parser.toXML(path, obj)

}

class HistoryHandle(appl: BeToFrame) extends IOHandle[History](appl) {
      
      
      protected lazy val home = System.getProperty("user.home") + "/history.beto"
      
      
      def fileExists(str: String): Boolean = {
            val file = new File(str)
            file.exists
      }
      
      override def load(path: String = home): History = {
            if(fileExists(path)){   
                  reader(path) { r =>
                        val any = r.readObject
                        r.close
                        any.asInstanceOf[History]   
                  }
                  
            } else {
                  new History
            }
      }
     
      override def save(obj: History, path: String = home) = {
            writer(path) { 
                  w =>  w.writeObject(obj)
                  w.close
            }
      }
      
      def writer(path: String)(write: ObjectOutputStream => Unit) = {
            write(new ObjectOutputStream(new FileOutputStream(path)))
      }
      
      def reader(path: String)(read: ObjectInputStream => History) = {
            read(new ObjectInputStream(new FileInputStream(path)))
      } 
      
}
           



