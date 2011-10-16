/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package beto.beans


class History extends Serializable{
      
      var list : List[String] = List()
      private val max = 5
      
      def add(path : String) =  { 
            if (!list.contains(path)) {
                  list =  path :: list 
            }
      }

      def remove(path : String) = {
            val (start, end) = list.splitAt(list.indexOf(path))
            list = start ::: end.tail
      }
      
      def size = list.size
      
      def each(iter: String => Unit) =  list.foreach(s => iter(s))
      
}
