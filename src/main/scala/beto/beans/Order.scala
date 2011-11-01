package beto.beans

/**
 * Created by IntelliJ IDEA.
 * User: jtomin
 * Date: 01.11.11
 * Time: 15:31
 * To change this template use File | Settings | File Templates.
 */

object Order {

  implicit def sortBy[A](a: List[A]) = new {
    def <>(f: A => Double): List[A] = {
      a.sortBy(e => f(e))
    }
  }
}