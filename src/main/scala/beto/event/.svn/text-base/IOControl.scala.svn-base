/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package beto.event

import _root_.beto.beans.History
import _root_.beto.beans.view.Network
import _root_.beto.handle.{HistoryHandle, ScenarioHandle}
import _root_.beto.view.BeToFrame


class IO(appl: BeToFrame) {

      private val graphHandle = new ScenarioHandle(appl: BeToFrame)
      private val historyHandle = new HistoryHandle(appl: BeToFrame)


      def loadGraph(path: String): Network =  graphHandle.load(path)

      def loadHistory: History =  historyHandle.load()

      def saveHistory(history: History) =  historyHandle.save(history)

      def saveGraph = println("Not supported yet")
}
