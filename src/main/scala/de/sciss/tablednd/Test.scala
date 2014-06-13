package de.sciss.tablednd

import java.awt.datatransfer.{DataFlavor, Transferable}
import java.awt.event.InputEvent
import javax.swing.TransferHandler.TransferSupport
import javax.swing.table.AbstractTableModel
import javax.swing.{JTable, JComponent, TransferHandler, DropMode}

import scala.swing._
import scala.util.Try

object Test extends SimpleSwingApplication {
  lazy val top: Frame = new MainFrame {
    val tabModel = new AbstractTableModel {
      private var data = Vector[(String, Int)](
        "Foo" -> 1, "Bar" -> 2
      )

      def insert(idx: Int, key: String, value: Int): Unit = {
        data = data.patch(idx, Seq(key -> value), 0)
        fireTableRowsInserted(idx, idx)
      }

      def update(idx: Int, key: String): Unit = {
        data = data.patch(idx, Seq(key -> data(idx)._2), 1)
        fireTableCellUpdated(idx, 0)
      }

      def getRowCount: Int = data.size

      def getColumnCount: Int = 2

      def getValueAt(row: Int, col: Int): AnyRef = {
        val d = data(row)
        if (col == 0) d._1 else d._2.asInstanceOf[AnyRef]
      }
    }

    val tab = new Table { _tab =>
      model = tabModel
      peer.setDragEnabled(true)
      peer.setDropMode(DropMode.ON_OR_INSERT_ROWS)
      peer.setTransferHandler(new TransferHandler {
        override def canImport(support: TransferSupport): Boolean = {
          val res = support.isDrop && {
            val dl = support.getDropLocation.asInstanceOf[JTable.DropLocation]
            val locOk = dl.isInsertRow || dl.getColumn == 0
            // println(s"locOk? $locOk")
            locOk && support.isDataFlavorSupported(DataFlavor.stringFlavor)
          }
          res
        }

        override def importData(support: TransferSupport): Boolean = {
          support.isDrop && {
            val dl  = support.getDropLocation.asInstanceOf[JTable.DropLocation]
            val s   = support.getTransferable.getTransferData(DataFlavor.stringFlavor).asInstanceOf[String]
            if (dl.isInsertRow) {
              val value0  = (math.random * 100).toInt
              val row     = dl.getRow
              val valOptS = Dialog.showInput[String](message = "Value", initial = value0.toString)
              val valOpt  = valOptS.flatMap(s => Try(s.toInt).toOption)
              valOpt.exists { value =>
                // println(s"insert($row, $s, $value)")
                tabModel.insert(row, s, value)
                true
              }
            } else {
              tabModel.update(dl.getRow, s)
            }
            true
          }
        }
      })
    }

    val words = "This is simple to accomplish by installing the necessary logic in the canImport method of the TransferHandler class".split(' ')

    val lb = new Label("Drag Me") {
      val th = new TransferHandler {
        override def createTransferable(c: JComponent): Transferable = {
          new Transferable {
            def getTransferData(flavor: DataFlavor): AnyRef = if (flavor == DataFlavor.stringFlavor) {
              words((math.random * words.size).toInt)
            } else null

            def getTransferDataFlavors: Array[DataFlavor] = Array(DataFlavor.stringFlavor)

            def isDataFlavorSupported(flavor: DataFlavor): Boolean = flavor == DataFlavor.stringFlavor
          }
        }

        override def getSourceActions(c: JComponent): Int = TransferHandler.COPY

        override def exportAsDrag(comp: JComponent, e: InputEvent, action: Int): Unit = super.exportAsDrag(comp, e, action)
      }

      peer.setTransferHandler(th)

      listenTo(mouse.moves)
      reactions += {
        case evt @ event.MouseDragged(_, _, _) => th.exportAsDrag(peer, evt.peer, TransferHandler.COPY)
      }
    }

    contents = new BoxPanel(Orientation.Vertical) {
      contents += new ScrollPane(tab)
      contents += lb
    }
    pack()
    centerOnScreen()
    open()
  }
}
