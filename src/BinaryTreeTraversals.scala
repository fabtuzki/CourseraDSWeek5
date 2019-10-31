import java.io.IOException

import scala.collection.mutable.ArrayBuffer
import scala.io.Source


object BinaryTreeTraversals {
  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.stdin.getLines().toArray.drop(1).map(x => (x.split(" ")(0).toLong, x.split(" ")(1).toInt, x.split(" ")(2).toInt))

    /*
        val input = Array("0 7 2", "10 -1 -1", "20 -1 6", "30 8 9", "40 3 -1", "50 -1 -1", "60 1 -1", "70 5 4", "80 -1 -1", "90 -1 -1")
          .map(x => (x.split(" ")(0).toLong, x.split(" ")(1).toInt
            , x.split(" ")(2).toInt
          ))
    */

    val treeTraverse = new TreeTraversalThis(input)

    val root = input(0)
    new Thread(null, new Runnable() {
      def run() {
        try {
          treeTraverse.InOrderTraversal(root)
          println(treeTraverse.inOrderResult.mkString(" "))
          treeTraverse.PreOrderTraversal(root)
          println(treeTraverse.preOrderTraversal.mkString(" "))
          treeTraverse.PostOrderTraversal(root)
          println(treeTraverse.postOrderTraversal.mkString(" "))
        } catch {
          case e: IOException =>
        }
      }
    }, "1", 1 << 26).start()


  }

}

class TreeTraversalThis(var treeArr: Array[(Long, Int, Int)]) {
  val inOrderResult = new ArrayBuffer[Long]
  val preOrderTraversal = new ArrayBuffer[Long]
  val postOrderTraversal = new ArrayBuffer[Long]


  def InOrderTraversal(node: (Long, Int, Int)): Unit = {
    if (node._1 == -1)
      return null
    else {
      try {
        InOrderTraversal(treeArr(node._2))
      } catch {
        case e: ArrayIndexOutOfBoundsException =>
      }

      inOrderResult.append(node._1)
      try {
        InOrderTraversal(treeArr(node._3))
      } catch {
        case e: ArrayIndexOutOfBoundsException =>
      }

    }
  }

  def PreOrderTraversal(node: (Long, Int, Int)): Unit = {
    if (node._1 == -1)
      return null
    else {
      //      println("pre order node append " + node)
      preOrderTraversal.append(node._1)
      try {
        PreOrderTraversal(treeArr(node._2))
      } catch {
        case e: ArrayIndexOutOfBoundsException =>
      }
      try {
        PreOrderTraversal(treeArr(node._3))
      } catch {
        case e: ArrayIndexOutOfBoundsException =>
      }

    }
  }


  def PostOrderTraversal(node: (Long, Int, Int)): Unit = {
    if (node._1 == -1) {
      return null
    } else {
      try {
        PostOrderTraversal(treeArr(node._2))
      } catch {
        case e: ArrayIndexOutOfBoundsException =>
      }
      try {

        PostOrderTraversal(treeArr(node._3))
      } catch {
        case e: ArrayIndexOutOfBoundsException =>
      }
      //      println("post order tranversial node : " + node)
      postOrderTraversal.append(node._1)

    }
  }

}

//class NodeThis(var index: Int, var key: Int, var left: NodeThis, var right: NodeThis) {}
