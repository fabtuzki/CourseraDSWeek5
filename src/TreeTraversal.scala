import scala.collection.mutable._
import scala.util.control.Breaks

class TreeTraversal {


  def inOrderTraversal(originalTree: Array[(Int, Int, Int)]): Array[Int] = {
    val result = new ArrayBuffer[Int]
    val stackNode = new Stack[(Int, Int, Int, Int)]
    var currentIndex = 0

    while (result.length < originalTree.length) {

      //Check node left
      do {
        stackNode.push((currentIndex, originalTree(currentIndex)._1, originalTree(currentIndex)._2, originalTree(currentIndex)._3))
        currentIndex = originalTree(currentIndex)._2
        //        stackNode.foreach(x => println("stack traverse left: " + x))
        //        println("value of i " + currentIndex)
      } while (currentIndex != -1)

      //print key at current position (no more left child)
      result.append(stackNode.top._2)
      //      println("result list after print key 1: " + result.mkString(" "))

      //check if can traverse right :
      val loopControl = new Breaks
      loopControl.breakable {
        while (stackNode.top._4 == -1) {
          stackNode.pop()
          if (stackNode.nonEmpty) {
            stackNode.update(0, (stackNode.top._1, stackNode.top._2, -1, stackNode.top._4))
            if (result.length <= 1 || !result.contains(stackNode.top._2)) {
              result.append(stackNode.top._2)
            }
          } else {
            loopControl.break()
          }
        }

      }
      //      println("result list after print key 2: " + result.mkString(" "))
      if (stackNode.nonEmpty) {
        currentIndex = stackNode.top._4
        stackNode.update(0, (stackNode.top._1, stackNode.top._2, stackNode.top._3, -1))
      }

    }

    result.toArray
  }

  def preOrderTraversal(originalTree: Array[(Int, Int, Int, Int)]): Array[Int] = {
    val result = new ArrayBuffer[Int]
    val stackNode = new Stack[(Int, Int, Int, Int)] //index of the node, key node, index left node, index right node
    var currentIndex = 0

    //traverse:
    //from root down to left, if left out then right
    //Original tree: Key, left index, right index, recorded?
    while (result.length < originalTree.length) {
      //traverse left-most
      do {
        stackNode.push((currentIndex, originalTree(currentIndex)._1, originalTree(currentIndex)._2, originalTree(currentIndex)._3))
        result.append(originalTree(currentIndex)._1)
        originalTree.update(currentIndex, (originalTree(currentIndex)._1, originalTree(currentIndex)._2, originalTree(currentIndex)._3, 1))
        currentIndex = originalTree(currentIndex)._2
      } while (currentIndex != -1)

      //right traverse:

      val loopControl = new Breaks
      loopControl.breakable {
        while (stackNode.top._4 == -1) {
          stackNode.pop()
          if (stackNode.nonEmpty) {
            stackNode.update(0, (stackNode.top._1, stackNode.top._2, -1, stackNode.top._4))
            if (originalTree(stackNode.top._1)._4 == 0) {
              result.append(stackNode.top._2)
            }
          } else {
            loopControl.break()
          }
        }

      }
      println("result list after print key 2: " + result.mkString(" "))
      if (stackNode.nonEmpty) {
        currentIndex = stackNode.top._4
        stackNode.update(0, (stackNode.top._1, stackNode.top._2, stackNode.top._3, -1))
      }


    }
    result.toArray


  }

  def postOrderTraversal(originalTree: Array[(Int, Int, Int)]): Array[Int] = {
    null
  }


}
