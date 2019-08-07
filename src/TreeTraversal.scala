import scala.collection.mutable._

class TreeTraversal {


  def inOrderTraversal(originalTree: Array[(Int, Int, Int)]): Array[Int] = {
    val result = new ArrayBuffer[Int]
    val stackNode = new Stack[(Int, Int, Int, Int)]
    var i = 0
    while (result.length <= originalTree.length) {
      while (originalTree(i)._3 != -1) {
        stackNode.push((i, originalTree(i)._1, originalTree(i)._2, originalTree(i)._3))
        i = originalTree(i)._3
      }
      result.append(stackNode.top._2)
      if (stackNode.top._4 == -1){
      stackNode.pop()
      }else {
        i = stackNode.top._4
      }


    }

    result.toArray
  }

  def preOrderTraversal(originalTree: Array[(Int, Int, Int)]): Array[Int] = {

  }

  def postOrderTraversal(originalTree: Array[(Int, Int, Int)]): Array[Int] = {

  }


}
