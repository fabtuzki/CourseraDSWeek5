import scala.collection.mutable.{ArrayBuffer, Stack}
import scala.util.control.Breaks


object IsBST {
  def main(args: Array[String]): Unit = {

    val root = new Node(null, null, 100)
  }

  def isBST(root: Node): Boolean = {
    isBinarySearchTree(root, -1000000000, 1000000000)
  }

  def isBinarySearchTree(node: Node, min: Int, max: Int): Boolean = {
    //base case 1:
    if (node == null) {
      true
    }
    //base case 2:
    if (node.data < min || node.data > max) {
      false
    }
    //recursive fucntion :
    isBinarySearchTree(node.left, min, node.data - 1) && isBinarySearchTree(node.right, node.data + 1, max)
  }


}

class Node(var left: Node, var right: Node, var data: Int, var parent : Node ) {

}
  