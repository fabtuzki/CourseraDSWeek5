import scala.collection.mutable.ArrayBuffer

class TreeTraversal(originalTree: Array[(Int, Int, Int)]) {


  val treeConstructed = ingestTree(originalTree)

  def ingestTree(input: Array[(Int, Int, Int)]): Array[Int] = {
    val result = Array.ofDim[Int](input.length)
    for(i <- 0 until input.length){
      val parentValue = input(i)._1
      val leftChild = input(i)._2
      val rightChile = input(i)._3
      result.update()


    }


  }

  def inOrderTraversal(): Array[Int] = {

  }

  def preOrderTraversal(): Array[Int] = {

  }

  def postOrderTraversal(): Array[Int] = {

  }


}
