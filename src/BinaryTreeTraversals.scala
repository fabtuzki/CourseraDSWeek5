import scala.io.Source

object BinaryTreeTraversals {
  def main(args: Array[String]): Unit = {
    val source = Source.fromFile("C:\\Users\\Jade Phung\\Documents\\homework2\\week4_binary_search_trees\\1_tree_traversals\\tests\\21").getLines().toArray.drop(1)

    val input = source.map(x => (x.split(" ")(0).toInt, x.split(" ")(1).toInt, x.split(" ")(2).toInt, 0))

//        val input = Array((0, 7, 2, 0),
//          (10, -1, -1, 0),
//          (20, -1, 6, 0),
//          (30, 8, 9, 0),
//          (40, 3, -1, 0),
//          (50, -1, -1, 0),
//          (60, 1, -1, 0),
//          (70, 5, 4, 0),
//          (80, -1, -1, 0),
//          (90, -1, -1, 0))

    val treeTraverse = new TreeTraversal

//        treeTraverse.inOrderTraversal(input)


    val output = treeTraverse.preOrderTraversal(input)
    val correctOutputSource = Source.fromFile("C:\\Users\\Jade Phung\\Documents\\homework2\\week4_binary_search_trees\\1_tree_traversals\\tests\\21.a").getLines().toArray

    val correctOutput = correctOutputSource(1).split(" ").map(_.toInt)
    for (i <- 0 until correctOutput.length) {
      val isTrue = if (output(i) == correctOutput(i)) {
        true
      } else {
        false
      }
      println("index: " + i + " calculated output: " + output(i) + " correct output : " + correctOutput(i) + " " + isTrue)


    }


  }

}
