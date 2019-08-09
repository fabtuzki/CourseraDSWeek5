import scala.collection.mutable.{ArrayBuffer, Stack}
import scala.util.control.Breaks

object IsBST {
  def main(args: Array[String]): Unit = {


  }

  /*Write in recursive style:
What to do :
1. Record max + min value allowed for each node
2. Check if such node satisfies condition
 + If satisfies, move to the "Next" (unvisited node) , update min - max condition
 + If not return false and break loop

  */


  var check = 0

  //Naive version:
  //fuck recursive :)

  def isBST(originalTree: Array[(Int, Int, Int, Int, Int, Int)]): String = {
    var result = 0
    val visitedNode = new Stack[(Int, Int, Int, Int)]
    var currentIndex = 0
    while (result < originalTree.length) {
      //Check node left
      do {
        visitedNode.push((currentIndex, originalTree(currentIndex)._1, originalTree(currentIndex)._2, originalTree(currentIndex)._3))
        currentIndex = originalTree(currentIndex)._2
        //        stackNode.foreach(x => println("stack traverse left: " + x))
        //        println("value of i " + currentIndex)
      } while (currentIndex != -1)

      //print key at current position (no more left child)
      result += 1
      originalTree.update(visitedNode.top._1, (visitedNode.top._2, visitedNode.top._3, visitedNode.top._4, 1))
      //      println("result list after print key 1: " + result.mkString(" "))

      //check if can traverse right :
      val loopControl = new Breaks
      loopControl.breakable {
        while (visitedNode.top._4 == -1) {
          visitedNode.pop()
          if (visitedNode.nonEmpty) {
            visitedNode.update(0, (visitedNode.top._1, visitedNode.top._2, -1, visitedNode.top._4))
            if (originalTree(visitedNode.top._1)._4 == 0) {
              result += 1
              originalTree.update(visitedNode.top._1, (visitedNode.top._2, visitedNode.top._3, visitedNode.top._4, 1))
            }
          } else {
            loopControl.break()
          }
        }

      }
      //            println("result list after print key 2: " + result.mkString(" "))
      if (visitedNode.nonEmpty) {
        currentIndex = visitedNode.top._4
        visitedNode.update(0, (visitedNode.top._1, visitedNode.top._2, visitedNode.top._3, -1))
      }

    }


  }


}

