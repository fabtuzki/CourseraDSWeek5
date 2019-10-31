import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.control.Breaks

object isBSTNew {
  def main(args: Array[String]): Unit = {
    //Test the traverse downward method:
    val input = scala.io.Source.stdin.getLines().toArray.drop(1)
    val inputTree = input.zipWithIndex
      .map(x => (x._1.split(" ")(0).toDouble, x._1.split(" ")(1).toInt, x._1.split(" ")(2).toInt, x._2))

    if (inputTree.length > 0) {
      traverseDownward(inputTree) match {
        case true => println("CORRECT")
        case false => println("INCORRECT")

      }

    } else {
      println("CORRECT")
    }
  }


  def traverseDownward(tree: Array[(Double, Int, Int, Int)]): Boolean = {
    //The purpose of this is to update the value min/max of the node
    //False case includes:
    //Min >= Max
    val arrMin = Array.fill[Double](tree.length)(Double.NegativeInfinity)
    val arrMax = Array.fill[Double](tree.length)(Double.PositiveInfinity)

    val root = tree(0)
    var check = true
    //Set up a stack:
    val BSTStack = new ListBuffer[(Double, Int, Int, Int)]
    //available method includes: head, prepend, remove(0) = pop
    BSTStack.prepend(root)
    var i = 1

    val loopControl = new Breaks
    loopControl.breakable {
      while (i < tree.length && check == true) {
        //traverse left:
        if (BSTStack.head._2 != -1 && tree(BSTStack.head._2)._1 != -1) {
          //update the value of min and max for the current node:
          //max = previous node's value
          arrMax.update(BSTStack.head._2, BSTStack.head._1)
          arrMin.update(BSTStack.head._2, arrMin(BSTStack.head._4))
          BSTStack.prepend(tree(BSTStack.head._2))
          i += 1
          /*
                    println("prepend 1 left element : " + BSTStack.head)
                    println("array min " + arrMin.mkString(" "))
                    println("array max " + arrMax.mkString(" "))
          */

          //Then now we check the condition:
          //Current min value > current max
          if (arrMax(BSTStack.head._4) <= arrMin(BSTStack.head._4) ||
            BSTStack.head._1 <= arrMin(BSTStack.head._4) ||
            BSTStack.head._1 >= arrMax(BSTStack.head._4)) {
            check = false
            loopControl.break()
          }
        } else if (BSTStack.head._3 != -1 && tree(BSTStack.head._3)._1 != -1) {
          //traverse right
          //update the value of min and max for the current node:
          arrMax.update(BSTStack.head._3, arrMax(BSTStack.head._4))
          arrMin.update(BSTStack.head._3, BSTStack.head._1)
          BSTStack.prepend(tree(BSTStack.head._3))
          i += 1
          /*
                    println("prepend 1 right element : " + BSTStack.head)
                    println("array min " + arrMin.mkString(" "))
                    println("array max " + arrMax.mkString(" "))
          */

          if (arrMax(BSTStack.head._4) <= arrMin(BSTStack.head._4) ||
            BSTStack.head._1 <= arrMin(BSTStack.head._4) ||
            BSTStack.head._1 >= arrMax(BSTStack.head._4)) {
            check = false
            loopControl.break()
          }


        } else {
          //Remove from tree the checked node
          tree.update(BSTStack.head._4, (-1, -1, -1, -1))
          try {
            //            println("remove 1 element: " + BSTStack.head)
            BSTStack.remove(0)
          }
          catch {
            case e: NoSuchElementException => loopControl.break()
          }
        }
      }


    }


    check
  }


  //Let's try to do an iterative version of depth first search:


  def checkBinaryTree(tree: Array[(Double, Int, Int)]): Boolean = {

    false
  }

}

