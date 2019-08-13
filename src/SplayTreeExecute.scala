object SplayTreeExecute {
  def main(args: Array[String]): Unit = {
    val root = new Node(null, null, 4, null)

    val splayTree = new SplayTree()
    splayTree.STInsert(2, splayTree.getRoot(root))
    println("current root " + splayTree.getRoot(root).data)

    splayTree.STInsert(6, splayTree.getRoot(root))
    println("current root " + splayTree.getRoot(root).data)

    splayTree.STInsert(1, splayTree.getRoot(root))
    println("current root " + splayTree.getRoot(root).data)

    splayTree.STInsert(3, splayTree.getRoot(root))
    println("current root " + splayTree.getRoot(root).data)

    splayTree.STInsert(5, splayTree.getRoot(root))
    println("current root " + splayTree.getRoot(root).data)

    splayTree.STInsert(7, splayTree.getRoot(root))
    println("current root " + splayTree.getRoot(root).data)


    splayTree.recursiveInOrderTraversal(splayTree.getRoot(root))

  }

}
