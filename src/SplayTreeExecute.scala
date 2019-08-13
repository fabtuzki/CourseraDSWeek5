object SplayTreeExecute {
  def main(args: Array[String]): Unit = {
    val root = new Node(null, null, 4, null)

    val splayTree = new SplayTree()
    splayTree.STInsert(2, splayTree.getRoot(root))

    splayTree.STInsert(6, splayTree.getRoot(root))

    splayTree.STInsert(1, splayTree.getRoot(root))

    splayTree.STInsert(3, splayTree.getRoot(root))

    splayTree.STInsert(5, splayTree.getRoot(root))

    splayTree.STInsert(7, splayTree.getRoot(root))
    println("recursive before deletion")
    splayTree.recursiveInOrderTraversal(splayTree.getRoot(root))

    splayTree.STDelete(5, root)
    println("recursive after deletion")
    splayTree.recursiveInOrderTraversal(splayTree.getRoot(root))


  }

}
