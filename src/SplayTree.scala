class SplayTree {

  def getRoot(nodeN: Node): Node = {
    if (nodeN.parent == null) {
      nodeN
    } else {
      getRoot(nodeN.parent)
    }
  }

  def zigZig(nodeN: Node) = {
    val nodeP = nodeN.parent
    val nodeQ = nodeP.parent
    val nodeParent = nodeQ.parent
    if (nodeParent != null && nodeParent.right == nodeQ) {
      nodeParent.right = nodeN
    } else if (nodeParent != null && nodeParent.left == nodeQ) {
      nodeParent.left = nodeN
    }
    nodeN.parent = nodeQ.parent
    nodeP.parent = nodeN
    nodeQ.parent = nodeP
    nodeQ.left = nodeP.right
    nodeP.right = nodeQ
    nodeP.left = nodeN.right
    nodeN.right = nodeP
  }

  def zigZigOpposite(nodeN: Node) = {
    val nodeP = nodeN.parent
    val nodeQ = nodeP.parent
    val nodeParent = nodeQ.parent
    if (nodeParent != null && nodeParent.right == nodeQ) {
      nodeParent.right = nodeN
    } else if (nodeParent != null && nodeParent.left == nodeQ) {
      nodeParent.left = nodeN
    }
    nodeN.parent = nodeQ.parent
    nodeP.parent = nodeN
    nodeQ.parent = nodeP
    nodeQ.right = nodeP.left
    nodeP.left = nodeQ
    nodeP.right = nodeN.left
    nodeN.left = nodeP
  }


  def zigZag(nodeN: Node) = {
    val nodeP = nodeN.parent
    val nodeQ = nodeP.parent
    val nodeParent = nodeQ.parent
    if (nodeParent != null && nodeParent.right == nodeQ) {
      nodeParent.right = nodeN
    } else if (nodeParent != null && nodeParent.left == nodeQ) {
      nodeParent.left = nodeN
    }

    nodeN.parent = nodeQ.parent
    nodeQ.parent = nodeN
    nodeP.parent = nodeN
    nodeP.right = nodeN.left
    nodeQ.left = nodeN.right
    nodeN.left = nodeP
    nodeN.right = nodeQ
  }

  def zigZagOpposite(nodeN: Node) = {
    val nodeP = nodeN.parent
    val nodeQ = nodeP.parent
    val nodeParent = nodeQ.parent
    if (nodeParent != null && nodeParent.right == nodeQ) {
      nodeParent.right = nodeN
    } else if (nodeParent != null && nodeParent.left == nodeQ) {
      nodeParent.left = nodeN
    }

    nodeN.parent = nodeQ.parent
    nodeQ.parent = nodeN
    nodeP.parent = nodeN
    nodeP.left = nodeN.right
    nodeQ.right = nodeN.left
    nodeN.left = nodeQ
    nodeN.right = nodeP
    nodeP.right = nodeP.right
  }


  def zig(nodeN: Node) = {
    val nodeP = nodeN.parent
    nodeP.parent = nodeN
    nodeP.right = nodeN.left
    nodeN.left = nodeP
    nodeN.parent = null
  }

  def zag(nodeN: Node) = {
    val nodeP = nodeN.parent
    nodeP.parent = nodeN
    nodeP.left = nodeN.right
    nodeN.right = nodeP
    nodeN.parent = null

  }

  def splay(nodeN: Node): Unit = {
    //Determine which case Zig Zig, Zig Zag or Zig
    if (nodeN.parent.parent != null) {
      if (nodeN.parent.left == nodeN &&
        nodeN.parent.parent.left == nodeN.parent) {
        println("Do zigzig transformation")
        zigZig(nodeN)
      } else if (nodeN.parent.right == nodeN &&
        nodeN.parent.parent.left == nodeN.parent) {
        println("Do zigzag transformation")
        zigZag(nodeN)
      } else if (nodeN.parent.right == nodeN &&
        nodeN.parent.parent.right == nodeN.parent) {
        println("Do zigzig opposite transformation")

        zigZigOpposite(nodeN)
      } else if (nodeN.parent.left == nodeN &&
        nodeN.parent.parent.right == nodeN.parent) {
        println("Do zigzag opposite transformation")

        zigZagOpposite(nodeN)
      }

    } else {
      if (nodeN.parent.left == nodeN) {
        println("Do zag transformation")

        zag(nodeN)
      } else if (nodeN.parent.right == nodeN) {
        println("Do zig transformation")

        zig(nodeN)
      }
    }
  }


  def Find(key: Int, root: Node): Node = {
    if (root.data == key) {
      root
    }
    else if (root.data > key) {
      if (root.left != null) {
        Find(key, root.left)
      } else root
    }
    else {
      if (root.right != null) {
        Find(key, root.right)
      } else root
    }

  }

  def Insert(key: Int, root: Node): Unit = {
    val nearestNode = Find(key, root)
    if (nearestNode.data < key) {
      nearestNode.right = new Node(null, null, key, nearestNode)
    } else if (nearestNode.data > key) {
      nearestNode.left = new Node(null, null, key, nearestNode)
    }
  }

  def Next(nodeN: Node): Node = {
    if (nodeN.right != null) {
      LeftDescendant(nodeN.right)
    } else RightAncestor(nodeN)
  }

  def RightAncestor(nodeN: Node): Node = {
    if (nodeN.data < nodeN.parent.data) {
      nodeN.parent
    } else RightAncestor(nodeN.parent)
  }

  def LeftDescendant(nodeN: Node): Node = {
    if (nodeN.left != null) {
      LeftDescendant(nodeN.left)
    } else {
      nodeN
    }
  }


  def STFind(key: Int, root: Node): Node = {
    val nodeN = Find(key, root)
    splay(nodeN)
    nodeN
  }

  def STInsert(key: Int, root: Node): Node = {
    Insert(key, root)
    println("Inserted key " + key)
    STFind(key, root)
  }

  def STDelete(key: Int, root: Node): Unit = {
    splay(Next(root))
    splay(root)
    val left = root.left
    val right = root.right
    right.left = left
    left.parent = right
    //right becomes root
    right.parent = null
  }

  def STSplit(root: Node, key: Int): (Node, Node) = {
    val nodeN = Find(key, root)
    splay(nodeN)
    if (nodeN.data > key) {
      cutLeft(root)
    } else if (nodeN.data < key) {
      cutRight(root)
    } else {
      (nodeN.left, nodeN.right)
    }
  }

  def STMerge(root1: Node, root2: Node) = {
    val nodeN = Find(10000000, root1)
    splay(nodeN)
    nodeN.right = root2
    root2.parent = nodeN
  }

  def cutLeft(node: Node): (Node, Node) = {
    val L = node.left
    node.left = null
    L.parent = null
    (L, node)
  }

  def cutRight(node: Node): (Node, Node) = {
    val R = node.right
    node.right = null
    R.parent = null
    (R, node)
  }

  def recursiveInOrderTraversal(node: Node): Unit = {
    if (node == null) {
      return
    }

    recursiveInOrderTraversal(node.left)
    println(node.data)
    recursiveInOrderTraversal(node.right)

  }


}
