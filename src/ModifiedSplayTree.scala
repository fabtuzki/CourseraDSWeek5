class ModifiedSplayTree (var rootData: Int) {
  var root = new Node(null, null, rootData, null)

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
    if (nodeQ.left != null) {
      nodeQ.left.parent = nodeQ
    }
    nodeP.right = nodeQ
    nodeP.left = nodeN.right
    if (nodeP.left != null) {
      nodeP.left.parent = nodeP
    }
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
    if (nodeQ.right != null) {
      nodeQ.right.parent = nodeQ
    }
    nodeP.left = nodeQ
    nodeP.right = nodeN.left
    if (nodeP.right != null) {
      nodeP.right.parent = nodeP
    }
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
    //update parent for  child node of node N
    if (nodeP.right != null) {
      nodeP.right.parent = nodeP
    }
    nodeQ.left = nodeN.right
    //update parent for  child node of node N
    if (nodeQ.left != null) {
      nodeQ.left.parent = nodeQ
    }
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
    if (nodeP.left != null) {
      nodeP.left.parent = nodeP
    }
    nodeQ.right = nodeN.left
    if (nodeQ.right != null) {
      nodeQ.right.parent = nodeQ
    }
    nodeN.left = nodeQ
    nodeN.right = nodeP
    nodeP.right = nodeP.right
  }


  def zig(nodeN: Node) = {
    val nodeP = nodeN.parent
    nodeP.parent = nodeN
    nodeP.right = nodeN.left
    if (nodeP.right != null) {
      nodeP.right.parent = nodeP
    }
    nodeN.left = nodeP
    nodeN.parent = null
  }

  def zag(nodeN: Node) = {
    val nodeP = nodeN.parent
    nodeP.parent = nodeN
    nodeP.left = nodeN.right
    if (nodeP.left != null) {
      nodeP.left.parent = nodeP
    }
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

    root = getRoot(nodeN)
  }


  def Find(key: Int, nodeInput: Node): Node = {
    if (nodeInput.data == key) {
      nodeInput
    }
    else if (nodeInput.data > key) {
      if (nodeInput.left != null) {
        Find(key, nodeInput.left)
      } else nodeInput
    }
    else {
      if (nodeInput.right != null) {
        Find(key, nodeInput.right)
      } else nodeInput
    }

  }

  def Insert(key: Int, nodeInput: Node): Unit = {
    val nearestNode = Find(key, nodeInput)
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


  def STFind(key: Int, nodeInput: Node): Node = {
    val nodeN = Find(key, nodeInput)
    splay(nodeN)
    nodeN
  }

  def STInsert(key: Int, nodeInput: Node): Node = {
    Insert(key, nodeInput)
    println("Inserted key " + key)
    STFind(key, nodeInput)
  }

  def STDelete(key: Int): Unit = {
    val nodeN = Find(key, root)
    val nodeNNext = Next(nodeN)
    splay(nodeNNext)
    splay(nodeN)


    val left = nodeN.left
    val right = nodeN.right
    val parent = nodeN.parent
    if (parent.left == nodeN) {
      parent.left = right
    } else {
      parent.right = right
    }

    right.left = left
    left.parent = right

    right.parent = parent
    nodeN.left = null
    nodeN.right = null
    nodeN.parent = null


  }

  def STSplit(key: Int): (Node, Node) = {
    val nodeN = Find(key, root)
    while (nodeN != root) {
      splay(nodeN)
    }
    if (nodeN.data >= key) {
      cutLeft(nodeN)
    } else {
      cutRight(nodeN)
    }
  }

  def STMerge(root1: Node, root2: Node) = {
    var nodeN1 = Find(1000000000, root1)
    root = root1
    while (nodeN1 != root) {
      splay(nodeN1)
    }
    nodeN1.right = root2
    root2.parent = nodeN1
    root = nodeN1
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

  def recursivePreOrderTraversal(node: Node): Unit = {
    if (node == null) {
      return
    }
    println(node.data)
    recursivePreOrderTraversal(node.left)
    recursivePreOrderTraversal(node.right)
  }


}
