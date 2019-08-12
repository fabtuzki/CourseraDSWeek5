class SplayTree(binarySearchTree: Array[(Int, Int, Int)]) {
  def zigZig(nodeN: Node) = {
    val nodeP = nodeN.parent
    val nodeQ = nodeP.parent
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
    nodeN.parent = nodeQ.parent
    nodeQ.parent = nodeN
    nodeP.parent = nodeN
    nodeP.left = nodeN.right
    nodeQ.right = nodeN.left
    nodeN.left = nodeQ
    nodeN.right = nodeP
  }


  def zig(nodeN: Node) = {
    val nodeP = nodeN.parent
    nodeP.parent = nodeN
    nodeP.right = nodeN.left
    nodeN.left = nodeP
  }

  def zag(nodeN: Node) = {
    val nodeP = nodeN.parent
    nodeP.parent = nodeN
    nodeP.left = nodeN.right
    nodeN.right = nodeP

  }

  def splay(nodeN: Node): Unit = {
    //Determine which case Zig Zig, Zig Zag or Zig
    if (nodeN.parent.parent != null) {
      if (nodeN.parent.left == nodeN &&
        nodeN.parent.parent.left == nodeN.parent) {
        zigZig(nodeN)
      } else if (nodeN.parent.right == nodeN &&
        nodeN.parent.parent.left == nodeN.parent) {
        zigZag(nodeN)
      } else if (nodeN.parent.right == nodeN &&
        nodeN.parent.parent.right == nodeN.parent) {
        zigZigOpposite(nodeN)
      } else if (nodeN.parent.left == nodeN &&
        nodeN.parent.parent.right == nodeN.parent) {
        zigZagOpposite(nodeN)
      }

    } else {
      if (nodeN.parent.left == nodeN) {
        zag(nodeN)
      } else if (nodeN.parent.right == nodeN) {
        zig(nodeN)
      }
    }
  }


  def Find(key: Int, root: Node): Node = {
    if (root.data == key)
      root
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

  }

  def RightAncestor(nodeN: Node): Node = {
    if (nodeN.data < nodeN.parent.data) {
      nodeN.parent
    } else RightAncestor(nodeN.parent)
  }

  def LeftDescendant(nodeN: Node): Node = {

  }


  def STFind(key: Int, root: Node): Node = {
    val nodeN = Find(key, root)
    splay(nodeN)
    nodeN
  }

  def STInsert(key: Int, root: Node): Node = {
    Insert(key, root)
    STFind(key, root)
  }

  def STDelete(key: Int, root: Node): Node = {

  }

  def STSplit(root: Node, key: Int): (Node, Node) = {

  }

  def STMerge(root1: Node, root2: Node) = {

  }

  def cutLeft(node: Node): (Node, Node) = {

  }

  def cutRight(node: Node): (Node, Node) = {

  }


}
