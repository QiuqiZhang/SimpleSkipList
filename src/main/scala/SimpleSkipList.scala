import java.util.Random



// the SimpleSkipList is in ascending order (single direction)
// allow duplicates
// element type: Int

case class Node(
                 key: Integer = null,
                 var right: Node = null,
                 down: Node = null
               )

class SimpleSkipList {
  private var topHead: Node = Node()
  private val random = new Random()

  def contains(target: Int): Boolean = {
    var currentNode = this.topHead
    while (currentNode != null) {
      val rightNode = currentNode.right
      if (null == rightNode || rightNode.key > target) {
        currentNode = currentNode.down
      } else {
        if (rightNode.key < target) {
          currentNode = rightNode
        } else {
          return true //rightNode.value == target
        }
      }
    }
    false
  }

  private def recursiveInsert(currentNode: Node, keyToInsert: Int): Node = {
    // find the rightest node in horizontal search
    var cur = currentNode
    while(cur.right != null && cur.right.key <= keyToInsert) {
      cur = cur.right
    }

    // insert in bottom layer
    if(null == cur.down) {
      val newNode = Node(key = keyToInsert, right = cur.right)
      cur.right = newNode
      return newNode
    }

    // insert in index layer
    val downNode = recursiveInsert(cur.down, keyToInsert)
    if (null != downNode && random.nextBoolean()) {
      val newNode = Node(key = keyToInsert, right = cur.right, down = downNode)
      cur.right = newNode
      newNode
    } else null
  }

  def insert(num: Int): Unit = {
    val downNode = recursiveInsert(this.topHead, num)

    // if inserting a new node in the top layer, generate a new top layer by a probability of 50%
    if (downNode != null && random.nextBoolean()) {
      val newNode = Node(key = num, down = downNode)
      this.topHead = Node(right = newNode, down = this.topHead)
    }
  }

  def delete(num: Int): Boolean = {
    var isDeleted = false

    var currentNode = this.topHead
    while (currentNode != null) {
      val rightNode = currentNode.right
      if (null == rightNode || rightNode.key > num) {
        currentNode = currentNode.down
      } else {
        if (rightNode.key < num) {
          currentNode = rightNode
        } else {
          currentNode.right = rightNode.right //remove rightNode when rightNode.value == target
          currentNode = currentNode.down //continue deleting on next layer
          isDeleted = true
        }
      }
    }

    isDeleted
  }


}