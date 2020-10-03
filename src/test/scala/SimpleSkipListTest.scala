import org.scalatest.{FunSuite, Matchers}

class SimpleSkipListTest extends FunSuite with Matchers{

  test("empty list test") {
    val skipList = new SimpleSkipList
    assert(false equals skipList.contains(0))
    assert(false equals skipList.delete(0))
  }

  test("one element test") {
    val skipList = new SimpleSkipList
    skipList.insert(0)
    assert(true equals skipList.contains(0))
    skipList.delete(0)
    assert(false equals skipList.contains(0))
  }

  test("comprehensive massive test") {
    val skipList = new SimpleSkipList

    import java.util.Random
    val random = new Random()
    val testCaseArray = new Array[Int](50000) // may duplicate
    for(i <- 1 to 50000){
      testCaseArray(i-1) = random.nextInt()
      skipList.insert(testCaseArray(i-1))
    }
    for(x <- testCaseArray){
      assert(true equals skipList.contains(x))
    }
    for(x <- testCaseArray){
      assert(true equals skipList.delete(x))
    }
    for(x <- testCaseArray){
      assert(false equals skipList.contains(x))
    }
  }

}
