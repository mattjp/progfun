package MyModules

object MainModule {

  def fibonacci(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, a: Int = 0, b: Int = 1): Int = {
      if (n == 0) a
      else if (n == 1) b
      else loop(n-1, b, a+b)
    }

    loop(n)
  }

  def format(s: String, n: Int, f: Int => Int) = {
    val msg = "Function: %s \nInput: %d \nOutput: %d"
    msg.format(s, n, f(n))
  }

  def isSortedInt(as: Array[Int]): Boolean = {

    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length - 1) true
      else if (as(n) >= as(n + 1)) false
      else loop(n + 1)
    }

    loop(0)

  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length - 1) true
      else if (ordered(as(n), as(n + 1))) false
      else loop(n + 1)
    }

    loop(0)

  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  def uncurry[A, B, C](f: A => (B => C)): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

  def main(args: Array[String]): Unit = {

    println("-- Running Main --")

    println(format("Fibonacci", 7, fibonacci))
    println(isSorted(Array(1, 2, 3, 4, 5), (a: Int, b: Int) => a >= b))

    val myList = MyList(1, 2, 3, 4, 5)
    val result = myList match {
      case MyCons(x, MyCons(2, MyCons(4, _))) => x
      case MyNil => 42
      case MyCons(x, MyCons(y, MyCons(3, MyCons(4, _)))) => x + y
      case MyCons(h, t) => h + MyList.sum(t)
      case _ => 101
    }
    
    val newHead = MyList.setHead(myList, 6)
    // println(MyList.dropWhileOld(newHead, (x: Int) => x < 4))

    // println(MyList.dropWhile(newHead)(x => x < 4))

    val appHead = MyList.appendHead(myList, 0)
    // println(appHead)

    val appTail = MyList.appendTail(myList, 6)
    // println(appTail)

    val initMyList = MyList.init(myList)
    // println(initMyList)

    val foldRightEx = MyList.foldRight(MyList(1, 2, 3), MyNil:MyList[Int])(MyCons(_,_))
    // println(foldRightEx)

    val foldRightTest = MyList.product2(MyList[Double](1, 2, 3))
    // println(foldRightTest)

    val foldLeftTest = MyList.product3(MyList[Double](1, 2, 3))
    // println(foldLeftTest)

    val myListLength = MyList.length(myList)
    val myListLength2 = MyList.length2(myList)
    // println(myListLength)
    // println(myListLength2)

    val reverseTest = MyList.reverse(myList)
    // println(reverseTest)

    val appendTest = MyList.append(myList, MyList(6, 7, 8))
    // println(appendTest)

    val concatTest = MyList.concatenate(MyList(MyList(1, 2, 3), MyList(4, 5, 6), MyList(7, 8, 9)))
    // println(concatTest)

    val addOneTest = MyList.addOne(myList)
    // println(addOneTest)

    val doubleToStringTest = MyList.doubleToString(MyList[Double](1, 2, 3, 4, 5))
    // println(doubleToStringTest)

    val mapTest = MyList.map(myList)(_*5)
    // println(mapTest)

    val filterTest = MyList.filter(myList)(_ % 2 == 0)
    // println(filterTest)

    val flatMapTest = MyList.flatMap2(myList)(i => MyList(i, i))
    // println(flatMapTest)

    val filterViaFlatMapTest = MyList.filterViaFlatMap(myList)(_ % 2 == 1)
    // println(filterViaFlatMapTest)

    val addTwoListsTest = MyList.addTwoLists(MyList(1, 2, 3), MyList(4, 5, 6))
    // println(addTwoListsTest)

    val zipWithTest = MyList.zipWith(MyList(1, 2, 3), MyList(4, 5, 6))(_+_)
    println(zipWithTest)
  }

}

// object MyModule2 {

//   def sayGoodbye = println("Goodbye!")

//   def main2(args: Array[String] = Array.empty): Unit = sayGoodbye

// }

// Run using SBT
// object Main extends App {
//   println("--- Running --- ")

//   val tc = new testClass("hello!")
//   tc.printS

//   val tcc = testCaseClass("goodbye!")
//   tcc.printS

// }

// class testClass(s: String) {

//   val _s = s

//   def printS = println(s)

// }


// case class testCaseClass(s: String) {

//   def printS = println(s)

// }
