package MyModules

sealed trait MyList[+A] // declare a trait called "MyList" with no associated methods

case object MyNil extends MyList[Nothing] // data constructor that represents an empty MyList
case class MyCons[+A](head: A, tail: MyList[A]) extends MyList[A] // data constructor representing non-empty MyList

object MyList {

  def sum(ints: MyList[Int]): Int = ints match {
    case MyNil => 0
    case MyCons(x, xs) => x + sum(xs)
  }

  def product(doubles: MyList[Double]): Double = doubles match {
    case MyNil => 1.0
    case MyCons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): MyList[A] = 
    if (as.isEmpty) MyNil
    else MyCons(as.head, apply(as.tail: _*))

  def tail[A](l: MyList[A]): MyList[A] = l match {
    case MyNil => sys.error("tail of empty MyList")
    case MyCons(_, xs) => xs
  }

  def setHead[A](l: MyList[A], h: A): MyList[A] = l match {
    case MyNil => sys.error("setHead of empty MyList")
    case MyCons(_, xs) => MyCons(h, xs)
  }

  def drop[A](l: MyList[A], n: Int): MyList[A] = 
    if (n <= 0) l
    else drop(tail(l), n-1)

  def dropWhileOld[A](l: MyList[A], f: A => Boolean): MyList[A] = l match {
    case MyNil => MyNil
    case MyCons(x, xs) => {
      if (f(x)) dropWhileOld(xs, f)
      else MyCons(x, xs)
    }
  }

  def dropWhile[A](as: MyList[A])(f: A => Boolean): MyList[A] = as match {
    case MyCons(h, t) if (f(h)) => dropWhile(t)(f)
    case _ => as
  }

  def appendHead[A](l: MyList[A], h: A): MyList[A] = MyCons(h, l)

  def appendTail[A](l: MyList[A], t: A): MyList[A] = l match {
    case MyNil => MyCons(t, l)
    case MyCons(x, xs) => appendHead(appendTail(xs, t), x)
  }
  
  def init[A](l: MyList[A]): MyList[A] = l match {
    case MyNil => MyNil
    case MyCons(_, MyNil) => MyNil
    case MyCons(x, xs) => appendHead(init(xs), x)
  }

  def foldRight[A, B](as: MyList[A], z: B)(f: (A, B) => B): B = as match {
    case MyNil => z
    case MyCons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ns: MyList[Int]): Int = foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: MyList[Double]): Double = foldRight(ns, 1.0)(_ * _)

  def length[A](as: MyList[A]): Int = foldRight(as, 0)((_, y) => y + 1)

  def foldLeft[A, B](as: MyList[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def loop(ns: MyList[A], res: B): B = ns match {
      case MyNil => res
      case MyCons(x, xs) => loop(xs, f(res, x))
    }
    loop(as, z)
  }

  def sum3(ns: MyList[Int]): Int = foldLeft(ns, 0)(_+_)

  def product3(ns: MyList[Double]): Double = foldLeft(ns, 1.0)(_*_)

  def length2[A](ns: MyList[A]): Int = foldLeft(ns, 0)((y, _) => y + 1)

  def reverse[A](as: MyList[A]): MyList[A] = foldLeft(as, MyNil:MyList[A])((t, h) => MyCons(h,t))

  def append[A](as: MyList[A], ns: MyList[A]): MyList[A] = foldRight(as, ns)(MyCons(_,_))

  def concatenate[A](as: MyList[MyList[A]]): MyList[A] = foldRight(as, MyList[A]())(append(_,_))

  def addOne(as: MyList[Int]): MyList[Int] = foldRight(as, MyNil:MyList[Int])((h, t) => MyCons(h+1, t))
  
  def doubleToString(as: MyList[Double]): MyList[String] = foldRight(as, MyList[String]())((h, t) => MyCons(h.toString, t))

  def map[A, B](as: MyList[A])(f: A => B): MyList[B] = foldRight(as, MyList[B]())((h, t) => MyCons(f(h), t))

  def filter[A](as: MyList[A])(f: A => Boolean): MyList[A] = 
    foldRight(as, MyList[A]())((h, t) => 
      if (f(h)) MyCons(h, t) 
      else t
  )

  def flatMap[A, B](as: MyList[A])(f: A => MyList[B]): MyList[B] = 
    foldRight(as, MyList[B]())((h, t) => append(f(h), t))

  def flatMap2[A, B](as: MyList[A])(f: A => MyList[B]): MyList[B] = concatenate(map(as)(f))

  def filterViaFlatMap[A](as: MyList[A])(f: A => Boolean): MyList[A] = 
    flatMap2(as)(x => 
      if (f(x)) MyCons(x, MyNil) 
      else MyNil
    )

  def addTwoLists(as: MyList[Int], bs: MyList[Int]): MyList[Int] = (as, bs) match {
    case (MyNil, _) => bs // could also be `MyNil`
    case (_, MyNil) => as
    case (MyCons(x, xs), MyCons(y, ys)) => MyCons(x+y, addTwoLists(xs, ys))
  }

  def zipWith[A, B, C](as: MyList[A], bs: MyList[B])(f: (A, B) => C): MyList[C] = (as, bs) match {
    case (MyNil, _) => MyNil
    case (_, MyNil) => MyNil
    case (MyCons(x, xs), MyCons(y, ys)) => MyCons(f(x, y), zipWith(xs, ys)(f))
  }

  def listsAreEqual[A](as: MyList[A], bs: MyList[A]): Boolean = {
    val e = zipWith(as, bs)((a, b) => a == b)
    foldLeft(e, true)((a, b) => a && b)
  }

  def take[A](as: MyList[A], i: Int): MyList[A] = (as, i) match {
    case (MyNil, _) => MyNil // i > as.length
    case (_, 0) => MyNil // we've taken up to i
    case (MyCons(x, xs), _) => MyCons(x, take(xs, i-1))
  }

  def takeWindow[A](as: MyList[A], i: Int, j: Int): MyList[A] = drop(take(as, j), i)

  def hasSubsequence[A](sup: MyList[A], sub: MyList[A]): Boolean = {
    val l = length(sup) - length(sub)

    def loop(i: Int, j: Int, res: Boolean): Boolean = (j, res) match {
      case (k, _) if (k == length(sup)) => res
      case (_, true) => res
      case _ => {
        val window = takeWindow(sup, i, j)
        val newRes = listsAreEqual(window, sub)
        loop(i+1, j+1, newRes)
      }
    }

    loop(0, l, false)

    // 1. create function that determines if 2 lists are equal
    // 1.5. create function that creates a sub array
    //      cut first i elements, cut back j elements
    // 2. manually slide len sub accross sup
    // 3. if any window matches, return true
    // 4. return false
  }





















}











































