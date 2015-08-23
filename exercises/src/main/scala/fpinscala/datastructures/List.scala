package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] =
    l match {
       case Nil => Nil
       //case Cons(x, xs) => xs
       // from answer, good style to make x above into _, since we're not going to use it
       case Cons(_, xs) => xs
    } 

  // the answer here may be subjective
  // author returns exceptin for tail of Nil
  // somewhat determines same must be done here, but if we return Nil, then h + Nil = h
  def setHead[A](l: List[A], h: A): List[A] = Cons(h, tail(l))

  // same argument above goes for here
  def drop[A](l: List[A], n: Int): List[A] =
    n match {
      case x if x < 1 => l
      case _ => drop(tail(l), n-1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(x, xs) if f(x) => dropWhile(xs, f)
      case _ => l
    }

  // didn't work on init from exercises, but will need for foldLeft below
  // so I copied it in
  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("init of empty list")
      case Cons(_,Nil) => Nil
      case Cons(h,t) => Cons(h,init(t))
    }
  def init2[A](l: List[A]): List[A] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]
    @annotation.tailrec
    def go(cur: List[A]): List[A] = cur match {
      case Nil => sys.error("init of empty list")
      case Cons(_,Nil) => List(buf.toList: _*)
      case Cons(h,t) => buf += h; go(t)
    }
    go(l)
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, y) => y + 1)

  // duh duh duh, foldleft goes left to right
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z 
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }  
  }

  def sum3(ns: List[Int]) = foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((x, _) => x + 1) 

  // almost had this, but need to look up answer to figure out to represent
  // empty list for initial values as List[A]() (instead of Nil)
  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((h, t) => Cons(t, h))

  // skipping exercise 3.13

  // didn't get append2 (3.14), I thought about foldLeft, but it's done via foldRight, see master answers
  /*
  `append` simply replaces the `Nil` constructor of the first list with the second list, which is exactly the operation
  performed by `foldRight`.
  */
  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_,_))

  // didn't try 3.15, but same idea as the noticing of what append is doing via foldRight
  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])(append)    

  // didn't get exercise 3.16 either, but notice that you're having to create new lists, so starting at "end" to
  // pick up Nil necessary should lead you towards foldRight
  def add1(ns: List[Int]) = foldRight(ns, Nil:List[Int])((h, t) => Cons(h + 1, t))

  def doubleToString(ns: List[Double]): List[String] =
    foldRight(ns, Nil:List[String])((h, t) => Cons(h.toString, t))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil:List[B])((h, t) => Cons(f(h), t))

  // map (exercise 3.18) - got right, but not stack safe, see master answers
  // probably best way would be local mutation vs a local list (see example 2, makes the most "sense" to me)
  def map_2[A,B](l: List[A])(f: A => B): List[B] = {
    val buf = new collection.mutable.ListBuffer[B]
    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h,t) => buf += f(h); go(t)
    }
    go(l)
    List(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
  }

  // I did first with a match, but that's unnecessary
  // same discussion about map appies here
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil:List[A])((h, t) => if (f(h)) Cons(h, t) else t )

  def filter_2[A](l: List[A])(f: A => Boolean): List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h,t) => if (f(h)) buf += h; go(t)
    }
    go(l)
    List(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
  }

  // exercise 3.20 - i'm not sure if I got it right, the solution given is definitely the simplest way to think about it
  //def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
  //  foldRight(as, Nil:List[B])((h, t) => append(f(h), t)) 
  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f)) 

  // got wrong, 2 things to notice
  // 1. flatMap is a _map_, works over one value at a time, not two
  // 2. flatMap's 2nd param returns a list, so you need to as well  
  // wrong:
  // def filterViaflatMap[A](as: List[A])(f: A => Boolean): List[A] =
  // flatMap(as)((h, t) => if (f(h)) Cons(h, t) else t) 
  // right:
  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) List(a) else Nil)

  // your solution wasn't very close
  // work on the args together, so match on them together
  //def zipInts(l: List[Int], r: List[Int]): List[Int] =
  //  l match {
  //    case Nil => l
  //    case Cons(h, t) => Cons(h + r.head, zipInts(t, r.tail)) 
  //  }    
  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, addPairwise(t1,t2))
  }

  // this is showing you you should be using Eclipse for this to help with signatures
  // you should also be looking up signatures in regular docs to help with some of these questions

  // needed improvments on this, although you were closer
  // 1. needed to pass f to 2nd call of zipWith
  // 2. signature should be
  //    zipWith[A,B,C](a: List[A], b: List[B])(f: (A,B) => C): List[C] 
  //  
  def zipWith[A](a: List[A], b: List[A])(f: (A, A) => A): List[A] =
    (a, b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
    }

}
