package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  // didn't get the additional + 1 for the Branch case
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1 
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(x) => x
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  // didn't get Leaf having a depth of 0 either
  // makes sense though, need to count the depth on the branch
  // since you'll be descending/traversing it to get to the next
  // possible depth
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) = Branch(map(l)(f), map(r)(f)) 
  }

  // exercise 3.29
  // didn't get fold, but should have looked up its signature to start with
  // look it up in master
}
