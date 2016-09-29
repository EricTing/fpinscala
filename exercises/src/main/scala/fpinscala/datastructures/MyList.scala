sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def drop(l: List[Int], n: Int): List[Int] = {
    l match {
      case Nil => throw new Exception("drop from an empty list")
      case Cons(_, tail) => drop(tail, n - 1)
    }
  }

  def tail(l: List[Int]): List[Int] = {
    drop(l, 1)
  }

  def dropWhile(l: List[Int])(pred: Int => Boolean): List[Int] = {
    l match {
      case Cons(h, tail) if pred(h) => dropWhile(tail)(pred)
      case _ => l
    }
  }

  def setHead(l: List[Int], new_head: Int): Unit = {
    l match {
      case Cons(_, tail) => Cons(new_head, tail)
      case _ => throw new Error("set head on an empty List")
    }
  }

  def init(l: List[Int]): List[Int] = {
    l match {
      case Cons(h, Nil) => Nil
      case Cons(h, tail) => Cons(h, init(tail))
      case Nil => throw new Error("init of an empty list")
    }
  }

  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B = {
    l match {
      case Nil => z
      case Cons(head, tail) => f(head, foldRight(tail, z)(f))
    }
  }

  def length(l: List[Int]): Int = {
    foldRight(l, 0)((_, z) => z + 1)
  }


  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
  }

  def sumViaFoldLeft(l: List[Int]): Int = {
    foldLeft(l, 0)((b, a) => a + b)
  }

  def productViaFoldLeft(l: List[Int]): Int = {
    foldLeft(l, 1)((b, a) => a * b)
  }

  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, Nil)((b, a) => Cons[A](a, b))
  }

  def append[A](l1: List[A], l2: List[A]): List[A] = {
    foldLeft(l1, l2)((b, a) => Cons[A](a, b))
  }

  def concatenate[A](l: List[List[A]]): List[A] = {
    foldRight(l, Nil: List[A])(append)
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = {
    foldRight(l, Nil: List[B])((head, tail) => Cons(f(head), tail))
  }

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = {
    concatenate(map(l)(f))
  }

  def zip[A,B](l1: List[A], l2: List[B]): List[(A, B)] = {
    (l1, l2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), zip(t1, t2))
    }
  }

  def zipFoo[A,B,C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = {
    map(zip(l1, l2))((a) => f(a._1, a._2))
  }

  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean =
    (l, sub) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => hasSubsequence(t1, t2)
      case (Cons(h1, t1), Cons(h2, t2)) => hasSubsequence(t1, sub)
    }

  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = {
    (l, prefix) match {
      case (_, Nil) => true
      case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => startsWith(t1, t2)
      case _ => false
    }
  }

  @annotation.tailrec
  def hasSubsequence2[A](l: List[A], sub: List[A]): Boolean = l match {
    case Nil => sub == Nil
    case _ if startsWith(l, sub) => true
    case Cons(h, tail) => hasSubsequence(tail, sub)
  }
}
