package fpinscala.exercises.datastructures

import scala.annotation.tailrec

/** `List` data type, parameterized on a type, `A`. */
enum List[+A]:
  /** A `List` data constructor representing the empty list. */
  case Nil
  /** Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
    which may be `Nil` or another `Cons`.
   */
  case Cons(head: A, tail: List[A])

object List: // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  def product(doubles: List[Double]): Double = doubles match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  @annotation.nowarn // Scala gives a hint here via a warning, so let's disable that
  val result = List(1,2,3,4,5) match
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))

  def foldRight[A,B](as: List[A], acc: B, f: (A, B) => B): B = // Utility functions
    as match
      case Nil => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumViaFoldRight(ns: List[Int]): Int =
    foldRight(ns, 0, (x,y) => x + y)

  def productViaFoldRight(ns: List[Double]): Double =
    foldRight(ns, 1.0, _ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] =
    l match
      case Nil => throw Exception("Nil.tail")
      case Cons(_, tail) => tail

  def setHead[A](l: List[A], h: A): List[A] =
    l match
      case Nil => throw Exception("Nil.setHead")
      case Cons(_, tail) => Cons(h, tail)

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else 
      l match
        case Nil => Nil
        case Cons(h, t) => drop(t, n-1)
      

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match
      case Cons(head, tail) if f(head) => dropWhile(tail, f)
      case _ => l
    
  def init[A](l: List[A]): List[A] =
    l match
      case Nil => throw Exception("Nil.init")
      case Cons(_, Nil) => Nil
      case Cons(head, tail) => Cons(head, init(tail)) //stack unsafe
    
  def length[A](l: List[A]): Int =
    @tailrec
    def helper(l: List[A], n: Int): Int =
      l match
        case Nil => n
        case Cons(_, tail) => helper(tail, n + 1)
    helper(l, 0)    

  @tailrec
  def foldLeft[A,B](l: List[A], acc: B, f: (B, A) => B): B =
    l match
      case Nil => acc
      case Cons(a, as) => foldLeft(as, f(acc, a), f)

  def sumViaFoldLeft(ns: List[Int]): Int =
    foldLeft(ns, 0, _ + _)

  def productViaFoldLeft(ns: List[Double]): Double =
    foldLeft(ns, 1, _ * _)

  def lengthViaFoldLeft[A](l: List[A]): Int =
    foldLeft(l, 0, (acc, _) => acc + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A](), (t, h) => Cons(h, t))

  @main def blah = println(reverse(List(1,2,3)))

  def foldRightViaFoldLeft[A, B](l: List[A], acc: B, f: (A, B) => B): B =
    ???
  def foldRightViaFoldLeft_1[A, B](l: List[A], acc: B, f: (A, B) => B): B =
    ???
  def foldLeftViaFoldRight[A, B](l: List[A], acc: B, f: (B,A) => B): B =
    ???

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r, Cons(_, _))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, List[A](), append(_, _))

  def incrementEach(l: List[Int]): List[Int] =
    foldRight(l, Nil, (a, acc: List[Int]) => Cons(a + 1, acc))

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil, (a, acc: List[String]) => Cons(a.toString, acc))

  def map[A,B](l: List[A], f: A => B): List[B] =
    foldRight(l, Nil, (a, bs: List[B]) => Cons(f(a), bs))

  def filter[A](as: List[A], f: A => Boolean): List[A] =
    foldRight(as, Nil, (a, as: List[A]) => if f(a) then Cons(a, as) else as)

  def flatMap[A,B](as: List[A], f: A => List[B]): List[B] =
    concat(map(as, f))

  def flatMapViaFoldRight[A,B](as: List[A], f: A => List[B]): List[B] =
    foldRight(as, List[B](), (a, b) => appendViaFoldRight(f(a),b))

  def filterViaFlatMap[A](as: List[A], f: A => Boolean): List[A] =
    def toSingleton(a: A): List[A] = if f(a) then List(a) else Nil
    flatMap(as, toSingleton)

  def addPairwise(as: List[Int], bs: List[Int]): List[Int] =
    (as, bs) match
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1,t2))
      case _ => Nil

  def zip[A, B](as: List[A], bs: List[B]): List[(A, B)] =
    zipWith(as, bs, (_,_))
  @main def testZip = println(zip(List(1,2,3),List(1,2)))

  def zipWith[A, B,C](as: List[A], bs: List[B], f: (A, B) => C): List[C] =
    @tailrec
    def helper(as: List[A], bs: List[B], acc: List[C]):List[C] =
      (as, bs) match
        case (Cons(h1,t1),Cons(h2,t2)) => helper(t1,t2,Cons(f(h1,h2),acc))
        case _ => acc
    reverse(helper(as,bs,Nil))
   
  def tails[A](as: List[A]): List[List[A]] =
    @tailrec
    def helper(as: List[A], acc: List[List[A]]): List[List[A]] =
      as match
        case Nil => acc
        case Cons(_,t) => helper(t, Cons(as, acc))
    reverse(helper(as,Nil))
  @main def testTails =
    println(tails(List(1, 2, 3)))

  @tailrec
  def startsWith[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match
    case (Cons(h1,t1), Cons(h2,t2)) if h1==h2 => startsWith(t1,t2)
    case (_, Nil) => true
    case _ => false

  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match
    case Nil => sub == Nil
    case _ if startsWith(sup,sub) => true
    case Cons(_,t) => hasSubsequence(t,sub)
    