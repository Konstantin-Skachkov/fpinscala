package fpinscala.exercises.laziness

import scala.annotation.tailrec
import LazyList.*

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toListRecursive: List[A] = this match
    case Empty => Nil
    case Cons(h, t) => h() :: t().toListRecursive

  def toList: List[A] =
    @tailrec
    def helper(input: LazyList[A], acc: List[A]): List[A] =
      input match
        case Empty => acc.reverse
        case Cons(h, t) => helper(t(), h() :: acc)
    helper(this, Nil)

  def toListNoReverseWithBuffer: List[A] = ???

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] = if n <= 0 then Empty else
    this match
      case Empty => Empty
      case Cons(h, t) => Cons(h, () => t().take(n-1))

  def drop(n: Int): LazyList[A] = if n <= 0 then this else
    this match
      case Empty => Empty
      case Cons(_, t) => t().drop(n-1)

  def takeWhile(p: A => Boolean): LazyList[A] =
    this match
      case Empty => Empty
      case Cons(h, t) => if p(h()) then Cons(h, () => t().takeWhile(p)) else Empty
  def takeWhileFoldRight(p: A => Boolean): LazyList[A] =
    foldRight(empty)((a, as) => if(p(a)) then cons(a, as) else empty)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a,b) => p(a) && b)

  def headOption: Option[A] = this match
    case Empty => None
    case Cons(h, t) => Some(h())
  def headOptionViaFoldRight: Option[A] =
    foldRight[Option[A]](None)((a,_) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): LazyList[B] =
    foldRight[LazyList[B]](empty)((a, bs) => cons(f(a), bs))

  def filter(p: A => Boolean): LazyList[A] =
    foldRight[LazyList[A]](Empty)((a, as) => if(p(a)) then cons(a, as) else as)

  def append[B >: A](as: => LazyList[B]): LazyList[B] =
    foldRight[LazyList[B]](as)(cons(_,_))

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    foldRight[LazyList[B]](empty)((a,bs)=> f(a).append(bs))

  def mapViaUnfold[B](f:A=>B): LazyList[B] =
    unfold(this):
      case Empty => None
      case Cons(h, t) => Some((f(h()), t()))      
  def takeViaUnfold(n:Int): LazyList[A] =
    unfold(this, n):
      // case (Cons(h, t), n) if n==1 => Some(h(), (empty, n-1))// why matters?
      // case (Cons(h, t), n) if n>1 => Some(h(), (t(), n-1))
      case (Cons(h, t), n) if n>0 => Some(h(), (t(), n-1))
      case _ => None

  def takeWhileViaUnfold(p: A=>Boolean): LazyList[A] =
    unfold(this):
      case Cons(h, t) if p(h()) => Some(h(), t()) //todo: h() twice?
      case _ => None
  def zipWithViaUnfold[B, C](that: LazyList[B], f: (A, B)=> C) =
    unfold(this, that):
      case (Cons(h1,t1),Cons(h2,t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _ => None
  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] =
    unfold(this, that):
      case (Cons(h1,t1),Cons(h2,t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Cons(h1,t1),Empty) => Some((Some(h1()), None), (t1(), empty))
      case (Empty,Cons(h2,t2)) => Some((None, Some(h2())), (empty, t2()))
      case _ => None

  def startsWith[B](that: LazyList[B]): Boolean =
    (this, that) match
      case (_, Empty) => true
      case (Empty, _) => false
      case (Cons(h1, t1), Cons(h2,t2)) => (h1() == h2()) && t1().startsWith(t2())
  def tails: LazyList[LazyList[A]] =
    unfold(this){ s => s match
      case Empty => None
      case Cons(h, t) => Some(s, t())
    }
  def scanRight[B](b: B)(f: (A, => B) => B) =
    unfold(this){ s => s match
      case Empty => None
      case Cons(h, t) => Some(???, ???)
    }

@main    
def testTails = println(LazyList(1,2,3).tails.map(_.toList).toList)

object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] = 
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty 
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = LazyList.cons(1, ones)

  def continually[A](a: A): LazyList[A] = LazyList.cons(a, continually(a))

  def from(n: Int): LazyList[Int] = LazyList.cons(n, from(n+1))

  lazy val fibs: LazyList[Int] =
    def go(a: Int, b: Int): LazyList[Int] =
      cons(a, go(b, a + b))
    go(0,1)

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(state) match
      case None => Empty
      case Some((a, s)) => cons(a, unfold(s)(f))

  lazy val fibsViaUnfold: LazyList[Int] =
    unfold[Int, (Int, Int)]((0,1)):
      case (a, b)=> Some( (a,(b,a+b)) )

  def fromViaUnfold(n: Int): LazyList[Int] =
    unfold[Int, Int](n):
      case i => Some(i, i+1)

  def continuallyViaUnfold[A](a: A): LazyList[A] =
    unfold(()):
      case _ => Some(a, ())

  lazy val onesViaUnfold: LazyList[Int] =
    continuallyViaUnfold(1)

@main
def test = {
  println(ones.takeViaUnfold(0).toList)
  println(ones.takeViaUnfold(1).toList)
  println(ones.takeViaUnfold(2).toList)
  println(ones.takeViaUnfold(0).toList)
  println(ones.takeViaUnfold(1).toList)
  println(ones.takeViaUnfold(2).toList)
}