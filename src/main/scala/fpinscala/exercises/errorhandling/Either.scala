package fpinscala.exercises.errorhandling

// Hide std library `Either` since we are writing our own in this chapter
import scala.{Either as _, Left as _, Right as _}
import scala.util.control.NonFatal

enum Either[+E,+A]:
  case Left(get: E)
  case Right(get: A)

  def map[B](f: A => B): Either[E, B] = this match
    case Left(error) => Left(error)
    case Right(value) => Right(f(value))

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match
    case Left(error) => Left(error)
    case Right(aVal) => f(aVal)

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match
    case Left(e) => b
    case right => right

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (this, b) match
    case (Right(a), Right(b)) => Right(f(a,b))
    case (Left(e), _) => Left(e)
    case (Right(_), Left(e)) => Left(e)

object Either:
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es.foldRight[Either[E, List[B]]](Right(Nil)) { case (a,bsE)=>
      f(a).map2(bsE)(_ :: _)
    }

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] =
    traverse(es)(identity[Either[E,A]])

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if xs.isEmpty then
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Throwable, Int] = 
    try Right(x / y)
    catch case NonFatal(t) => Left(t)

  def catchNonFatal[A](a: => A): Either[Throwable, A] =
    try Right(a)
    catch case NonFatal(t) => Left(t)

  def map2All[E, A, B, C](a: Either[List[E], A], b: Either[List[E], B], f: (A, B) => C): Either[List[E], C] =
    (a,b) match
      case (Right(a), Right(b)) => Right(f(a,b))
      case (Left(l), Right(_)) => Left(l)
      case (Right(_), Left(l)) => Left(l)
      case (Left(l1), Left(l2)) => Left(l1 ++ l2)

  def traverseAll[E, A, B](as: List[A], f: A => Either[List[E], B]): Either[List[E], List[B]] =
    as.foldRight[Either[List[E], List[B]]](Right(Nil)){ (a,bsE) =>
      map2All(f(a), bsE, _ :: _)
    }

  def sequenceAll[E, A](as: List[Either[List[E], A]]): Either[List[E], List[A]] = traverseAll(as, identity)
