package fpinscala.exercises.state

import scala.annotation.tailrec


trait RNG:
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.

object RNG:
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    (a, _)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng =>
      val (a, rng2) = s(rng)
      (f(a), rng2)

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (next, rng2) = rng.nextInt
    (next >> 1, rng2)
    // map(int)(_ >> 1)(rng)

  def double(rng: RNG): (Double, RNG) =
    val (i, rng2) = nonNegativeInt(rng)
    val d = i.toDouble / (Int.MaxValue.toDouble + 1)
    (d, rng2)
    // map(nonNegativeInt)(_.toDouble/(Int.MaxValue.toDouble + 1))(rng)

  def intDouble(rng: RNG): ((Int,Double), RNG) =
    val (nextInt, rng2) = rng.nextInt
    val (nextDouble, rng3) = double(rng2)
    ((nextInt, nextDouble), rng3)

  def doubleInt(rng: RNG): ((Double,Int), RNG) =
    val (intDoublePair, rng2) = intDouble(rng)
    (intDoublePair.swap, rng2)
    //map(intDouble)(_.swap)(rng)

  def double3(rng: RNG): ((Double,Double,Double), RNG) =
    val (nextDouble1, rng2) = RNG.double(rng)
    val (nextDouble2, rng3) = RNG.double(rng2)
    val (nextDouble3, rng4) = RNG.double(rng3)
    ((nextDouble1, nextDouble2, nextDouble3), rng4)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    @tailrec
    def helper(count: Int, acc: List[Int])(rng: RNG): (List[Int], RNG) = count match
      case n: Int if n >= 0 =>
        val (nextInt, rng2) = rng.nextInt
        helper(count - 1, nextInt :: acc)(rng2)
      case _ => (Nil, rng)
    helper(count, Nil)(rng)

  // def ints2(count: Int): Rand[List[Int]] =
  //   sequence(List.fill(count)(int))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng =>
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a,b), rng3)

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    rs.foldRight(unit[List[A]](Nil)){ (randA, randAs) =>
      map2[A, List[A], List[A]](randA, randAs)(_ :: _)
    }
  def intsViaSequence(count: Int) =
    sequence(List.fill(count)(int))

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] =
    rng =>
      val (a, rng2) = r(rng)
      f(a)(rng2)

  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] =
    flatMap(r)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))

opaque type State[S, +A] = S => (A, S)

object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def map[B](f: A => B): State[S, B] =

      flatMap(a => unit(f(a)))
    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      // s =>
      //   val (a, s2) = underlying(s)
      //   val (b, s3) = sb(s2)
      //   (f(a,b), s3)
      underlying.flatMap(a => sb.map(b => f(a,b)))  

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      s =>
        val (a, s2) = underlying(s)
        f(a)(s2)

  def unit[S, A](a: A): State[S, A] =
    s => (a, s)

  def sequence[S, A](actions: List[State[S, A]]): State[S, List[A]] =
    actions.foldRight(unit[S, List[A]](Nil)){(f, acc) => f.map2[List[A], List[A]](acc) (_ :: _)
  }
  def traverse[S, A, B](as: List[A])(f: A => State[S, B]): State[S, List[B]] =
    as.foldRight[State[S, List[B]]](unit[S, List[B]](Nil)): (a, sb) =>
      f(a).map2(sb)(_ :: _)

  def modify[S](f: S => S): State[S, Unit] =
    for
      s <- get // Gets the current state and assigns it to `s`.
      _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
    yield ()

  def get[S]: State[S, S] = s => (s, s)

  def set[S](s: S): State[S, Unit] = _ => ((), s)

  def apply[S, A](f: S => (A, S)): State[S, A] = f

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int):
  def results = (coins, candies)
object Candy:
  import State.*
  val initialState = Machine(false, 5, 0)
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      _ <- traverse(inputs): 
        case i => modify(update(i))
      res <- get  
    } yield {
      res.results
    }
  def update(i: Input): (Machine => Machine) =
    machine => ???

    
