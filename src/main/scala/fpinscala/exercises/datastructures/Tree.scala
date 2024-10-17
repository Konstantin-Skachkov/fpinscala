package fpinscala.exercises.datastructures

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(_) => 1
    case Branch(l, r) => 1 + l.size + r.size

  def depth: Int = this match
    case Leaf(value) => 1
    case Branch(left, right) => 1 + Math.max(left.depth, right.depth)

  def map[B](f: A => B): Tree[B] = this match
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(left.map(f), right.map(f))

  def fold[B](f: A => B, g: (B,B) => B): B = this match
    case Leaf(value) => f(value)
    case Branch(left, right) => g(left.fold(f,g), right.fold(f,g))
  
  def sizeViaFold: Int = fold(_ => 1, 1 + _ + _)
  
  def depthViaFold: Int = fold(_ => 1, Math.max)
  
  def mapViaFold[B](f: A => B): Tree[B] = fold(a => Leaf(f(a)), Branch(_, _))
  
import Tree.*

@main def mapViaFoldTest = 
  println(Branch(Leaf(1),Branch(Leaf(2),Leaf(3))).mapViaFold[Int](_+1) )

object Tree:

  def size[A](t: Tree[A]): Int = t match
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)

  extension (t: Tree[Int]) def firstPositive: Int =
    t.fold(x => x, (x, y) => if x>0 then x else y)

  extension (t: Tree[Int]) def maximum: Int =
    t match
      case Leaf(value) => value
      case Branch(left, right) => left.maximum.max(right.maximum)

  extension (t: Tree[Int]) def maximumViaFold: Int =
    t.fold(x => x, Math.max)
