package com.github.mdr.categoryTheory

import Categories._
import scala.collection.IterableLike
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Builder
import scala.collection.generic.CanBuildFrom

object AnySetCategory extends SetCategory[Any](1) with CategoryWithProducts[FinSet[Any], TypedFn[Any]] {

  def product(o1: FinSet[Any], o2: FinSet[Any]) = {
    val p = for (x1 ← o1; x2 ← o2) yield (x1, x2)
    (TypedFn(p, o1) { case (x, y) ⇒ x }, p, TypedFn(p, o2) { case (x, y) ⇒ y })
  }

  def pair(f: TypedFn[Any], g: TypedFn[Any]): TypedFn[Any] = {
    require(f.domain == g.domain)
    val a = f.domain
    val (π1, p, π2) = product(f.codomain, g.codomain)
    TypedFn(f.domain, p) { x ⇒ (f(x), g(x)) }
  }

}

class SetCategory[T](element: T)
    extends Category[FinSet[T], TypedFn[T]]
    with CategoryWithInitialObject[FinSet[T], TypedFn[T]]
    with CategoryWithTerminalObject[FinSet[T], TypedFn[T]] {

  /**
   * Do a2, then a1.
   */
  def compose(a1: TypedFn[T], a2: TypedFn[T]) =
    if (a1.domain == a2.codomain)
      TypedFn(a2.domain, a1.codomain)(a1.map compose a2.map)
    else
      throw new IllegalArgumentException("Cannot compose " + a1 + " with " + a2 + ": incompatible types: " + a1.domain + " != " + a2.codomain)

  def identity(o: FinSet[T]) = TypedFn(o, o)(Predef.identity)

  def source(a: TypedFn[T]) = a.domain

  def target(a: TypedFn[T]) = a.codomain

  def initialObject = FinSet()

  def getMorphismFromInitialObject(o: FinSet[T]): TypedFn[T] = TypedFn(initialObject, o)(Predef.identity)

  def terminalObject = FinSet(element)

  def getMorphismToTerminalObject(o: FinSet[T]): TypedFn[T] = TypedFn(o, terminalObject)(x ⇒ element)

}

object FinSet {

  def apply[T](elements: T*) = new FinSet[T](Set(elements: _*))

  def underlying[T](finSet: FinSet[T]): Set[T] = finSet.getSet.asInstanceOf[Set[T]]

  implicit def canBuildFrom[T]: CanBuildFrom[FinSet[_], T, FinSet[T]] =
    new CanBuildFrom[FinSet[_], T, FinSet[T]] {
      def apply(): Builder[T, FinSet[T]] = newBuilder
      def apply(from: FinSet[_]): Builder[T, FinSet[T]] = newBuilder
    }

  implicit def set2FinSet[T](set: Set[T]): FinSet[T] = new FinSet(set)

  def newBuilder[T]: Builder[T, FinSet[T]] = new ArrayBuffer[T] mapResult { x ⇒ new FinSet[T](x.toSet) }

}

class FinSet[+T] private (set: Set[_]) extends Iterable[T] with IterableLike[T, FinSet[T]] {

  private def getSet = set

  def iterator: Iterator[T] = set.iterator.asInstanceOf[Iterator[T]]

  override protected[this] def newBuilder = FinSet.newBuilder

  override def equals(other: Any) = other match {
    case otherSet: FinSet[T] ⇒ otherSet.getSet == set
    case _                   ⇒ false
  }

  override lazy val hashCode = set.##

  override def toString = getClass.getSimpleName + "(" + set.mkString(", ") + ")"

}

object TypedFn {

  def apply[T](domain: FinSet[T], codomain: FinSet[T])(f: T ⇒ T) = new TypedFn(domain, codomain, domain.map(d ⇒ (d, f(d))).toMap)

}

class TypedFn[+T](val domain: FinSet[T], val codomain: FinSet[T], val map: Map[Any, T]) {

  def apply(t: Any): T =
    if (FinSet.underlying(domain) contains t.asInstanceOf[T])
      map.apply(t.asInstanceOf[T])
    else
      throw new IllegalArgumentException(t + " is not in the domain " + domain)

  override def equals(other: Any) = other match {
    case otherFn: TypedFn[T] ⇒ equal(otherFn)
    case _                   ⇒ false
  }

  private def equal(otherFn: TypedFn[_]) =
    domain == otherFn.domain && codomain == otherFn.codomain && domain.forall(t ⇒ this(t) == otherFn(t))

  override lazy val hashCode = domain.## + codomain.##

  override lazy val toString = getClass.getSimpleName + "(" + domain + " => " + codomain + ": " + domain.map(d ⇒ d + " -> " + map(d)).mkString(", ")

}

object CartesianFunctor extends Functor[FinSet[Int], TypedFn[Int], FinSet[(Int, Int)], TypedFn[(Int, Int)]] {

  def apply(s: FinSet[Int]): FinSet[(Int, Int)] = for (x1 ← s; x2 ← s) yield (x1, x2)

  def fmap(tf: TypedFn[Int]): TypedFn[(Int, Int)] =
    TypedFn(this(tf.domain), this(tf.codomain)) { case (x1, x2) ⇒ (tf(x1), tf(x2)) }
}