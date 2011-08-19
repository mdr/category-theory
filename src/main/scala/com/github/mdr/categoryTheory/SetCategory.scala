package com.github.mdr.categoryTheory

import Categories._

object AnySetCategory extends SetCategory[Any](1) with CategoryWithProducts[Set[Any], TypedFn[Any]] {

  def product(o1: Set[Any], o2: Set[Any]) = {
    val p: Set[Any] = for (x1 ← o1; x2 ← o2) yield (x1, x2)
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
    extends Category[Set[T], TypedFn[T]]
    with CategoryWithInitialObject[Set[T], TypedFn[T]]
    with CategoryWithTerminalObject[Set[T], TypedFn[T]] {

  def compose(a1: TypedFn[T], a2: TypedFn[T]) =
    if (a1.domain == a2.codomain)
      TypedFn(a2.domain, a1.codomain)(a1.f compose a2.f)
    else
      throw new IllegalArgumentException("Cannot compose " + a1 + " with " + a2 + ": incompatible types")

  def identity(o: Set[T]) = TypedFn(o, o)(Predef.identity)

  def source(a: TypedFn[T]) = a.domain

  def target(a: TypedFn[T]) = a.codomain

  def initialObject = Set()

  def getMorphismFromInitialObject(o: Set[T]): TypedFn[T] = TypedFn(initialObject, o)(Predef.identity)

  def terminalObject = Set(element)

  def getMorphismToTerminalObject(o: Set[T]): TypedFn[T] = TypedFn(o, terminalObject)(x ⇒ element)

}

object TypedFn {

  def apply[T](domain: Set[T], codomain: Set[T])(f: T ⇒ T) = new TypedFn(domain, codomain)(f)

}

class TypedFn[T](val domain: Set[T], val codomain: Set[T])(val f: T ⇒ T) {

  def apply(t: T): T = f(t)

  override def equals(other: Any) = other match {
    case otherFn: TypedFn[T] ⇒ equal(otherFn)
    case _                   ⇒ false
  }

  def equal(otherFn: TypedFn[T]) =
    domain == otherFn.domain && codomain == otherFn.codomain && domain.forall(t ⇒ this(t) == otherFn(t))

  override lazy val hashCode = domain.## + codomain.##

  override lazy val toString = "TypedFn(" + domain + " => " + codomain + ": " + domain.map(d ⇒ d + " -> " + f(d)).mkString(", ")

}

object CartesianFunctorS extends Functor[Set[Int], TypedFn[Int], Set[(Int, Int)], TypedFn[(Int, Int)]] {

  def apply(s: Set[Int]): Set[(Int, Int)] = for (x1 ← s; x2 ← s) yield (x1, x2)

  def fmap(tf: TypedFn[Int]): TypedFn[(Int, Int)] =
    TypedFn(this(tf.domain), this(tf.codomain)) { case (x1, x2) ⇒ (tf(x1), tf(x2)) }
}