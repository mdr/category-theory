package com.github.mdr.categoryTheory

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

object CartesianFunctor extends Functor[Set[Int], TypedFn[Int], Set[(Int, Int)], TypedFn[(Int, Int)]] {

  def apply(s: Set[Int]): Set[(Int, Int)] = for (x1 ← s; x2 ← s) yield (x1, x2)

  def fmap(tf: TypedFn[Int]): TypedFn[(Int, Int)] =
    TypedFn(this(tf.domain), this(tf.codomain)) { case (x1, x2) ⇒ (tf(x1), tf(x2)) }
}