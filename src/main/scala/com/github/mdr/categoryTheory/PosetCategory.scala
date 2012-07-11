package com.github.mdr.categoryTheory

import com.github.mdr.categoryTheory.Categories._

trait PartiallyOrderedSet[-T] {

  def ≤(a1: T, a2: T): Boolean

}

trait Lattice[T] extends PartiallyOrderedSet[T] {

  def meet(a1: T, a2: T): T

  def ∧(a1: T, a2: T): T = meet(a1, a2)

  def join(a1: T, a2: T): T

  def ∨(a1: T, a2: T): T = join(a1, a2)

}

trait BoundedLattice[T] extends Lattice[T] {

  val top: T

  def ⊤ : T = top

  val bottom: T

  def ⊥ : T = bottom
}

case class TypedArrow[T](source: T, target: T)

class PosetCategory[T](poset: PartiallyOrderedSet[T]) extends Category[T, TypedArrow[T]] {

  def compose(a1: TypedArrow[T], a2: TypedArrow[T]): TypedArrow[T] = TypedArrow(a1.source, a2.target)

  def identity(o: T): TypedArrow[T] = TypedArrow(o, o)

  def source(a: TypedArrow[T]): T = a.source

  def target(a: TypedArrow[T]): T = a.target

}

class LatticeCategory[T](lattice: Lattice[T])
    extends PosetCategory[T](lattice)
    with CategoryWithProducts[T, TypedArrow[T]]
    with CategoryWithCoproducts[T, TypedArrow[T]] {

  private implicit val cat = this

  def product(o1: T, o2: T): Product[T, TypedArrow[T]] = {
    val p: T = lattice.meet(o1, o2)
    Product(TypedArrow(p, o1), p, TypedArrow(p, o2))
  }

  def getMediatingMorphismForProduct(f: TypedArrow[T], g: TypedArrow[T]): TypedArrow[T] = {
    val a = dom(f).ensuring(_ == dom(g))
    val Product(π1, p, π2) = cod(f) × cod(g)
    val u = TypedArrow(a, p)
    require(π1 ∘ u == f)
    require(π2 ∘ u == g)
    u
  }

  def coproduct(o1: T, o2: T): Coproduct[T, TypedArrow[T]] = {
    val c: T = lattice.join(o1, o2)
    Coproduct(TypedArrow(o1, c), c, TypedArrow(o2, c))
  }

  def getMediatingMorphismForCoproduct(f: TypedArrow[T], g: TypedArrow[T]): TypedArrow[T] = {
    val a = cod(f).ensuring(_ == cod(g))
    val Coproduct(i1, c, i2) = dom(f) ⊕ dom(g)
    val u = TypedArrow(c, a)
    require(u ∘ i1 == f)
    require(u ∘ i2 == g)
    u
  }

}

class BoundedLatticeCategory[T](boundedLattice: BoundedLattice[T])
    extends LatticeCategory[T](boundedLattice)
    with CategoryWithInitialObject[T, TypedArrow[T]]
    with CategoryWithTerminalObject[T, TypedArrow[T]] {

  def initialObject: T = boundedLattice.⊥

  def getMorphismFromInitialObject(o: T): TypedArrow[T] = TypedArrow(initialObject, o)

  def terminalObject: T = boundedLattice.⊤

  def getMorphismToTerminalObject(o: T): TypedArrow[T] = TypedArrow(o, terminalObject)

}