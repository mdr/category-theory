package com.github.mdr.categoryTheory

trait Category[O, A] {

  /**
   * Do a2, then a1.
   */
  def compose(a1: A, a2: A): A

  def identity(o: O): A

  def source(a: A): O

  def target(a: A): O

}

trait CategoryWithInitialObject[O, A] extends Category[O, A] {

  def initialObject: O

  def getMorphismFromInitialObject(o: O): A

}

trait CategoryWithTerminalObject[O, A] extends Category[O, A] {

  def terminalObject: O

  def getMorphismToTerminalObject(o: O): A

}

case class Product[O, A](π1: A, p: O, π2: A)

trait CategoryWithProducts[O, A] extends Category[O, A] {

  def product(o1: O, o2: O): Product[O, A]

  /**
   * requires dom(f) == dom(g)
   * @return an arrow from dom(f) to cod(f) × cod(g)
   */
  def getMediatingMorphismForProduct(f: A, g: A): A

}

case class Coproduct[O, A](π1: A, p: O, π2: A)

trait CategoryWithCoproducts[O, A] extends Category[O, A] {

  def coproduct(o1: O, o2: O): Coproduct[O, A]

  /**
   * requires cod(f) == cod(g)
   * @return an arrow from dom(f) + dom(g) to cod(f)
   */
  def getMediatingMorphismForCoproduct(f: A, g: A): A

}

trait CategoryWithEqualizers[O, A] extends Category[O, A] {

  /**
   * requires dom(f) == dom(g) and cod(f) == cod(g)
   */
  def equalizer(f: A, g: A): A

  def getMorphismToEqualizer(e: A, equaliser: A): A

}

trait Functor[O1, A1, O2, A2] {

  def apply(o1: O1): O2

  def fmap(a1: A1): A2

}
trait EndoFunctor[O, A] extends Functor[O, A, O, A]
//
//class DualFunctor[O1, A1, O2, A2] extends Functor[O1, A1, O2, A2] {
