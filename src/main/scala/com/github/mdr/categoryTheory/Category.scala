package com.github.mdr.categoryTheory

object Categories {

  def cod[O, A](a: A)(implicit category: Category[O, A]) = a.cod
  def dom[O, A](a: A)(implicit category: Category[O, A]) = a.dom

  implicit def arrow2RichArrow[A](a: A): RichArrow[A] = new RichArrow[A](a)

  class RichArrow[A](a: A) {

    def ∘[O](a2: A)(implicit category: Category[O, A]): A = compose(a2)

    def compose[O](a2: A)(implicit category: Category[O, A]): A = category.compose(a, a2)

    def dom[O](implicit category: Category[O, A]): O = category.source(a)

    def cod[O](implicit category: Category[O, A]): O = category.target(a)

  }

  implicit def object2RichObject[O](o: O): RichObject[O] = new RichObject[O](o)

  class RichObject[O](o: O) {

    def identity[A](implicit category: Category[O, A]): A = category.identity(o)

    def id[A](implicit category: Category[O, A]): A = identity

  }

}

trait Category[O, A] {

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


class OppositeCategory[O, A](cat: Category[O, A]) extends Category[O, A] {

  def compose(a1: A, a2: A): A = cat.compose(a2, a1)

  def identity(o: O): A = cat.identity(o)

  def source(a: A): O = cat.target(a)

  def target(a: A): O = cat.source(a)

}

trait Functor[O1, A1, O2, A2] {

  def apply(o1: O1): O2

  def fmap(a1: A1): A2

}
trait EndoFunctor[O, A] extends Functor[O, A, O, A]
//
//class DualFunctor[O1, A1, O2, A2] extends Functor[O1, A1, O2, A2] {
