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

    def ×[A](o2: O)(implicit category: CategoryWithProducts[O, A]): (A, O, A)= category.product(o, o2)
    
  }


}
