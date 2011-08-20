package com.github.mdr.categoryTheory

import Categories._

object Tests {

  implicit val category = new SetCategory[Int](1)
  //  implicit val category = new OppositeCategory(new SetCategory[Int])
  //
  val s = Set(1, 2, 3)

  val f = new TypedFn(s, s)(_ match {
    case 1 ⇒ 2
    case 2 ⇒ 3
    case 3 ⇒ 1
  })

  val m4 = category.getMorphismFromInitialObject(s)
  val m3 = category.getMorphismToTerminalObject(s)

  val terminalObject2 = Set(42)

  val i = category.getMorphismToTerminalObject(terminalObject2)
  println(i)
  val j = TypedFn(category.terminalObject, terminalObject2)(x ⇒ terminalObject2.head)
  println(j)

  val iso1 = i ∘ j
  println(iso1, category.terminalObject.id)
  val iso2 = j ∘ i
  println(iso2, terminalObject2.id)

  val f2 = f ∘ f
  val f3 = f ∘ f ∘ f

}

object Test2 extends App {

  {
    implicit val category = AnySetCategory

    val s: Set[Any] = Set(1, 2, 3)
    val (π1, p, π2) = s × s
    println(s)
    println(p)
    val d: Set[Any] = Set(1, 2, 3, 4, 5)
    val f = TypedFn(d, s) { case x: Int ⇒ ((x + 1) % 3) + 1 }
    val g = TypedFn(d, s) { case x: Int ⇒ ((x * 2) % 3) + 1 }
    val pair = category.pair(f, g)
  }

  {
    implicit val category = new DualCategory(AnySetCategory)

    val s: Set[Any] = Set(1, 2, 3)
    println(s)
    val d: Set[Any] = Set(1, 2, 3, 4, 5)
    val f = TypedFn(d, s) { case x: Int ⇒ ((x + 1) % 3) + 1 }
    val g = TypedFn(d, s) { case x: Int ⇒ ((x * 2) % 3) + 1 }
  }

}
