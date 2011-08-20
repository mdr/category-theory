package com.github.mdr.categoryTheory

class DualCategory[O, A](cat: Category[O, A]) extends Category[O, A] {

  def compose(a1: A, a2: A): A = cat.compose(a2, a1)

  def identity(o: O): A = cat.identity(o)

  def source(a: A): O = cat.target(a)

  def target(a: A): O = cat.source(a)

}