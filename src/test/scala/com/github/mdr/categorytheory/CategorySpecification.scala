package com.github.mdr.categorytheory

import com.github.mdr.categoryTheory._
import com.github.mdr.categoryTheory.Categories._
import org.scalacheck._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary.arbitrary

object CategorySpecification extends Properties("Category") {

  implicit val category = new SetCategory[Int](1)

  implicit def arbitraryFinSet[T: Arbitrary]: Arbitrary[FinSet[T]] = Arbitrary(arbitrary[Set[T]].map(x ⇒ x: FinSet[T]))

  case class CompatibleTypeFns(f: TypedFn[Int], g: TypedFn[Int], h: TypedFn[Int]) {
    require { cod(h) == dom(g) && cod(g) == dom(f) }
  }

  def makeTypedFn[T: Arbitrary](dom: FinSet[T], cod: FinSet[T]): Gen[TypedFn[T]] =
    for {
      rangeValues ← Gen.listOfN(dom.size, Gen.oneOf(cod.toList))
      fun = dom.toList zip rangeValues toMap
    } yield TypedFn[T](dom, cod)(fun)

  implicit def typedFunGen[T: Arbitrary]: Arbitrary[TypedFn[T]] = Arbitrary {
    for {
      dom ← arbitrary[FinSet[T]]
      cod ← arbitrary[FinSet[T]]
      fn ← makeTypedFn(dom, cod)
    } yield fn
  }

  property("right identity") = forAll { (f: TypedFn[Int]) ⇒
    f ∘ dom(f).id == f
  }

  property("left identity") = forAll { (f: TypedFn[Int]) ⇒
    cod(f).id ∘ f == f
  }

  {
    implicit val arbitraryCompatibleTypeFns: Arbitrary[CompatibleTypeFns] = Arbitrary {
      for {
        a ← arbitrary[FinSet[Int]]
        b ← arbitrary[FinSet[Int]]
        c ← arbitrary[FinSet[Int]]
        d ← arbitrary[FinSet[Int]]
        f ← makeTypedFn(c, d)
        g ← makeTypedFn(b, c)
        h ← makeTypedFn(a, b)
      } yield CompatibleTypeFns(f, g, h)
    }

    property("associativity") = forAll { (compatibleTypeFns: CompatibleTypeFns) ⇒
      import compatibleTypeFns._
      (f ∘ g) ∘ h == f ∘ (g ∘ h)
    }
  }

}