package com.github.mdr.categorytheory

import com.github.mdr.categoryTheory._
import com.github.mdr.categoryTheory.Categories._
import org.scalacheck._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary.arbitrary

object SetCategorySpecification extends Properties("Category of finite sets") {

  implicit val category = AnySetCategory
  import SetCategoryGenerators._

  property("left identity") = forAll { (f: Morphism) ⇒
    cod(f).id ∘ f == f
  }

  property("right identity") = forAll { (f: Morphism) ⇒
    f ∘ dom(f).id == f
  }

  property("associativity") = forAll { (morphisms: ThreeComposableMorphisms) ⇒
    val ThreeComposableMorphisms(f, g, h) = morphisms
    require { cod(h) == dom(g) && cod(g) == dom(f) }
    (f ∘ g) ∘ h == f ∘ (g ∘ h)
  }

  property("initial objects") = forAll { (o: Object) ⇒
    val u = category.getMorphismFromInitialObject(o)
    dom(u) == category.initialObject && cod(u) == o
  }

  property("terminal objects") = forAll { (o: Object) ⇒
    val u = category.getMorphismToTerminalObject(o)
    cod(u) == category.terminalObject && dom(u) == o
  }

  property("products") = forAll(productTestCases) {
    case (f: Morphism, g: Morphism) ⇒
      val Product(π1, p, π2) = cod(f) × cod(g)
      val u = category.getMediatingMorphismForProduct(f, g)
      π1 ∘ u == f && π2 ∘ u == g
  }

  property("coproducts") = forAll(coproductTestCases) {
    case (f: Morphism, g: Morphism) ⇒
      val Coproduct(i1, c, i2) = dom(f) ⊕ dom(g)
      val u = category.getMediatingMorphismForCoproduct(f, g)
      u ∘ i1 == f && u ∘ i2 == g
  }
}

object SetCategoryGenerators {

  type Morphism = TypedFn[Any]
  type Object = FinSet[Any]

  private implicit val category = AnySetCategory

  implicit val arbitraryAnyFinSet: Arbitrary[FinSet[Any]] = Arbitrary(arbitrary[Set[Int]].map(x ⇒ x: FinSet[Int]))

  case class ThreeComposableMorphisms(f: TypedFn[Any], g: TypedFn[Any], h: TypedFn[Any]) {
    require { cod(h) == dom(g) && cod(g) == dom(f) }
  }

  implicit val arbitraryThreeComposableMorphisms: Arbitrary[ThreeComposableMorphisms] = Arbitrary {
    for {
      a ← arbitrary[FinSet[Any]]
      b ← arbitrary[FinSet[Any]]
      c ← arbitrary[FinSet[Any]]
      d ← arbitrary[FinSet[Any]]
      f ← arbitraryTypedFn(c, d)
      g ← arbitraryTypedFn(b, c)
      h ← arbitraryTypedFn(a, b)
    } yield ThreeComposableMorphisms(f, g, h)
  }

  def arbitraryTypedFn(dom: FinSet[Any], cod: FinSet[Any]): Gen[TypedFn[Any]] =
    for {
      rangeValues ← Gen.listOfN(dom.size, Gen.oneOf(cod.toList))
      fun = dom.toList zip rangeValues toMap
    } yield TypedFn[Any](dom, cod)(fun)

  implicit def typedFunGen: Arbitrary[TypedFn[Any]] = Arbitrary {
    for {
      dom ← arbitrary[FinSet[Any]]
      cod ← arbitrary[FinSet[Any]]
      fn ← arbitraryTypedFn(dom, cod)
    } yield fn
  }

  val productTestCases = for {
    o1 ← arbitrary[Object]
    o2 ← arbitrary[Object]
    o3 ← arbitrary[Object]
    f ← arbitraryTypedFn(o1, o2)
    g ← arbitraryTypedFn(o1, o3)
  } yield (f, g)

  val coproductTestCases = for {
    o1 ← arbitrary[Object]
    o2 ← arbitrary[Object]
    o3 ← arbitrary[Object]
    f ← arbitraryTypedFn(o2, o1)
    g ← arbitraryTypedFn(o3, o1)
  } yield (f, g)

}