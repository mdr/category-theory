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

  property("equalizers") = forAll(equalizerTestCases) {
    case (f: Morphism, g: Morphism) ⇒
      val e = category.equalizer(f, g)
      f ∘ e == g ∘ e
  }

}

object SetCategoryGenerators {

  type Morphism = TypedFn[Any]
  type Object = FinSet[Any]

  private implicit val category = AnySetCategory

  implicit val arbitraryObject: Arbitrary[Object] = Arbitrary(arbitrary[Set[Int]].map(x ⇒ x: FinSet[Int]))

  def arbitraryMorphism(dom: Object, cod: Object): Gen[Morphism] =
    for {
      rangeValues ← Gen.listOfN(dom.size, Gen.oneOf(cod.toList))
      fun = dom.toList zip rangeValues toMap
    } yield TypedFn[Any](dom, cod)(fun)

  implicit def arbitraryMorphism: Arbitrary[Morphism] = Arbitrary {
    for {
      dom ← arbitrary[Object]
      cod ← arbitrary[Object]
      fn ← arbitraryMorphism(dom, cod)
    } yield fn
  }

  case class ThreeComposableMorphisms(f: Morphism, g: Morphism, h: Morphism) {
    require { cod(h) == dom(g) && cod(g) == dom(f) }
  }

  implicit val arbitraryThreeComposableMorphisms: Arbitrary[ThreeComposableMorphisms] = Arbitrary {
    for {
      a ← arbitrary[Object]
      b ← arbitrary[Object]
      c ← arbitrary[Object]
      d ← arbitrary[Object]
      f ← arbitraryMorphism(c, d)
      g ← arbitraryMorphism(b, c)
      h ← arbitraryMorphism(a, b)
    } yield ThreeComposableMorphisms(f, g, h)
  }

  val productTestCases = for {
    o1 ← arbitrary[Object]
    o2 ← arbitrary[Object]
    o3 ← arbitrary[Object]
    f ← arbitraryMorphism(o1, o2)
    g ← arbitraryMorphism(o1, o3)
  } yield (f, g)

  val coproductTestCases = for {
    o1 ← arbitrary[Object]
    o2 ← arbitrary[Object]
    o3 ← arbitrary[Object]
    f ← arbitraryMorphism(o2, o1)
    g ← arbitraryMorphism(o3, o1)
  } yield (f, g)

  val equalizerTestCases = for {
    o1 ← arbitrary[Object]
    o2 ← arbitrary[Object]
    f ← arbitraryMorphism(o1, o2)
    g ← arbitraryMorphism(o1, o2)
  } yield (f, g)
}