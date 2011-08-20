package com.github.mdr.categorytheory

import com.github.mdr.categoryTheory._
import com.github.mdr.categoryTheory.Categories._
import org.scalacheck._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary.arbitrary

object CategorySpecification extends Properties("Category") {

  implicit val category = AnySetCategory

  implicit val typedFunGen = for {
    dom ← arbitrary[Set[Int]]
    cod ← arbitrary[Set[Int]]
  } yield TypedFn[Any](dom.asInstanceOf[Set[Any]], cod.asInstanceOf[Set[Any]]) { Predef.identity }

  property("right identity") = forAll(typedFunGen)((f: TypedFn[Any]) ⇒ f ∘ dom(f).id == f)
  property("left identity") = forAll(typedFunGen)((f: TypedFn[Any]) ⇒ cod(f).id ∘ f == f)

  property("startsWith") = forAll((a: String, b: String) ⇒ (a + b).startsWith(a))
  //
  //  property("endsWith") = Prop.forAll((a: String, b: String) ⇒ (a + b).endsWith(b))
  //
  //  // Is this really always true?
  //  property("concat") = Prop.forAll((a: String, b: String) ⇒
  //    (a + b).length > a.length && (a + b).length > b.length)
  //
  //  property("substring") = Prop.forAll((a: String, b: String) ⇒
  //    (a + b).substring(a.length) == b)
  //
  //  property("substring") = Prop.forAll((a: String, b: String, c: String) ⇒
  //    (a + b + c).substring(a.length, a.length + b.length) == b)
  //
}