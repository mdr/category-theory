package com.github.mdr.categoryTheory

import scala.PartialFunction.cond

object AnyGraphCategory
  extends GraphCategory[Any](1)

class GraphCategory[T](element: T)
    extends Category[Graph[T], GraphHom[T]]
    with CategoryWithInitialObject[Graph[T], GraphHom[T]]
    with CategoryWithTerminalObject[Graph[T], GraphHom[T]] {

  def compose(a1: GraphHom[T], a2: GraphHom[T]) =
    if (a1.domain == a2.codomain)
      GraphHom(a2.domain, a1.codomain)(a1.map compose a2.map)
    else
      throw new IllegalArgumentException("Cannot compose " + a1 + " with " + a2 + ": incompatible types: " + a1.domain + " != " + a2.codomain)

  def identity(o: Graph[T]) = GraphHom(o, o)(Predef.identity)

  def source(a: GraphHom[T]) = a.domain

  def target(a: GraphHom[T]) = a.codomain

  def initialObject = Graph(FinSet(), FinSet())

  def getMorphismFromInitialObject(o: Graph[T]): GraphHom[T] = GraphHom(initialObject, o)(Predef.identity)

  def terminalObject = Graph(FinSet(element), FinSet((element, element)))

  def getMorphismToTerminalObject(o: Graph[T]): GraphHom[T] = GraphHom(o, terminalObject)(x ⇒ element)

}

object Graph {

  def apply[T](vertices: FinSet[T], edges: FinSet[(T, T)]) = new Graph(vertices, edges)

}

class Graph[+T] private (val vertices: FinSet[T], val edges: FinSet[(T, T)]) {

  override def equals(other: Any) = cond(other) {
    case otherGraph: Graph[T] ⇒ otherGraph.vertices == vertices && otherGraph.edges == edges
  }

  override lazy val hashCode = vertices.## + edges.##

}

object GraphHom {

  def apply[T](domain: Graph[T], codomain: Graph[T])(f: T ⇒ T) =
    new GraphHom(domain, codomain, domain.vertices.map(d ⇒ (d, f(d))).toMap)

}

class GraphHom[+T](val domain: Graph[T], val codomain: Graph[T], val map: Map[Any, T]) {

  require(domain.edges.forall {
    case (s, t) ⇒ FinSet.underlying(codomain.edges) contains (map(s), map(t))
  })

  def apply(t: Any): T =
    if (FinSet.underlying(domain.vertices) contains t.asInstanceOf[T])
      map.apply(t.asInstanceOf[T])
    else
      throw new IllegalArgumentException(t + " is not in the domain " + domain)

  override def equals(other: Any) = other match {
    case otherFn: GraphHom[T] ⇒ equal(otherFn)
    case _                    ⇒ false
  }

  private def equal(otherFn: GraphHom[_]) =
    domain == otherFn.domain && codomain == otherFn.codomain && domain.vertices.forall(t ⇒ this(t) == otherFn(t))

  override lazy val hashCode = domain.## + codomain.##

  override lazy val toString = getClass.getSimpleName + "(" + domain + " => " + codomain + ": " + domain.vertices.map(d ⇒ d + " -> " + map(d)).mkString(", ")

}

