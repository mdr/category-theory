name := "category-theory"

organization := "com.github.mdr"

version := "1.0"

scalaVersion := "2.9.1"

libraryDependencies += "org.scala-tools.testing" %% "scalacheck" % "1.9" % "test"

EclipseKeys.withSource := true

EclipseKeys.eclipseOutput := Some("bin")