import sbt._
import Keys._

object Common {

  lazy val commonSettings = Seq(
    organization := "co.s4n",
    scalaVersion := "2.12.7",
    fork := true,
    fork in test := true,
    libraryDependencies ++= Seq(
      "org.scalatest"         %% "scalatest"              % "3.0.5"     % Test,
      "org.scalacheck"        %% "scalacheck"             % "1.14.0"    % Test
    ),
    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding",
      "UTF-8",
      "-feature",
      "-language:existentials",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-unchecked",
      "-Xfatal-warnings",
      "-Xlint",
      "-Yno-adapted-args",
      "-Ypartial-unification",
      "-Ywarn-dead-code",
      "-Ywarn-numeric-widen",
      "-Ywarn-value-discard",
      "-Ywarn-unused-import"
    )
  )
}
