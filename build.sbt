import Common._

lazy val `jam-parser` = project
  .settings(
    commonSettings,
    name := "jam-parser",
    libraryDependencies ++= Seq(
      "com.lihaoyi"    %% "fastparse"    % "2.1.0",
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "com.propensive" %% "magnolia"     % "0.10.0" withSources ()
    )
  )

lazy val `jam-swagger` = project
  .settings(
    commonSettings,
    name := "jam-swagger"
  )
  .dependsOn(`jam-parser`)

lazy val jam = (project in file(".")).aggregate(`jam-parser`, `jam-swagger`)

scalafmtOnCompile in ThisBuild := true

addCommandAlias("build", ";clean;coverage;test;coverageAggregate;coverageReport")
