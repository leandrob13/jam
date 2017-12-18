import Common._

lazy val `jam-parser` = project
  .settings(
    commonSettings,
    name := "jam-parser",
    version := "0.0.1",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "fastparse" % "1.0.0"
    )
  )

lazy val jam = (project in file(".")).aggregate(`jam-parser`)

scalafmtOnCompile in ThisBuild := true
