name := "calculator"
scalaVersion := "3.7.2"
scalacOptions ++= Seq("-deprecation", "-feature", "-Xfatal-warnings")
run / fork := true
run / connectInput := true
run / outputStrategy := Some(StdoutOutput)
Global / cancelable := true

libraryDependencies += "com.lihaoyi" %% "cask" % "0.10.2"
libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test
