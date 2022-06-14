name := "royal-mail-tracking-validator-scala"

version := "0.0.1"

scalaVersion := "2.13.8"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.3.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.9" % "test"

// sbt-scoverage config
coverageFailOnMinimum := true
coverageMinimumStmtTotal := 80
coverageMinimumBranchTotal := 90
