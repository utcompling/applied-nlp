name := "applied-nlp"

version := "0.2.0"

organization := "edu.utexas"

scalaVersion := "2.10.0"

retrieveManaged := true

crossPaths := false

resolvers ++= Seq(
   "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/releases/"
)

libraryDependencies ++= Seq(
  "org.scalanlp" % "nak" % "1.1.0",
  "org.scalanlp" % "chalk" % "1.1.1",
  "org.rogach" %% "scallop" % "0.8.0",
  "gov.nist.math" % "jama" % "1.0.2"
)
