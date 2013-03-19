name := "applied-nlp"

version := "0.3.0"

organization := "edu.utexas"

scalaVersion := "2.10.1"

retrieveManaged := true

crossPaths := false

resolvers ++= Seq(
  "opennlp sourceforge repo" at "http://opennlp.sourceforge.net/maven2",
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)

libraryDependencies ++= Seq(
  "org.scalanlp" % "nak" % "1.1.1",
  "org.scalanlp" % "chalk" % "1.1.2",
  "org.rogach" %% "scallop" % "0.8.1",
  "org.clapper" % "argot_2.9.1" % "0.3.8",
  "gov.nist.math" % "jama" % "1.0.2" 
)
