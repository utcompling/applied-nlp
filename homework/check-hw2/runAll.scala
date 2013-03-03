val studentDirs = new java.io.File(".").listFiles.map(_.toString).filter(_.endsWith("hw2")).sorted

studentDirs.foreach { dir =>
  println("*** " + dir + " ***")
  val command = List("scala",dir+"/"+args(0)) ::: args.drop(1).toList
  scala.sys.process.stringSeqToProcess(command).lines.foreach(println)
  println()
}
