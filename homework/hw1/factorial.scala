// Get the argument from the command line and check that it is an
// integer.  (Don't change the next block of code.)
val IntegerPattern = """-?\d+""".r
if (args.length != 1 || !IntegerPattern.pattern.matcher(args(0)).matches) {
  println("Incorrect arguments to variables.scala. Please provide one integer.")
  System.exit(0)
}

////////////////////////////////////////////////////////////////////////
// Start your work here

// Obtain the number, converting it from a String to an Int.

// Check that the number is in the right range. If it isn't, print a
// warning message and exit.

// Compute the factorial using recursion.

// Print the result

