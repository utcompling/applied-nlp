// Variables exercise

// Get the arguments from the command line and check there are two of
// them and that they are all integers. (Don't change the next block
// of code.)
val IntegerPattern = """-?\d+""".r
if (args.length != 2 || args.exists(arg => !IntegerPattern.pattern.matcher(arg).matches)) {
  println("Incorrect arguments to variables.scala. Please provide two integers.")
  System.exit(0)
}

////////////////////////////////////////////////////////////////////////
// Start your work here

// Obtain the numbers, converting them from Strings to Ints while
// doing so. 


// Add the numbers and print the result.


// Multiply the numbers and print the result.


// Compare the two numbers and set the "smaller" and "larger"
// variables appropriately.


// Print out which number is smaller and which is larger.


// Calculate the value of adding the two numbers and multiplying the
// result by the smaller number.

