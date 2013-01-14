// Default values of room number assignments you will build on. Don't
// change the next line.
val defaultRoomNumbers = Map("Sam" -> "312", "Ted" -> "325", "Jane" -> "312")


// Check that there are an even number of command line
// arguments. Print a warning and exit if there aren't.
if (args.length % 2 != 0) {
  println("Please supply an even number of arguments.")
  System.exit(0)
}


// Add the command line information to defaultRoomNumbers to create
// the roomNumbers map.


// Print out the people and the room they are in, sorted
// alphabetically by name.
println("\nPart (a)")


// Create a new Map roomsToPeople that maps room numbers to lists of
// the people who are in them.


// For each room, print the list of who is in it.
println("\nPart (b)")


