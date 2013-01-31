// These initial functions are used to test your code. Go down to the
// part that says "Do exercise 2.1..." and add your regular
// expressions for each problem.

import scala.util.matching.Regex

def matches (regex: Regex, input: String) = regex.pattern.matcher(input).matches

def check (regex: Regex, input: String, shouldMatch: Boolean = true) = {
  if (shouldMatch) {
    try {
      assert(matches(regex, input))
      println("Correct: " + regex + " matches " + input)
    } catch { 
      case e: AssertionError => println("ERROR!!! " + regex + " didn't match " + input)
    }
  } else {
    try {
      assert(!matches(regex, input))
      println("Correct: " + regex + " rejects " + input)
    } catch {
      case e: AssertionError => println("ERROR!!! " + regex + " didn't reject " + input)
    }
  }

}

def checkRegister (regex: Regex, input: String, value: String) = {
  try {
    val regex(x) = input
    assert(x==value)
    println("Correct: " + regex + " captured \"" + value + "\" from \"" + input + "\"")
  } catch {
    case e: AssertionError => println("ERROR!!! " + regex + " didn't store the value " + value)
    case m: MatchError => println("ERROR!!! " + regex + " didn't match " + input)
  }
}


// Do exercise 2.1 from Jurafsky and Martin, pages 42-43. Put your
// regular expressions for each problem in 2.1 in the triple quotes
// for the Regex variable E1, E2, ... E7 corresponding to the problem.

val E1 = """""".r

val E2 = """""".r

val E3 = """""".r

val E4 = """""".r

// For this one, assume that a "word" is a sequence of uppercase or lowercase letters
val E5 = """""".r

val E6 = """""".r

val E7 = """""".r



// Don't change the following lines! These will check the correctness
// of your regular expressions. Initially, these will complain, but
// don't worry about that -- just work through them one at a time
// until everything says "Correct" in the output.

check(E1, "hello")
check(E1, "world!", false)
check(E1, "CamelCaseVariable")
check(E1, "CamelCaseVariableWithNumbers1234", false)

check(E2, "b")
check(E2, "xyzb")
check(E2, "xyzbcz", false)

check(E3, "the the")
check(E3, "Humbert Humbert")
check(E3, "the bug", false)
check(E3, "the big bug", false)

check(E4, "b")
check(E4, "bab")
check(E4, "bbbabbbab")
check(E4, "bbaab", false)
check(E4, "abb", false)

check(E5, "221b")
check(E5, "3 is a magic number")
check(E5, "5E98#@@ ($)oeu")
check(E5, "hello", false)
check(E5, "x25", false)
check(E5, "2011", false)

check(E6, "the grotto has a raven in it")
check(E6, "the raven is in the grotto over there")
check(E6, "grotto grotto grotto raven raven raven")
check(E6, "the raven is black", false)
check(E6, "the grotto is large", false)
check(E6, "many grottos have a raven", false)

checkRegister(E7, "This is a sentence.", "This")
checkRegister(E7, "First, I would like to say \"Hello!\"", "First")

