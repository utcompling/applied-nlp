package appliednlp.spell

object SpellingCorrector {

  def main(args: Array[String]) {
    
    // The input sentence
    val input = args(0)
    
    // Get the vocabulary from one or both input files (when available)
    val firstVocab = getVocab(args(1)) 
    val secondVocab = if (args.length>2) getVocab(args(2)) else Set[String]()
    val vocab = firstVocab ++ secondVocab
    
    // Set up the vs candidate generator
    val vsCandGen = VectorSpaceCandidateGenerator(vocab)

    // Process the input sentence.
    println("Detecting spelling errors in: " + input)
    input.split(" ").foreach { token => { 
      if (!vocab(token)) {
	println("ERROR: " + token)
	val vsCandidates = vsCandGen(token)
	println("  VS: " + vsCandidates.toSeq.sorted.mkString(" "))
      }
    }}
  }

  // Get a word list from a file with one word per line
  def getVocab(filename: String) = 
    io.Source.fromFile(filename).getLines.toSet

}


object LanguageModel {

  def apply(text: String) = {
    val unigramCounts = collection.mutable.HashMap[String,Double]().withDefault(x=>0.0)
    var numTokens = 1
    text
      .replaceAll("""[^a-zA-Z\s]""","")
      .replaceAll("\\s+"," ")
      .split(" ")
      .foreach { word => { 
	unigramCounts(word) += 1
	numTokens += 1
      }}
    unigramCounts.mapValues(_/numTokens).toMap.withDefault(x=>1.0/numTokens)
  }
}

/**
 * Candidate generators produce valid words from the vocabulary that
 * are close (by some measure) to the typo.
 */
trait CandidateGenerator {

  /**
   * Produce a set of candidates for the typo.
   *
   * @param typo the typo that we need candidates for
   * @return the set of candidates as determined by this candidate generator
   */
  def apply(typo: String): Set[String]
}


class VectorSpaceCandidateGenerator(
  vocabVectors: Map[String, Map[String, Int]],
  invertedIndex: Map[String, Seq[String]],
  numCandidates: Int
) extends CandidateGenerator {

  import VectorSpaceCandidateGenerator._

  def apply(typo: String) = {
    val typoVector = getVector(typo)
    typoVector
      .keys
      .flatMap(invertedIndex)
      .toSeq
      .map(c => (c,cosine(typoVector,vocabVectors(c))))
      .sortBy(_._2)
      .takeRight(numCandidates)
      .map(_._1)
      .toSet
  }

}

/**
 * A companion object to help set up VS candidate generators and
 * provide helper functions.
 */
object VectorSpaceCandidateGenerator {
  import math.{sqrt,pow}

  def apply(vocab: Set[String], numCandidates: Int = 20) = {

    // A map from words to their counts. Can be used later to look up
    // vectors for candidates without needing to recompute the counts.
    // (Trading use of more space to make cosine computations faster.)
    val vocabVectors: Map[String,Map[String,Int]] = 
      vocab.map(word => (word, getVector(word))).toMap

    // Build the inverted index.
    val invertedIndex = vocabVectors
      .toSeq
      .flatMap { case(word, ngrams) => {
	ngrams.map { case(ngram, count) => (ngram,word) }.toSeq
      }}
      .groupBy(x=>x._1)
      .mapValues(_.map(_._2))
      .withDefault(x=>Seq[String]())

    new VectorSpaceCandidateGenerator(vocabVectors, invertedIndex, numCandidates)
  }

  // Get the character ngrams in a word with their counts
  def getVector(word: String, size: Int = 3): Map[String,Int] =
    ("#"+word+"#")
      .sliding(size)
      .toSeq
      .groupBy(x=>x)
      .mapValues(_.length)
      .withDefault(x=>0)

  // Compute the cosine between two vectors
  def cosine(x: Map[String,Int], y: Map[String, Int]) = {
    val dotProduct = x.map { case(k,v) => v*y(k) }.sum
    dotProduct/(norm(x)*norm(y))
  }

  // Compute the Euclidean norm of a vector
  def norm(x: Map[String,Int]) = sqrt(x.values.map(pow(_,2)).sum)

}
				   
