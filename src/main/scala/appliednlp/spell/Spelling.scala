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

    val editCandGen1 = EditDistanceCandidateGenerator(vocab)
    val editCandGen2 = EditDistanceCandidateGenerator(vocab,TwoEdits)

    // Get the language model
    val unigramProb = LanguageModel(io.Source.fromFile(args(3)).mkString)

    // Process the input sentence.
    println("Detecting spelling errors in: " + input)
    input.split(" ").foreach { token => { 
      if (!vocab(token)) {
	println("ERROR: " + token)
	val vsCandidates = vsCandGen(token)
	println("  VS: " + vsCandidates.toSeq.sorted.mkString(" "))
	
	val ed1Candidates = editCandGen1(token)
	println("  ED1: " + ed1Candidates.toSeq.sorted.mkString(" "))
	
	val ed2Candidates = editCandGen2(token)
	println("  ED2: " + ed2Candidates.toSeq.sorted.mkString(" "))

	val allCandidates = vsCandidates ++ ed1Candidates
	val best = allCandidates.toSeq.map(c => (c, unigramProb(c))).sorted.last._1
	println("  Best: " + best)
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

sealed trait NumEdits
object OneEdit extends NumEdits
object TwoEdits extends NumEdits

class EditDistanceCandidateGenerator(vocab: Set[String], distance: NumEdits)
extends CandidateGenerator {

  val alpha = ('A' to 'Z') ++ ('a' to 'z')

  def apply(typo: String) = {
    val candidates = distance match {
      case OneEdit => edits(typo)
      case TwoEdits => for (e1 <- edits(typo); e2 <- edits(e1)) yield e2
    }
    candidates.filter(vocab)
  }

  def edits(typo: String) = {

    val typoSeq = typo.toSeq
    val typoLength = typoSeq.length
    val nonTranspositions = (0 until typoLength).flatMap { i => {
      val deletion = (typoSeq.take(i) ++ typoSeq.drop(i+1)).mkString
      val substitutions = 
	for (c <- alpha)
	  yield (typoSeq.take(i) ++ Seq(c) ++ typoSeq.drop(i+1)).mkString
      val insertions = 
	for (c <- alpha)
	  yield (typoSeq.take(i) ++ Seq(c) ++ typoSeq.drop(i)).mkString
      Seq(deletion) ++ substitutions ++ insertions
    }}
    val transpositions = (1 until typoLength).map { i => { 
      (typoSeq.take(i-1) 
       ++ Seq(typoSeq(i),typoSeq(i-1)) 
       ++ typoSeq.drop(i+1)).mkString
    }}

    (nonTranspositions ++ transpositions).toSet
  }

}

object EditDistanceCandidateGenerator {

  def apply(vocab: Set[String], distance: NumEdits = OneEdit) = 
    new EditDistanceCandidateGenerator(vocab, distance)
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
				   
