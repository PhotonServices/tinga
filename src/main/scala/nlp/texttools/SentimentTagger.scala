/**
 * @author Ernesto Gutiérrez Corona- ernesto.g.corona@gmail.com
 */

package tinga.nlp.texttools

import TextPreprocessor.readFileToMap
import TextPreprocessor.lexiconDir
import scala.collection.mutable.Buffer

class SentimentTagger(lang: String){
  val positive_adjectives = readFileToMap(lexiconDir + f"vocabularies/$lang%s/$lang%s-positive_adjectives.txt")
  val positiva_adverbs    = readFileToMap(lexiconDir + f"vocabularies/$lang%s/$lang%s-positive_adverbs.txt")
  val positive_nouns      = readFileToMap(lexiconDir + f"vocabularies/$lang%s/$lang%s-positive_nouns.txt")
  val positive_verbs      = readFileToMap(lexiconDir + f"vocabularies/$lang%s/$lang%s-positive_verbs.txt")
  val negative_adjectives = readFileToMap(lexiconDir + f"vocabularies/$lang%s/$lang%s-negative_adjectives.txt")
  val negativa_adverbs    = readFileToMap(lexiconDir + f"vocabularies/$lang%s/$lang%s-negative_adverbs.txt")
  val negative_nouns      = readFileToMap(lexiconDir + f"vocabularies/$lang%s/$lang%s-negative_nouns.txt")
  val negative_verbs      = readFileToMap(lexiconDir + f"vocabularies/$lang%s/$lang%s-negative_verbs.txt")
  val intensity_adverbs   = readFileToMap(lexiconDir + f"vocabularies/$lang%s/$lang%s-intensity_adverbs.txt")
  val negation_adverbs    = readFileToMap(lexiconDir + f"vocabularies/$lang%s/$lang%s-negation_adverbs.txt")
  val slang               = readFileToMap(lexiconDir + f"vocabularies/$lang%s/$lang%s-slang.txt")

  def tagWord(word: String): (String, Double) = {
    if(positive_adjectives contains word) return ("+JJ", positive_adjectives(word).toDouble)
    if(positiva_adverbs contains word) return ("+RB", positiva_adverbs(word).toDouble)
    if(positive_nouns  contains word) return ("+NN", positive_nouns(word).toDouble)
    if(positive_verbs contains word) return ("+VB", positive_verbs(word).toDouble)
    if(negative_adjectives contains word) return ("-JJ", negative_adjectives(word).toDouble)
    if(negativa_adverbs contains word) return ("-RB", negativa_adverbs(word).toDouble)
    if(negative_nouns  contains word) return ("-NN", negative_nouns(word).toDouble)
    if(negative_verbs contains word) return ("-VB", negative_verbs(word).toDouble)
    if(intensity_adverbs contains word) return ("iRB", intensity_adverbs(word).toDouble)
    if(negation_adverbs contains word) return ("nRB", negation_adverbs(word).toDouble)
    if(slang contains word) return ("sJJ", negation_adverbs(word).toDouble)
    return ("-", 0)
  }

  def tagExpression(expression: Buffer[String]): Buffer[(String, Double)] = {
    val buffer = Buffer[(String, Double)]()
    for(word <- expression)
      buffer.append(tagWord(word))
    buffer
  }
}
