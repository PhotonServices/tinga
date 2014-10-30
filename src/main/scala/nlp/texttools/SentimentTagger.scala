/**
 * @author Ernesto Gutiérrez Corona- ernesto.g.corona@gmail.com
 */

package tinga.nlp.texttools

import TextPreprocessor.readFileToMap
import TextPreprocessor.lexiconDir
import TextPreprocessor.removeDiacritics
import scala.collection.mutable.Buffer

class SentimentTagger(lang: String){
  val positiveAdjectives = readFileToMap(lexiconDir + f"vocabularies/$lang%s/$lang%s-positive_adjectives.txt")
  val positiveAdverbs    = readFileToMap(lexiconDir + f"vocabularies/$lang%s/$lang%s-positive_adverbs.txt")
  val positiveNouns      = readFileToMap(lexiconDir + f"vocabularies/$lang%s/$lang%s-positive_nouns.txt")
  val positiveVerbs      = readFileToMap(lexiconDir + f"vocabularies/$lang%s/$lang%s-positive_verbs.txt")
  val negativeAdjectives = readFileToMap(lexiconDir + f"vocabularies/$lang%s/$lang%s-negative_adjectives.txt")
  val negativeAdverbs    = readFileToMap(lexiconDir + f"vocabularies/$lang%s/$lang%s-negative_adverbs.txt")
  val negativeNouns      = readFileToMap(lexiconDir + f"vocabularies/$lang%s/$lang%s-negative_nouns.txt")
  val negativeVerbs      = readFileToMap(lexiconDir + f"vocabularies/$lang%s/$lang%s-negative_verbs.txt")
  val intensityWords     = readFileToMap(lexiconDir + f"vocabularies/$lang%s/$lang%s-intensity_words.txt")
  val negationWords      = readFileToMap(lexiconDir + f"vocabularies/$lang%s/$lang%s-negation_words.txt")
  val slang              = readFileToMap(lexiconDir + f"vocabularies/$lang%s/$lang%s-slang.txt")
  val posAdjUnaccented   = positiveAdjectives map { case(k,v) => (removeDiacritics(k), v) }
  val posAdvUnaccented   = positiveAdverbs map { case(k,v) => (removeDiacritics(k), v) }
  val posNounsUnaccented = positiveNouns map { case(k,v) => (removeDiacritics(k), v) }
  val posVerbsUnaccented = positiveVerbs map { case(k,v) => (removeDiacritics(k), v) }
  val negAdjUnaccented   = negativeAdjectives map { case(k,v) => (removeDiacritics(k), v) }
  val negAdvUnaccented   = negativeAdverbs map { case(k,v) => (removeDiacritics(k), v) }
  val negNounsUnaccented = negativeNouns map { case(k,v) => (removeDiacritics(k), v) }
  val negVerbsUnaccented = negativeVerbs map { case(k,v) => (removeDiacritics(k), v) }
  val intWordsUnaccented = intensityWords map { case(k,v) => (removeDiacritics(k), v) }
  val denWordsUnaccented = negationWords map { case(k,v) => (removeDiacritics(k), v) }
  val slangUnaccented    = slang map { case(k,v) => (removeDiacritics(k), v) }

  def tagWord(word: String): (String, Double) = {
    if(positiveAdjectives contains word) return ("+JJ", positiveAdjectives(word).toDouble)
    if(positiveAdverbs contains word) return ("+RB", positiveAdverbs(word).toDouble)
    if(positiveNouns  contains word) return ("+NN", positiveNouns(word).toDouble)
    if(positiveVerbs contains word) return ("+VB", positiveVerbs(word).toDouble)
    if(negativeAdjectives contains word) return ("-JJ", negativeAdjectives(word).toDouble)
    if(negativeAdverbs contains word) return ("-RB", negativeAdverbs(word).toDouble)
    if(negativeNouns  contains word) return ("-NN", negativeNouns(word).toDouble)
    if(negativeVerbs contains word) return ("-VB", negativeVerbs(word).toDouble)
    if(intensityWords contains word) return ("iRB", intensityWords(word).toDouble)
    if(negationWords contains word) return ("nRB", negationWords(word).toDouble)
    if(slang contains word) return ("sJJ", slang(word).toDouble)
    return ("-", 0)
  }

  def tagExpression(expression: Buffer[String], accented: Boolean = true): Buffer[(String, Double)] = {
    val buffer = Buffer[(String, Double)]()
    for(word <- expression)
      if(accented)
        buffer.append(tagWord(word))
      else
        buffer.append(tagUnaccentedWord(word))
    buffer
  }

  def tagUnaccentedWord(word: String): (String, Double) = {
    if(posAdjUnaccented contains word) return ("+JJ", posAdjUnaccented(word).toDouble)
    if(posAdvUnaccented contains word) return ("+RB", posAdvUnaccented(word).toDouble)
    if(posNounsUnaccented  contains word) return ("+NN", posNounsUnaccented(word).toDouble)
    if(posVerbsUnaccented contains word) return ("+VB", posVerbsUnaccented(word).toDouble)
    if(negAdjUnaccented contains word) return ("-JJ", negAdjUnaccented(word).toDouble)
    if(negAdvUnaccented contains word) return ("-RB", negAdvUnaccented(word).toDouble)
    if(negNounsUnaccented  contains word) return ("-NN", negNounsUnaccented(word).toDouble)
    if(negVerbsUnaccented contains word) return ("-VB", negVerbsUnaccented(word).toDouble)
    if(intWordsUnaccented contains word) return ("iRB", intWordsUnaccented(word).toDouble)
    if(denWordsUnaccented contains word) return ("nRB", denWordsUnaccented(word).toDouble)
    if(slangUnaccented  contains word) return ("sJJ", slangUnaccented (word).toDouble)
    return ("-", 0)
  }
}
