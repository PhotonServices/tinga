/**
 * @author Ernesto Gutiérrez Corona- ernesto.g.corona@gmail.com
 */

package tinga.nlp.texttools

import TextPreprocessor.langSpecificChars
import TextPreprocessor.lexiconDir
import TextPreprocessor.readAsStream
import scala.io.Source
import scala.collection.immutable.Map

class SpellChecker(lang: String){

  def train(path: String, encoding: String = "utf-8"): Map[String, Int] = {
    val lines = readAsStream(path)
    val str = lines.mkString(" ").toLowerCase.filter(x => x<'0' || x>'9')
    str.split("( )+").groupBy(identity).mapValues(_.size)
  }

  val NWORDS = train(lexiconDir + f"vocabularies/$lang%s/$lang%s-lexicon_spell_check.txt")

  var alphabet = "abcdefghijklmnopqrstuvwxyz" + TextPreprocessor.langSpecificChars(lang).mkString

  def edits1(word:String) = {
      Set.empty ++
      (for (i <- 0 until word.length) yield (word take i) + (word drop (i + 1))) ++ // Deletes
      (for (i <- 0 until word.length - 1) yield (word take i) + word(i + 1) + word(i) + (word drop (i + 2))) ++ // Transposes
      (for (i <- 0 until word.length; j <- alphabet) yield (word take i) + j + (word drop (i+1))) ++ // Replaces
      (for (i <- 0 until word.length; j <- alphabet) yield (word take i) + j + (word drop i)) // Inserts
    }

  def known_edits2(word:String) = {Set.empty ++ (for (e1 <- edits1(word); e2 <- edits1(e1) if NWORDS contains e2) yield e2)}

  def known(words: List[String] ):List[String] = words.filter( s => NWORDS.contains(s) )

  def correct(word: String): String = {
    (known(List(word)) match {
      case List(word) => List(word)
      case Nil =>
        known(edits1(word).toList) match {
          case Nil => known_edits2(word)
          case s => s
        }
    }).foldLeft(word)((a,b) => if (NWORDS.getOrElse(a,0) > NWORDS.getOrElse(b,0)) a else b)
  }
}
