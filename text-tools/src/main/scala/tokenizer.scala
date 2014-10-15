package tinga.nlp.texttools

import java.util.regex.Matcher
import scala.collection.mutable.Buffer
import scala.util.matching.Regex
import TextPreprocessor.punctuationChars
import PoSTagger._


class WordToken(word: String, tag: String, lemma: String){
  private val _str = word
  private var _posTag  = tag
  private var _lemmata = lemma
  private var _chunkTag = ""
  private var _roleTag = ""
  private var _relationTag = ""

  def str = _str

  def posTag = _posTag

  def lemmata = _lemmata

  def chunkTag = _chunkTag

  def roleTag = _roleTag

  def relationTag = _relationTag

  def posTag_= (tag: String) = _posTag = tag

  def lemmata_= (lemma: String) = _lemmata = lemma

  def chunkTag_= (tag: String) = _chunkTag = tag

  def roleTag_= (tag: String) = _roleTag = tag

  def relationTag_= (tag: String) = _relationTag = tag

  override def toString() = this._str + "/" + this._posTag
}

object WordToken{
  def apply(word: String = "", tag: String = "", lemma: String = "") = new WordToken(word, tag, lemma)
}

class SentenceToken(sentence: List[WordToken], k: String) extends Seq[WordToken]{
  def this() = this(List(WordToken()), "declarative")

  def this(sentence: List[WordToken]) = this(sentence, "declarative")

  private var _words = sentence
  private var _position = "start"
  private var _kind = k

  def iterator = _words.iterator

  def apply(i: Int) = _words(i)

  def length = _words.length

  def words = _words

  def position = _position

  def kind = _kind

  def position_= (pos: String) = _position = pos

  def kind_= (tag: String) = _kind = tag

  def addWord(word: WordToken) = _words = _words ::: List(word)

  def str = _words.mkString(" ")

  override def toString() = "(" + this._words.mkString(" ") + ", " + this._kind + ")"
}

object SentenceToken{
  def apply() = new SentenceToken()
  def apply(sentence: List[WordToken]) = new SentenceToken(sentence)
  def apply(sentence: List[WordToken], k: String) = new SentenceToken(sentence, k)
}

class Paragraph(paragraph: List[SentenceToken]) extends Seq[SentenceToken]{
  private var _sentences = paragraph

  def iterator = _sentences.iterator

  def apply(i: Int) = _sentences(i)

  def length = _sentences.length

  def sentences = _sentences

  def addSentence(sentence: SentenceToken) = _sentences ::: List(sentence)

  def str = _sentences.mkString(" ")

  override def toString() = str
}

object Paragraph{
  def apply(paragraph: List[SentenceToken]) = new Paragraph(paragraph)
}

class Tokenizer(lang: String){
  private val _lang = lang
  private val _tagger = PoSTagger(lang)
  val abbrevReg1 = new Regex(" (\\w){1,3}\\.")
  val abbrevReg2 = new Regex(" ((\\w){1}\\.)+")
  val urlReg1 = new Regex("((https?)?(ftp)?(://))([A-Za-z0-9]){1,}\\.([A-Za-z0-9]){2,}(\\.\\S+)?(/\\S+)?")
  val urlReg2 = new Regex("((https?)?(ftp)?(://))?([A-Za-z0-9]){2,}\\.([A-Za-z0-9]){2,}(\\.\\S+)?(/\\S+)")
  val punctPattern = new Regex("\\Q" + punctuationChars.filter(c => c != '\'').mkString("\\E|\\Q") + "\\E")

  def splitToSentences(text: String): List[(String, String)] = {
    def exceptionHandler(text: String): String = {
      val t = abbrevReg1.
                replaceAllIn(abbrevReg2.
                  replaceAllIn(urlReg1.
                    replaceAllIn(urlReg2.
                      replaceAllIn(text, m => Matcher.quoteReplacement(m.matched).replace(".", "¨")),
                                         m => Matcher.quoteReplacement(m.matched).replace(".", "¨")),
                                         m => Matcher.quoteReplacement(m.matched).replace(".", "")),
                                         m => Matcher.quoteReplacement(m.matched).replace(".", "")).replace("...", ".")
      if(t(t.length -1) != '.' && t(t.length -1) != '!' && t(t.length -1) != '?') t + "." else t
    }

    def split(str: List[Char], acc: List[Char]): List[(String, String)] = str match {
      case head :: tail =>
            if(head == '?') (acc.reverse.mkString + " ?", "interrogative") :: split(tail, Nil)
            else if(head == '!') (acc.reverse.mkString +" !", "exclamatory") :: split(tail, Nil)
            else if(head == '.') (acc.reverse.mkString + " .", "declarative") :: split(tail, Nil)
            else split(tail, head :: acc)
      case Nil => Nil
    }
    split(exceptionHandler(text).toList, Nil) map (x => (x._1.replace("¨", "."), x._2))
  }

  def splitToWords(text: String, lang: String = "en"): Buffer[String] = {
    var t = text.split("( )+").toBuffer
    if(lang == "en")
      t = text.replace("'", " '").replace("n 't", " n't").split("( )+").toBuffer
    else
    if(lang == "fr")
      t = text.replace("'", "' ").split("( )+").toBuffer
    t.map(w => if(urlReg1.findAllIn(w).isEmpty && urlReg2.findAllIn(w).isEmpty)
                  punctPattern.replaceAllIn(w, m => " " + Matcher.quoteReplacement(m.matched) + " ")
               else w).
          flatMap(w => w.split("( )+")).filter(w => w != "")
  }

  def tokenize(text: String): Paragraph = {
    Paragraph(splitToSentences(text).map(s => SentenceToken(_tagger.tagExpression(splitToWords(s._1, _lang)).toList.map(w => WordToken(w._1, w._2)), s._2)))
  }
}
