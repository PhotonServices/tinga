package tinga.nlp.texttools

import scala.collection.mutable.Buffer
import scala.util.matching.Regex
import PoSTagger._

class WordToken(word: String, tag: String, lemma: String){
  private val _wordStr = word
  private var _posTag  = tag
  private var _lemmata = lemma
  private var _chunkTag = ""
  private var _roleTag = ""
  private var _relationTag = ""

  def wordStr = _wordStr

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

  override def toString() = this._wordStr//+"/"+this._posTag+"/"+this._lemmata
}

object WordToken{
  def apply(word: String = "", tag: String = "", lemma: String = "") = new WordToken(word, tag, lemma)
}

class SentenceToken(sentence: List[WordToken]) extends Seq[WordToken]{
  private var _words = sentence
  private var _position = "start" // "start" or "middle" or "end" according to its position in paragraph
  private var _kind = "declarative"

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

  override def toString() = str
}

object SentenceToken{
  def apply() = new SentenceToken(List())
  def apply(sentence: List[WordToken]) = new SentenceToken(sentence)
  def apply(sentence: String) = new SentenceToken(sentence.split(" ").map(w => WordToken(w.trim)).toList)
}

class Paragraph(paragraph: List[SentenceToken]) extends Seq[SentenceToken]{
  private var _sentences = paragraph

  def iterator = _sentences.iterator

  def apply(i: Int) = _sentences(i)

  def length = _sentences.length

  def sentences = _sentences

  def addSentence(sentence: SentenceToken) = _sentences ::: List(sentence)

  def str = _sentences.mkString//_sentences map(s =>if(s.kind == "Exclamatory") s.str +"!" else if(s.kind == "Interrogative") s.str +"?" else if(s.kind == "Declarative") s.str +".")
}

object TextSplitter{
  def exceptionHandler(text: String): String = {
    val notAbbrev = new Regex("\\S{4,} *\\.{1,3} *")
    notAbbrev.replaceAllIn(text.replace("...","¨"), m => m.matched replace(".", " ¨ ")) + "¨"
  }

  def splitToSentences(text: String): List[(String, String)] = {
    def split(str: List[Char], acc: List[Char]): List[(String, String)] = str match {
      case head :: tail =>
            if(head == '?') (acc.reverse.mkString, "Interrogative") :: split(tail, Nil)
            else if(head == '!') (acc.reverse.mkString, "Exclamatory") :: split(tail, Nil)
            else if(head == '¨') (acc.reverse.mkString, "Declarative") :: split(tail, Nil)
            else split(tail, head :: acc)
      case Nil => Nil
    }
    split(exceptionHandler(text).toList, Nil)
  }

  def splitToWords(text: String): List[String] = {
    text.split(" ").filter (w => w != "").toList
  }

}

object Tokenizer{
  def tokenize(text: String, lang: String = "en"): List[SentenceToken] = {
    val tagger = PoSTagger(lang)
    tagger.tagExpression()
  }
}
