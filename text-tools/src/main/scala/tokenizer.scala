package tinga.nlp.texttools


class WordToken(word: String, tag: String, lemma: String){
  // Auxiliar Constructor 1
  def this(word: String, tag: String) = {
    this(word, tag, "")
  }
  // Auxiliar Constructor 2
  def this(word: String) = {
    this(word, "", "")
  }

  private val _wordStr = word
  private var _posTag  = tag
  private var _lemmata = lemma

  def wordStr = _wordStr

  def posTag = _posTag

  def lemmata = _lemmata

  def posTag_= (tag: String) = _posTag = tag

  def lemmata_= (lemma: String) = _lemmata = lemma

  override def toString() = this._wordStr//+"/"+this._posTag+"/"+this._lemmata
}

object WordToken{
  def apply(word: String, tag: String = "", lemma: String = "") = new WordToken(word, tag, lemma)
}

class PhraseToken(phrase: List[WordToken], tag: String){
  // Auxiliar Constructor 1
  def this(phrase: List[WordToken]) = {
    this(phrase, "-")
  }

  private val _words = phrase
  private val _phraseStr = phrase.mkString(" ")
  private var _chunkTag = tag
  private var _position = 1

  def words = _words

  def phraseStr = _phraseStr

  def chunkTag = _chunkTag

  def chunkTag_= (tag: String) = _chunkTag = tag

  override def toString() = this._phraseStr
}

object PhraseToken{
  def apply(phrase: List[WordToken], tag: String = "") = new PhraseToken(phrase, tag)
}

class SentenceToken(sentence: List[PhraseToken]){
  private val _phrases = sentence
  private val _sentenceStr = sentence.mkString(" ")
  private var _position = "start" // "start" or "middle" or "end" according to its position in paragraph

  def phrases = _phrases

  def sentenceStr = _sentenceStr

  def position = _position

  def position_= (pos: String) = _position = pos

  override def toString() = this._sentenceStr
}

object SentenceToken{
  def apply(sentence: List[PhraseToken]) = new SentenceToken(sentence)
}

object TextSplitter{
  val sentenceSplitters  = List(".", "...", "!", "?")
  val paragraphSplitters = List(".\n")
  /*
  def splitText(text: String): List[String] ={

  }

  def splitParagraph

  def splitSentence*/
}
