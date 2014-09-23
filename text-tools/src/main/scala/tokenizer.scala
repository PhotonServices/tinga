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
