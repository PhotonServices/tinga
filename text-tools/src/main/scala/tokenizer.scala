package tinga.nlp.texttools

import scala.util.matching.Regex

class WordToken(word: String, tag: String = "", lemma: String = ""){
  // Auxiliar Constructor 1
  def this(word: String, tag: String) = {
    this(word, tag, "")
  }
  // Auxiliar Constructor 2
  def this(word: String) = {
    this(word, "", "")
  }

  private var _wordStr = word
  private var _posTag  = tag
  private var _lemmata = lemma
  private var _prevWord = ""
  private var _nextWord = ""

  def wordStr = _wordStr

  def posTag = _posTag

  def lemmata = _lemmata

  def prevWord = _prevWord

  def nextWord = _nextWord

  def posTag_= (tag: String) = _posTag = tag

  def lemmata_= (lemma: String) = _lemmata = lemma

  def prevWord_= (previous: String) = _prevWord = previous

  def nextWord_= (next: String) = _nextWord = next

  override def toString() = this._wordStr+"/"+this._posTag+"/"+this._lemmata
}
