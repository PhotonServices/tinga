/**
 * @author Ernesto Gutiérrez Corona- ernesto.g.corona@gmail.com
 */

package tinga.sentiment

import tinga.nlp.texttools.TextPreprocessor._
import tinga.nlp.texttools.SentimentTagger
import tinga.nlp.texttools.SentimentUtils._
import tinga.nlp.texttools.Tokenizer
import tinga.nlp.texttools.SpellChecker
import scala.util.matching.Regex
import scala.collection.mutable.Buffer


class Sentiment(lang: String){
  private val _lang = lang
  val ordinary  = (('a' to 'z') ++ ('A' to 'Z')).toSet
  val tokenizer = new Tokenizer(_lang)
  val corrector = new SpellChecker(_lang)
  val sentiTag  = new SentimentTagger(_lang)
  val sentiPrep = sentimentPreprocess(_lang)(_)
  val responses = List("No sentiment detected", "More context is needed", "Sentiment")

  def isUnaccented(str: String) = str.forall(ordinary.contains(_))

  def normalizeScore(score: Double): Double = {
    var s = score
    if(score < -2.0) s = -2.0
    if(score > 2.0) s = 2.0
    s
  }

  def spellCheck(word: String, accented: Boolean = true): String = {
    var str = word.toLowerCase
    if(accented)
      if(isUnaccented(word.trim)) corrector.correct(str)
      else str
    else
      removeDiacritics(str)
  }

  def sentimentGroups(sentimentSentence: Buffer[(String, Double)]): Buffer[Buffer[(String, Double)]] = {
    val doubleNegationLangs = List("es", "fr")
    var denierFlag = false
    var i, j = 0
    var l = Buffer[Int]()
    for(element <- sentimentSentence){
      if(element._1 == "-") i = i+1
      else{
        if(i > 2){
          l = l:+ j
          denierFlag = false
        }
        else{
          if(element._1 == "nRB" && !denierFlag){
            l = l:+ j
            if(doubleNegationLangs contains _lang) denierFlag = true
          }
        }
        i = 0
      }
      j = j+1
    }
    val buffer = Buffer[Buffer[(String, Double)]]()
    var start = 0
    for(index <- 0 to l.length-1){
      buffer.append(sentimentSentence.slice(start,l(index)).filter(x => x._1 != "-"))
      start = l(index)
    }
    buffer.append(sentimentSentence.slice(start, sentimentSentence.length).filter(x => x._1 != "-"))
    buffer
  }

  def scoreSentimentGroup(sentimentGroup: Buffer[(String, Double)]): (Double, String) = {
    val adjectives   = List("+JJ", "-JJ","sJJ")
    val adverbs      = List("+RB", "-RB")
    val nouns        = List("+NN", "-NN")
    val verbs        = List("+VB", "-VB")
    val deniers      = List("nRB")
    val intensifiers = List("iRB")
    var adj, adv, nn, vb, den, int = false
    var sentiment = 0.0
    var doubleNeg = false
    var op = ""
    for(tagPair <- sentimentGroup){
      if(den == true && (deniers contains tagPair._1)) doubleNeg = true
      if(adjectives contains tagPair._1) adj = true
      if(adverbs contains tagPair._1) adv = true
      if(nouns contains tagPair._1) nn = true
      if(verbs contains tagPair._1) vb = true
      if(deniers contains tagPair._1) den = true
      if(intensifiers contains tagPair._1) int = true
    }
    if((adj && nn) || (adv && vb) || ((int || den) && (nn || vb || adj || adv))) op = "mult"
    else
      if(nn || vb || adv || adj) op = "sum"
      else op = "context-dependent"
    if(sentimentGroup.length == 0) return (0.0, "no-sentiment")
    if(sentimentGroup.length == 1){
      if(nn || vb)
        return (0.5 * sentimentGroup(0)._2.toDouble, "sentiment")
      if(den || int)
        return (0.0, "context-dependent")
      if(adj || adv)
        return (sentimentGroup(0)._2.toDouble, "sentiment")
    }
    else{
      if(op == "sum") sentiment  = sentimentGroup.foldLeft(0.0)(_+_._2)
      if(op == "mult") sentiment = sentimentGroup.foldLeft(1.0)(_*_._2)
      if(op == "context-dependent") return (0.0, "context-dependent")

      if(doubleNeg) sentiment = sentiment * -2
    }
  (normalizeScore(sentiment), "sentiment")
  }

  def scoreSentimentSentence(sentimentSentence: Buffer[(String, Double)]): (Double, String) = {
    val groups = sentimentGroups(sentimentSentence)
    var before, current = (0.0, "")
    var score = 0.0
    var zeroFlag = ""
    if(groups.length >= 1)
      for(i <- 0 to groups.length-1){
        current = scoreSentimentGroup(groups(i))
        score = before._1 + current._1
        before = (score, current._2)
        zeroFlag = current._2
      }
    (normalizeScore(score), zeroFlag)
  }

  def globalParagraphScore(sentencesScores: Buffer[(String, Double, String, Int)]): (String, Double, String, Int) = {
    if(sentencesScores.length >= 1){
      var score = 0.0
      var str = ""
      var int = 1
      var contextDependent = false
      var noSentiment = false
      for(s <- sentencesScores){
        str = str + s._1
        score = score + s._2
        int = scala.math.max(1, s._4)
        if(s._3 == "context-dependent") contextDependent = true
        if(s._3 == "no-sentiment") noSentiment = true
      }
      if(score == 0.0)
        if(contextDependent) return (str, score, "context-dependent", int)
        if(noSentiment) return(str, score, "no-sentiment", int)

      return (str, normalizeScore(score), "sentiment", int)
    }
    else
      return ("",0.0,"",0)
  }

  def algebraicSentiment(text: String, spellChecking: Boolean = true): Buffer[(String, Double, String, Int)] = {
    val t = sentiPrep(text)
    if(t == "") return Buffer(("",0.0,"no-sentiment",0))
    val sentences = tokenizer.splitToSentences(t)
    val sentimentSentences = sentences.map(s => sentiTag.tagExpression(tokenizer.splitToWords(s._1).map(w => spellCheck(w, spellChecking)), spellChecking))
    val scores = sentimentSentences.map(sS => scoreSentimentSentence(sS))
    val intensity = sentences.map(s => if(s._1 contains "^") 2 else 1)
    sentences.zipWithIndex.map({ case (v,i) => (v._1, scores(i)._1, scores(i)._2, intensity(i))})
  }

}