/**
 * @author Ernesto Gutiérrez Corona- ernesto.g.corona@gmail.com
 */

package tinga.sentiment

import tinga.nlp.texttools.TextPreprocessor._
import tinga.nlp.texttools.SentimentTagger
import tinga.nlp.texttools.SentimentUtils._
import tinga.nlp.texttools.Tokenizer
import tinga.nlp.texttools.WordToken
import tinga.nlp.texttools.SentenceToken
import tinga.nlp.texttools.Paragraph
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

  def isOrdinary(str: String) = str.forall(ordinary.contains(_))

  def spellCheck(word: String) = if(isOrdinary(word.trim)) corrector.correct(word.toLowerCase)
                                 else word.toLowerCase

  def sentimentGroups(sentimentSentence: Buffer[(String, Double)]): Buffer[Buffer[(String, Double)]] = {
    var i, j = 0
    var l = Buffer[Int]()
    for(element <- sentimentSentence){
      if(element._1 == "-") i = i+1
      else{
        if(i>2){
          l = l:+ j
          i = 0
        }
        j = j+1;
      }
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
    for(group <- sentimentGroup){
      if(den == true && (deniers contains group._1)) doubleNeg = true
      if(adjectives contains group._1) adj = true
      if(adverbs contains group._1) adv = true
      if(nouns contains group._1) nn = true
      if(verbs contains group._1) vb = true
      if(deniers contains group._1) den = true
      if(intensifiers contains group._1) int = true
    }
    if((adj && nn) || (adv && vb) || ((int || den) && (nn || vb || adj || adv))) op = "mult"
    else
      if(nn || vb || adv) op = "sum"
      else op = "context"
    if(sentimentGroup.length == 0) return (0.0, "no-sentiment")
    if(sentimentGroup.length == 1){
      if(nn || vb )
        return (0.5 * sentimentGroup(0)._2.toDouble, "sentiment")
      if(den || int)
        return (0.0, "context")
      if(adj || adv)
        return (sentimentGroup(0)._2.toDouble, "sentiment")
    }
    else{
      if(op == "sum") sentiment  = sentimentGroup.foldLeft(0.0)(_+_._2)
      if(op == "mult") sentiment = sentimentGroup.foldLeft(1.0)(_*_._2)
      if(op == "context") return (0.0, "context")

      if(doubleNeg) sentiment = scala.math.abs(sentiment) * -2
    }
  if(sentiment > 2.0) sentiment = 2.0
  if(sentiment < -2.0) sentiment = -2.0
  (sentiment, "sentiment")
  }

  def scoreSentimentSentence(sentimentSentence: Buffer[(String, Double)]): (Double, String) = {
    val groups = sentimentGroups(sentimentSentence)
    var before, current = (0.0, "")
    var score = 0.0
    var zeroFlag = ""
    for(i <- 0 to groups.length-1){
      current = scoreSentimentGroup(groups(i))
      score = before._1 + current._1
      zeroFlag = current._2
    }
    score = score / groups.length
    (score, zeroFlag)
  }

  //def scoreComment(sentimentSentences: Buffer[Buffer[(String, Int)]]): (Double, String)

  def algebraicSentiment(text: String): Buffer[(String, Double, String, String)] = {
    val t = sentiPrep(text)
    val sentences = tokenizer.splitToSentences(t)
    val sentimentSentences = sentences.map(s => sentiTag.tagExpression(tokenizer.splitToWords(s._1).map(w => spellCheck(w))))
    val scores = sentimentSentences.map(sS => scoreSentimentSentence(sS))
    val intensity = sentences.map(s => if(s._1 contains "^") "intense" else "regular")
    sentences.zipWithIndex.map({ case (v,i) => (v._1, scores(i)._1, scores(i)._2, intensity(i))})
  }


/*
  def tokenize(text: String): Paragraph = {
    val t = sentiPrep(text)
    val sentences = tokenizer.splitToSentences(t)
    Paragraph(sentences.
      map(s => { val words = tokenizer.splitToWords(s._1);
                  SentenceToken(words.toList.
                    map(w => { val corrected = if(isOrdinary(w)) corrector.correct(w) else w;
                               val word = WordToken(corrected, sentiTag.tagWord(corrected)._1);
                               word.polarity = sentiTag.tagWord(corrected)._2;
                               word
                             }), s._2)
                }))
  }

  def score(text: String): (Double, String) = {
    val paragraph = tokenize(text)
    for(sentence <- paragraph){
      for(g <- groupTags(sentence)){

      }

    }

    def groupTags(sentence: SentenceToken): List[SentenceToken] = {
      var i = 0
      val group1 = SentenceToken()
      val group2 = SentenceToken()
      for(word <- sentence){
        if(word.posTag != "-"){
          if(i<3){
            group1.addWord(word)
          }
          else{
            i = 0
            if(group2.length == 1)
              group2.addWord(word)
          }
        }
        else i += 1
      }
      List(group1, group2)
    }
  }
*/
}
