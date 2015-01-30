/**
 * @author Ernesto Gutiérrez Corona- ernesto.g.corona@gmail.com
 */

package tinga.sentiment

import tinga.nlp.texttools.TextPreprocessor._
import tinga.nlp.texttools.TextFeatures._
import tinga.nlp.texttools.SentimentTagger
import tinga.nlp.texttools.SentimentUtils._
import tinga.nlp.texttools.PoSTagger
import tinga.nlp.texttools.Tokenizer
import tinga.nlp.texttools.SpellChecker
import tinga.nlp.texttools.WordToken
import tinga.nlp.texttools.SentenceToken
import scala.util.matching.Regex
import scala.collection.mutable.Buffer
import scala.collection.JavaConversions.mapAsScalaMap
import scala.collection.mutable.Map
import scala.collection.mutable.Buffer

class Sentiment(lang: String){
  private val _lang = lang
  val ordinary  = (('a' to 'z') ++ ('A' to 'Z')).toSet
  val tokenizer = new Tokenizer(_lang)
  val corrector = new SpellChecker(_lang)
  val sentiTag  = new SentimentTagger(_lang)
  val tagger    = new PoSTagger(_lang)
  val sentiPrep = sentimentPreprocess(_lang)(_)
  val sentiClass = new SentimentClassifier(_lang)

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
          if(element._1 == "NEG" && !denierFlag){
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
    val deniers      = List("NEG")
    val intensifiers = List("INT")
    var adj, adv, nn, vb, den, int, pos, neg = false
    var sentiment = 0.0
    var doubleNeg = false

    for(tagPair <- sentimentGroup){
      if(den == true && (deniers contains tagPair._1)) doubleNeg = true
      if(adjectives contains tagPair._1) adj = true
      if(adverbs contains tagPair._1) adv = true
      if(nouns contains tagPair._1) nn = true
      if(verbs contains tagPair._1) vb = true
      if(deniers contains tagPair._1) den = true
      if(intensifiers contains tagPair._1) int = true
      if(tagPair._2 < 0.0 && !(deniers contains tagPair._1)) neg = true
      if(tagPair._2 > 0.0 && !(intensifiers contains tagPair._1)) pos = true
    }

    if(sentimentGroup.length == 0) return (0.0, "no-sentiment")
    if(!(pos || neg) && (den || int)) return (0.0, "context-dependent")

    if(sentimentGroup.length == 1){
      if(nn || vb)
        return (1.0 * sentimentGroup(0)._2.toDouble, "sentiment")
      if(den || int)
        return (0.0, "context-dependent")
      if(adj || adv)
        return (sentimentGroup(0)._2.toDouble, "sentiment")
    }
    else{
      sentiment = scala.math.abs(sentimentGroup.foldLeft(1.0)(_*_._2))
      if(neg) sentiment = sentiment * -1
      if(den) sentiment = sentiment * -1
      if(doubleNeg) sentiment = sentiment * 2
    }
  (normalizeScore(sentiment), "sentiment")
  }

  def scoreSentimentSentence(sentimentSentence: Buffer[(String, Double)]): (Double, String) = {
    val groups = sentimentGroups(sentimentSentence)
    //println(sentimentSentence)
    //println(groups)
    var before, current = (0.0, "")
    var score = 0.0
    var zeroTag = ""
    if(groups.length >= 1)
      for(i <- 0 to groups.length-1){
        current = scoreSentimentGroup(groups(i))
        //  println(current)
        score = before._1 + current._1
        before = (score, current._2)
        zeroTag = current._2
      }
    println(sentiClass.classify(groups))
    if(score == 0.0){
      //println(score)
      (normalizeScore(score), zeroTag)
      }
    else
      (normalizeScore(score), zeroTag)
      //(sentiClass.classify(groups), zeroTag)
  }

  def globalParagraphScore(sentencesScores: Buffer[(String, Double, String, Int)]): (String, Double, String, Int) = {
    if(sentencesScores.length >= 1){
      var score = 0.0
      var str = ""
      var int = 1
      var contextDependent = false
      var noSentiment = false
      var sentiment = false
      for(s <- sentencesScores){
        str = str + s._1
        score = score + s._2
        int = scala.math.max(1, s._4)
        if(s._3 == "context-dependent") contextDependent = true
        if(s._3 == "no-sentiment") noSentiment = true
        if(s._3 == "sentiment") sentiment = true
      }
      if(score == 0.0){
        if(contextDependent) return (str, score, "context-dependent", int)
        if(noSentiment && !sentiment) return(str, score, "no-sentiment", int)
      }
      return (str, normalizeScore(score), "sentiment", int)
    }
    else
      return ("",0.0,"",0)
  }

  def sentiment(text: String, spellChecking: Boolean = true): Buffer[(String, Double, String, Int)] = {
    val t = sentiPrep(text)
    if(t == "") return Buffer(("",0.0,"no-sentiment",0))
    val sentences = tokenizer.splitToSentences(t)
    val sentimentSentences = sentences.map(s => sentiTag.tagExpression(tokenizer.splitToWords(s._1).map(w => spellCheck(w, spellChecking)), spellChecking))
    //val classes = sentimentSentences.map(sS => classifySentimentSentence(sS))
    val scores = sentimentSentences.map(sS => scoreSentimentSentence(sS))
    val intensity = sentences.map(s => if(s._1 contains "^") 2 else 1)
    sentences.zipWithIndex.map({ case (v,i) => (v._1, scores(i)._1, scores(i)._2, intensity(i))})
  }

  def wordCloud(text: String): Buffer[String] = {
    val words = Buffer[String]()
    val t = sentiPrep(text)
    val p = tokenizer.tokenize(t)
    for(s <- p){
      for(w <- s){
        w.posTag = tagger.tagsetTransform(w.posTag)
      }
    }
    var unigrams = p map (s => tokenNgrams(s,1))
    var bigrams = p map (s => tokenNgrams(s, 2))
    var trigrams = p map (s => tokenNgrams(s, 3))

    def unigramFilter(unigramList: List[List[WordToken]]) = {
      unigramList filter(x => x(0).posTag == "NOUN")
    }

    def bigramFilter(bigramList: List[List[WordToken]]) = {
      bigramList filter(x => (List("NOUN", "ADJ") contains x(0).posTag) && (List("NOUN", "ADJ") contains x(1).posTag))
    }

    def trigramFilter(trigramList: List[List[WordToken]]) = {
      trigramList filter(x => (List("NOUN") contains x(0).posTag)         &&
                              (List("CONJ", "PREP") contains x(1).posTag) &&
                              (List("NOUN") contains x(2).posTag))
    }
  unigrams = unigrams map (unigramFilter)
  bigrams  = bigrams map (bigramFilter)
  trigrams = trigrams map (trigramFilter)

  for(i <- 0 to p.length-1){
    if(!trigrams(i).isEmpty) words.append(trigrams(i)(0).mkString(" "))
    else if(!bigrams(i).isEmpty) words.append(bigrams(i)(0).mkString(" "))
         else if(!unigrams(i).isEmpty) words.append(unigrams(i)(0).mkString(" "))
              else words.append("")
  }
  words
  }

}
