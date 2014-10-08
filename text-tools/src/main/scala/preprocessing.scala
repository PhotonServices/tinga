package tinga.nlp.texttools

import scala.collection.JavaConversions.mapAsScalaMap
import scala.collection.mutable.Map
import scala.collection.mutable.Buffer
import scala.io.Source

object TextPreprocessor{

  val punctuationChars = List('.',',',';',':','¡','!','¿','?','(',')','[',']','{','}','`',
                              '\\','\'','@','#','$','^','&','*','+','-','|','=','_','~','%',
                              '<','>','/', '"')
  val currentDir = getCurrentDirectory + "/src/main/resources/vocabularies/"

  def getCurrentDirectory = new java.io.File( "." ).getCanonicalPath

  def readFileToMap(path: String): Map[String,String] = {
    var map: Map[String, String] = new java.util.HashMap[String, String]
    for(line <- Source.fromFile(path).getLines()) {
      if (line.split(" ").length == 2)
        map += line.split(" ")(0) -> line.split(" ")(1)}
    map
  }

  def readFileToStringList(path: String, encoding: String = "utf-8"): List[String] = {
    Source.fromFile(path, encoding).getLines.toList
  }

  def readFileToCharList(path: String, encoding: String = "utf-8"): List[Char] = {
    Source.fromFile(path, encoding).getLines.toList.flatMap(c => c.toCharArray)
  }

  def langSpecificChars(lang: String): List[Char] = {
    readFileToCharList(currentDir + f"special-characters/$lang%s-characters.txt")
  }

  def langStopwords(lang: String): List[String] = {
    readFileToStringList(currentDir + f"stopwords/$lang%s-stopwords.txt")
  }

  def preprocess(lang: String)(text: String,
                               punctuation: Boolean = false,
                               exceptPunct: List[Char] = List(),
                               stopwords: Boolean = false,
                               exceptStop: List[String] = List()): String = lang match {
    case "en" => cleanText(text, List(),
                           punctuation, exceptPunct,
                           stopwords, langStopwords("en"), exceptStop)
    case "es" => cleanText(text, langSpecificChars("es"),
                           punctuation, exceptPunct,
                           stopwords, langStopwords("es"), exceptStop)
    case "fr" => cleanText(text, langSpecificChars("fr"),
                           punctuation, exceptPunct,
                           stopwords, langStopwords("fr"), exceptStop)
    case "it" => cleanText(text, langSpecificChars("it"),
                           punctuation, exceptPunct,
                           stopwords, langStopwords("it"), exceptStop)
    case "de" => cleanText(text, langSpecificChars("de"),
                           punctuation, exceptPunct,
                           stopwords, langStopwords("de"), exceptStop)
  }

  def cleanText(text: String, langChars: List[Char],
                punctuation: Boolean, exceptPunct: List[Char],
                stopwords: Boolean, langStopwords: List[String], exceptStop: List[String]): String = {
    if (punctuation && !stopwords) {
      text filter { (c: Char) => (isAllowedChar(c, langChars)) &&
                                 (!(punctuationChars contains c) ||
                                 (exceptPunct contains c)) }
    }
    else{
        if (stopwords) {
          val punctDeleted = text filter { (c: Char) => (isAllowedChar(c, langChars)) &&
                                                        (!(punctuationChars contains c) ||
                                                        (exceptPunct contains c)) }
          val wordList = punctDeleted.split(' ').toList map (str => str.toLowerCase.trim)
          val stopwordsRemoved = wordList filter { (str: String) => (!(langStopwords contains str) ||
                                                                    (exceptStop contains str)) }
          stopwordsRemoved.mkString(" ")
        }
        else text filter { (c: Char) => isAllowedChar(c, langChars) }
    }

  }

  def isAllowedChar(c: Char, chars: List[Char]) = c <= '~' || chars.contains(c)

  def main(args: Array[String]) {
    val englishTagger = new PoSTagger("en")
    println(englishTagger.tagExpression(Buffer("Food", "is", "awesome", "I","'ll", "friendly")))
    val spanishTagger = new PoSTagger("es")
    println(spanishTagger.tagExpression(Buffer("El", "auto", "corre", "rapidamente",".","PANTALLA","Del","iphone")))
    val frenchTagger = new PoSTagger("fr")
    println(frenchTagger.tagExpression(Buffer("La", "vie", "en", "rose")))
    println(englishTagger.tagExpression(Buffer("Food", "is", "awesome", "I","'ll", "friendly")))
    println(spanishTagger.tagExpression(Buffer("El", "auto", "corre", "rapidamente",".","PANTALLA","Del","iphone")))
    println(TextPreprocessor.preprocess("es")("La comida me parece bien @!, esta es una linea de prueba de Mr. Bean y la Sra. Magloire y punto.", true, List(), true, List("me")))
    println(SentimentUtils.emoticonsIdentifier("it")(":( La comida  x-p XP me :)) parece bien :)"))
    println(SentimentUtils.repeatedCharsHandler("es")(":( La comida  x-p XP me !!! parece bieeeeeen :)"))
    println(SentimentUtils.upperCaseHandler("BIEN MAL HORRIBLE"))
    println(Tokenizer.splitToSentences("La vida es una canción. Esta es otra oración? No esta es otra oración!"))
    val p = Tokenizer.tokenize("es")("Me gusta pero no la compraría por 3000 pesos! La vida es una canción... Esta es otra oración? No esta es otra oración! ")
    println(p.str)
    p foreach (s => println(s))
  }

}
