package tinga.nlp.texttools

import scala.collection.JavaConversions.mapAsScalaMap
import scala.collection.mutable.Map
import scala.collection.mutable.MutableList
import scala.io.Source

object TextPreprocessor{

  val punctuationChars = List('.',',',';',':','¡','!','¿','?','(',')','[',']','{','}','`',
                              ''','\\','\'','@','#','$','^','&','*','+','-','|','=','_',
                              '~','%')
  val currentDir = getCurrentDirectory + "/src/main/resources/vocabularies/"

  def getCurrentDirectory = new java.io.File( "." ).getCanonicalPath

  def readFileToMap(path: String): Map[String,String] = {
    var map: Map[String, String] = new java.util.HashMap[String, String]
    for(line <- Source.fromFile(path).getLines()) {
      if (line.split(" ").length == 2)
        map += line.split(" ")(0) -> line.split(" ")(1)}
    map
  }

  def readFileToStringList(path: String): List[String] = {
    Source.fromFile(path).getLines.toList
  }

  def readFileToCharList(path: String): List[Char] = {
    Source.fromFile(path).getLines.toList.flatMap(c => c.toCharArray)
  }

  def langSpecificChars(lang: String): List[Char] = {
    readFileToCharList(currentDir + f"special-characters/$lang%s-characters.txt")
  }

  def langStopwords(lang: String): List[String] = {
    readFileToStringList(currentDir + f"stopwords/$lang%s-stopwords.txt")
  }

  def preprocess(lang: String)(text: String, punctuation: Boolean = false,
                    exceptPunct: List[Char] = List(), stopwords: Boolean = false,
                    exceptStop: List[String] = List()): String = lang match {
    case "en" => cleanText(punctuationChars, langStopwords("en"), punctuation,
                           stopwords, List(), text, exceptPunct, exceptStop)
    case "es" => cleanText(punctuationChars, langStopwords("es"), punctuation,
                           stopwords, langSpecificChars("es"), text, exceptPunct, exceptStop)
    case "fr" => cleanText(punctuationChars, langStopwords("fr"), punctuation,
                           stopwords, langSpecificChars("fr"), text, exceptPunct, exceptStop)
    case "it" => cleanText(punctuationChars, langStopwords("it"), punctuation,
                           stopwords, langSpecificChars("it"), text, exceptPunct, exceptStop)
    case "de" => cleanText(punctuationChars, langStopwords("de"), punctuation,
                           stopwords, langSpecificChars("de"), text, exceptPunct, exceptStop)
  }

  def cleanText(punctuationChars: List[Char], langStopwords: List[String],
                punctuation: Boolean, stopwords: Boolean, langChars: List[Char],
                text: String, exceptPunct: List[Char], exceptStop: List[String]): String = {
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
    println(englishTagger.tagExpression(MutableList("Food", "is", "awesome", "I","'ll", "friendly")))
    val spanishTagger = new PoSTagger("es")
    println(spanishTagger.tagExpression(MutableList("El", "auto", "corre", "rapidamente",".","PANTALLA","Del","iphone")))
    val frenchTagger = new PoSTagger("fr")
    println(frenchTagger.tagExpression(MutableList("La", "vie", "en", "rose")))
    println(TextPreprocessor.preprocess("es")("La comida me parece bien @!, esta es una linea de prueba de Mr. Bean y la Sra. Magloire y punto.", true, List(), true, List("me")))
    println(SentimentUtils.emoticonsIdentifier("it")(":( La comida  x-p XP me :)) parece bien :)"))
    println(SentimentUtils.repeatedCharsHandler("es")(":( La comida  x-p XP me !!! parece bieeeeeen :)"))
    println(SentimentUtils.upperCaseHandler("BIEN MAL HORRIBLE"))
  }


}
