/**
 * @author Ernesto Gutiérrez Corona- ernesto.g.corona@gmail.com
 */

package tinga.nlp.texttools

import scala.collection.JavaConversions.mapAsScalaMap
import scala.collection.mutable.Map
import scala.collection.mutable.Buffer
import scala.io.Source

/** Object for preprocessing text
 *
 *  It can be customized for english: "en", spanish: "es", french: "fr", italian: "it" and german: "de"
 */
object TextPreprocessor{

  val punctuationChars = List('.',',',';',':','¡','!','¿','?','(',')','[',']','{','}','`',
                              '\\','\'','@','#','$','^','&','*','+','-','|','=','_','~','%',
                              '<','>','/', '"')
  var lexiconDir = ""
  if(getCurrentDirectory.endsWith("tinga"))
    lexiconDir = getCurrentDirectory + "/src/main/resources/lexicon/"
  else
    lexiconDir = getCurrentDirectory + "/texttools/src/main/resources/lexicon/"

  def getCurrentDirectory = new java.io.File( "." ).getCanonicalPath

  def readFileToMap(path: String, encoding: String = "utf-8"): Map[String,String] = {
    val file = Source.fromFile(path, encoding)
    var map: Map[String, String] = new java.util.HashMap[String, String]
    for(line <- file.getLines()) {
      if (line.split(" ").length == 2)
        map += line.split(" ")(0) -> line.split(" ")(1)}
    file.close
    map
  }

  def readFileToStringList(path: String, encoding: String = "utf-8"): List[String] = {
    val file = Source.fromFile(path, encoding)
    val list =file.getLines.toList
    file.close
    list
  }

  def readFileToCharList(path: String, encoding: String = "utf-8"): List[Char] = {
    val file = Source.fromFile(path, encoding)
    val list = file.getLines.toList.flatMap(c => c.toCharArray)
    file.close
    list
  }

  def langSpecificChars(lang: String): List[Char] = {
    readFileToCharList(lexiconDir + f"special-characters/$lang%s-characters.txt")
  }

  def langStopwords(lang: String): List[String] = {
    readFileToStringList(lexiconDir + f"stopwords/$lang%s-stopwords.txt")
  }

  /** Preprocess text customized by language
   *
   * @return String optionally cleaned from punctuation (with exceptions) and stopwords (with exceptions)
   */
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

  def removeDiacritics(str: String): String = {
    val diacriticChars = "ÀàÈèÌìÒòÙùÁáÉéÍíÓóÚúÝýÂâÊêÎîÔôÛûŶŷÃãÕõÑñÄäËëÏïÖöÜüŸÿÅåÇçŐőŰű".toCharArray
    val asciiChars     = "AaEeIiOoUuAaEeIiOoUuYyAaEeIiOoUuYyAaOoNnAaEeIiOoUuYyAaCcOoUu".toCharArray
    str map (c => if(diacriticChars contains c) asciiChars(diacriticChars.indexOf(c)) else c)
  }
}
