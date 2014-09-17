package tinga.nlp.texttools



object Preprocessing{
  def getCurrentDirectory = new java.io.File( "." ).getCanonicalPath
  val dir = getCurrentDirectory + "/src/main/resources/vocabularies/"
  lazy val spanishChars = io.Source.fromFile(dir + "special-characters/es-characters.txt").
                          getLines.toList.flatMap (c => c.toCharArray)
  lazy val frenchChars  = io.Source.fromFile(dir + "special-characters/fr-characters.txt").
                          getLines.toList.flatMap (c => c.toCharArray)
  lazy val italianChars = io.Source.fromFile(dir + "special-characters/it-characters.txt").
                          getLines.toList.flatMap (c => c.toCharArray)
  lazy val germanChars  = io.Source.fromFile(dir + "special-characters/de-characters.txt").
                          getLines.toList.flatMap (c => c.toCharArray)
  lazy val englishStopwords = io.Source.fromFile(dir + "stopwords/en-stopwords.txt").
                              getLines.toList
  lazy val spanishStopwords = io.Source.fromFile(dir + "stopwords/es-stopwords.txt").
                              getLines.toList
  lazy val frenchStopwords  = io.Source.fromFile(dir + "stopwords/fr-stopwords.txt").
                              getLines.toList
  lazy val italianStopwords = io.Source.fromFile(dir + "stopwords/it-stopwords.txt").
                              getLines.toList
  lazy val germanStopwords  = io.Source.fromFile(dir + "stopwords/de-stopwords.txt").
                              getLines.toList
  val punctuationChars = List('.',',',';',':','¡','!','¿','?','(',')','[',']','{','}','`',
                              ''','\\','\'','@','#','$','^','&','*','+','-','|','=','_',
                              '~','%')
  val emptyList = List()


  def preprocessing(lang: String)(text: String, punctuation: Boolean = false, exceptPunct: List[Char] = List(),
                    stopwords: Boolean = false, exceptStop: List[String] = List()):
                    String = lang match {
    case "en" => preprocess(punctuationChars, englishStopwords, punctuation,
                            stopwords, emptyList, text, exceptPunct, exceptStop)
    case "es" => preprocess(punctuationChars, spanishStopwords, punctuation,
                            stopwords, spanishChars, text, exceptPunct, exceptStop)
    case "fr" => preprocess(punctuationChars, frenchStopwords, punctuation,
                            stopwords, frenchChars, text, exceptPunct, exceptStop)
    case "it" => preprocess(punctuationChars, italianStopwords, punctuation,
                            stopwords, italianChars, text, exceptPunct, exceptStop)
    case "de" => preprocess(punctuationChars, germanStopwords, punctuation,
                            stopwords, germanChars, text, exceptPunct, exceptStop)
  }

  def preprocess(punctuationChars: List[Char], langStopwords: List[String],
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
    println(preprocessing("es")("La comida me parece bien @!", true, emptyList, true, List("me")))
    println(SentimentUtils.emoticonsIdentifier("it")(":( La comida  x-p XP me :)) parece bien :)"))
    println(SentimentUtils.repeatedCharsHandler("es")(":( La comida  x-p XP me !!! parece bieeeeeen :)"))
  }
}
