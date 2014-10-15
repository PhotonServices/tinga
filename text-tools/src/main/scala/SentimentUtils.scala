package tinga.nlp.texttools

import java.util.regex.Matcher
import scala.util.matching.Regex
import TextPreprocessor.readFileToStringList
import TextPreprocessor.lexiconDir
import TextPreprocessor.preprocess

object SentimentUtils{

  val englishSentiment = List(" excellent ", " good ", " neutral ", " bad ", " terrible ", " very much ")
  val spanishSentiment = List(" excelente ", " bueno ", " neutral ", " malo ", " terrible ", " muy ")
  val frenchSentiment  = List(" excellent ", " bon ", " neutre ", " mauvais ", " terrible ", " très ")
  val italianSentiment = List(" eccellente ", " buono ", " neutro ", " male ", " terribile ", " molto ")
  val germanSentiment  = List(" ausgezeichnet ", " gut ", " neutral ", " schlecht ", " schrecklich ", " sehr ")

  val loveEmoticons   = readFileToStringList(lexiconDir + "emoji-emoticons/love.txt")
  val grinEmoticons   = readFileToStringList(lexiconDir + "emoji-emoticons/grin.txt")
  val smileEmoticons  = readFileToStringList(lexiconDir + "emoji-emoticons/smile.txt")
  val winkEmoticons   = readFileToStringList(lexiconDir + "emoji-emoticons/wink.txt")
  val tauntEmoticons  = readFileToStringList(lexiconDir + "emoji-emoticons/taunt.txt")
  val gaspEmoticons   = readFileToStringList(lexiconDir + "emoji-emoticons/gasp.txt")
  val worryEmoticons  = readFileToStringList(lexiconDir + "emoji-emoticons/worry.txt")
  val sadEmoticons    = readFileToStringList(lexiconDir + "emoji-emoticons/sad.txt")
  val cryEmoticons    = readFileToStringList(lexiconDir + "emoji-emoticons/cry.txt")
  val angryEmoticons  = readFileToStringList(lexiconDir + "emoji-emoticons/angry.txt")
  val sickEmoticons   = readFileToStringList(lexiconDir + "emoji-emoticons/sick.txt")
  val posEmoticons    = readFileToStringList(lexiconDir + "emoji-emoticons/good.txt")
  val negEmoticons    = readFileToStringList(lexiconDir + "emoji-emoticons/bad.txt")
  val neutroEmoticons = readFileToStringList(lexiconDir + "emoji-emoticons/neutro.txt")

  val excellentEmoticons = loveEmoticons ++ grinEmoticons mkString("\\E|\\Q")
  val goodEmoticons      = smileEmoticons ++ winkEmoticons ++ posEmoticons mkString("\\E|\\Q")
  val neutralEmoticons   = neutroEmoticons mkString("\\E|\\Q")
  val badEmoticons       = worryEmoticons ++ sadEmoticons ++ sickEmoticons ++ negEmoticons mkString("\\E|\\Q")
  val terribleEmoticons  = cryEmoticons ++ angryEmoticons  mkString("\\E|\\Q")
  val contextEmoticons   = tauntEmoticons ++ gaspEmoticons mkString("\\E|\\Q")

  val excellentPattern = new Regex("\\Q" + excellentEmoticons + "\\E")
  val goodPattern      = new Regex("\\Q" + goodEmoticons + "\\E")
  val neutralPattern   = new Regex("\\Q" + neutralEmoticons + "\\E")
  val badPattern       = new Regex("\\Q" + badEmoticons + "\\E")
  val terriblePattern  = new Regex("\\Q" + terribleEmoticons + "\\E")
  val contextPattern   = new Regex("\\Q" + contextEmoticons + "\\E")

  def emoticonsIdentifier(lang: String)(text: String): String = lang match {
    case "en" => emoticonsReplacer(englishSentiment, text)
    case "es" => emoticonsReplacer(spanishSentiment, text)
    case "fr" => emoticonsReplacer(frenchSentiment, text)
    case "it" => emoticonsReplacer(italianSentiment, text)
    case "de" => emoticonsReplacer(germanSentiment, text)
  }

  def emoticonsReplacer(langSentiment: List[String], text: String): String = {
    var textStr = excellentPattern replaceAllIn(text, langSentiment(0))
    textStr = goodPattern replaceAllIn(textStr, langSentiment(1))
    textStr = neutralPattern replaceAllIn(textStr, langSentiment(2))
    textStr = badPattern replaceAllIn(textStr, langSentiment(3))
    textStr = terriblePattern replaceAllIn(textStr, langSentiment(4))
    contextPattern replaceAllIn(textStr, langSentiment(5))
  }

  def repeatedCharsHandler(lang: String)(text: String): String = lang match{
    case "en" => removeRepeatedChars(List('c','e','f','g','l','m','n','p','o','t' ), text)
    case "es" => removeRepeatedChars(List('c','l','r','n'), text)
    case "fr" => removeRepeatedChars(List('f','n','m','t','s'), text)
    case "it" => removeRepeatedChars(List('c','e','i','n','r','s','t'), text)
    case "de" => removeRepeatedChars(List('m','l','n','r','s'), text)
  }

  def removeRepeatedChars(allowedDoubleChars: List[Char], text: String): String = {
    val str = allowedDoubleChars.mkString
    val singlePattern = new Regex(f"([^$str%s\\s])\\1{1,}")
    val doublePattern = new Regex(f"([$str%s])\\1{2,}")
    val t = doublePattern replaceAllIn(singlePattern replaceAllIn(text,
                                                                  m => Matcher.quoteReplacement(m.matched).substring(0,1) + "^"),
                                                                  m => Matcher.quoteReplacement(m.matched).substring(0,2) + "^")
    t.split("( )+") map(w => if(w contains "^") "^" + w.replace("^","") else w) mkString(" ")
  }

  def upperCaseHandler(text: String): String = {
    val upperCasePattern = new Regex("\\d*[A-ZÁÉÍÓÚÀÈÌÒÙÑÄÖÜ]{4,}\\d*")
    upperCasePattern replaceAllIn(text, m => Matcher.quoteReplacement(m.matched).toLowerCase + "^")
  }

  def replaceAdversativeConjunctions(lang: String)(text: String): String = lang match{
    case "en" => replaceString(List("but", "still", "however", "yet", "nevertheless"), text, " . ")
    case "es" => replaceString(List("pero", "sin embargo", "al contrario", "sino", "sino que"), text, " . ")
    case "fr" => replaceString(List("mais", "toutefois", "cependant", "contrairement à"), text, " . " )
    case "it" => replaceString(List("ma", "tuttavia", "a differenza di", "però"), text, " . ")
    case "de" => replaceString(List("aber", "jedoc", "im Gegensatz zu", "sondern"), text, " . ")
  }

  def replaceString(strs: List[String], text: String, replacement: String): String = {
    val pattern =  new Regex("\\Q " + strs.mkString(" \\E|\\Q ") + " \\E")
    pattern.replaceAllIn(text, replacement)
  }

  def sentimentPreprocess(lang: String = "en")(text: String): String = {
    val findEmoticons = emoticonsIdentifier(lang)(_)
    val preprocessText = preprocess(lang)((_:String))
    val repeatedChars = repeatedCharsHandler(lang)(_)
    val findUpperCase = upperCaseHandler(_)
    val replaceAC= replaceAdversativeConjunctions(lang)(_)
    replaceAC(findUpperCase(repeatedChars(preprocessText(findEmoticons(text)))))
  }
}
