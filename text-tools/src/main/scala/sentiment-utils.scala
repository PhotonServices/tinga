package tinga.nlp.texttools

import scala.util.matching.Regex


object SentimentUtils{

  val englishSentiment = List("excellent", "good", "neutral", "bad", "terrible")
  val spanishSentiment = List("excelente", "bueno", "neutral", "malo", "terrible")
  val frenchSentiment  = List("excellent", "bon", "neutre", "mauvais", "terrible")
  val italianSentiment = List("eccellente", "buono", "neutro", "male", "terribile")
  val germanSentiment  = List("ausgezeichnet", "gut", "neutral", "schlecht", "schrecklich")

  val loveEmoticons  = List("<3", "♥", "(})", "({)", ">:D<", ">:d<", ":*", ":-*", ":-^", ";-^", "X-^", ";;)")
  val grinEmoticons  = List(">:D", ":-D", ":D", "=-D", "=D", "X-D", "x-D", "XD", "xD", "8-D", ":))", ":-))",
                           ":>", ":->", "LOL", "(lol)", "(LOL)", ":d", ":-d", ":'D", ":'-D")
  val smileEmoticons = List(">:)", ":-)", ":)", "=)", "=]", ":]", ":}", ":>", ":3", "8)", "8-)", "n.n")
  val winkEmoticons  = List(">;]", ";-)", ";)", ";-]", ";]", ";D", ";^)", "*-)", "*)")
  val tauntEmoticons = List(">:P", ":-P", ":P", ":-p", ":p", ":-b", ":b", ":c)", ":o)", ":^)")
  val gaspEmoticons  = List(">:o", ":-O", ":O", ":o", ":-o", "o_O", "o.O", "°O°", "°o°", "o.o")
  val worryEmoticons = List(">:/",  ":-/", ":/", ":\\", ">:\\", ":-.", ":-s", ":s", ":S", ":-S", ">.>")
  val sadEmoticons   = List(">:[", ":-(", ":(", "=(", ":-[", ":[", ":{", ":-<", ":c", ":-c", "=/", "u.u", "7.7")
  val cryEmoticons   = List(":'(", ":'''(", ";'(", ":((", ":-((", ":<")
  val angryEmoticons = List(">-[", ">-(", "8o|", "X-(", "x-(", "X(", ":@", ":-@")
  val sickEmoticons  = List("(:|", ":~)", "x-s", "X-S", "X-P", "x-p", ":-&", "+o(", "[-(")

  val excellentEmoticons = loveEmoticons ++ grinEmoticons mkString("\\E|\\Q")
  val goodEmoticons      = smileEmoticons ++ winkEmoticons mkString("\\E|\\Q")
  val neutralEmoticons   = tauntEmoticons ++ gaspEmoticons mkString("\\E|\\Q")
  val badEmoticons       = worryEmoticons ++ sadEmoticons ++ sickEmoticons mkString("\\E|\\Q")
  val terribleEmoticons  = cryEmoticons ++ angryEmoticons  mkString("\\E|\\Q")

  val excellentPattern = new Regex("\\Q" + excellentEmoticons + "\\E")
  val goodPattern      = new Regex("\\Q" + goodEmoticons + "\\E")
  val neutralPattern   = new Regex("\\Q" + neutralEmoticons + "\\E")
  val badPattern       = new Regex("\\Q" + badEmoticons + "\\E")
  val terriblePattern  = new Regex("\\Q" + terribleEmoticons + "\\E")

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
    terriblePattern replaceAllIn(textStr, langSentiment(4))
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

    doublePattern replaceAllIn(singlePattern replaceAllIn(text,
                                                          m => m.matched.substring(0,1) + "^"),
                                                          m => m.matched.substring(0,2) + "^")
  }

  def upperCaseHandler(text: String): String = {
    val upperCasePattern = new Regex("\\d*[A-ZÁÉÍÓÚÀÈÌÒÙÑÄÖÜ]{4,}\\d*")

    upperCasePattern replaceAllIn(text, m => m.matched.toLowerCase + "^")
  }


}
