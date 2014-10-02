package tinga.nlp.texttools

import scala.collection.mutable.Buffer
import TextPreprocessor.readFileToMap
import TextPreprocessor.readFileToStringList
import TextPreprocessor.currentDir


class PoSTagger(lang: String) {
  def this() = {
    this("en")
    println("No language defined using english as default")
  }

  private lazy val _lexicon    = readFileToMap(currentDir + f"pos-trained-corpus/$lang%s-lexicon.txt")
  private lazy val _morphology = readFileToStringList(currentDir + f"pos-trained-corpus/$lang%s-morphology.txt")
  private lazy val _context    = readFileToStringList(currentDir + f"pos-trained-corpus/$lang%s-context.txt")
  private val _cmd = (
                      List("word",          // Word is x
                            "char",         // Word contain x
                            "goodright",    // Word followed by word x
                            "goodleft",     // Word preceded by word x
                            "haspref",      // Word starts with x
                            "hassuf",       // Word ends with x
                            "addpref",      // x + Word is in lexicon
                            "addsuf",       // Word + x is in lexicon
                            "deletepref",   // Word without x at the start is in lexicon
                            "deletesuf"     // Word without x at the end is in lexicon
                        ) flatMap ((str: String) => List("f" + str, str))) ++ (
                      List("prevtag",       // Preceding word is tagged x
                          "nexttag",        // Following word is tagged x
                          "prev2tag",       // Word 2 before is tagged x
                          "next2tag",       // Word 2 after is tagged x
                          "prev1or2tag",    // One of 2 preceding words is tagged x
                          "next1or2tag",    // One of 2 following words is tagged x
                          "prev1or2or3tag", // One of 3 preceding words is tagged x
                          "next1or2or3tag", // One of 3 following words is tagged x
                          "surroundtag",    // Preceding word is tagged x and following word is tagged y
                          "curwd",          // Current word is x
                          "prevwd",         // Preceding word is x
                          "nextwd",         // Following word is x
                          "prev1or2wd",     // One of 2 preceding words is x
                          "next1or2wd",     // One of 2 following words is x
                          "next1or2or3wd",  // One of 3 preceding words is x
                          "prev1or2or3wd",  // One of 3 following words is x
                          "prevwdtag",      // Preceding word is x and tagged y
                          "nextwdtag",      // Following word is x and tagged y
                          "wdprevtag",      // Current word is y and preceding word is tagged x
                          "wdnexttag",      // Current word is x and following word is tagged y
                          "wdand2aft",      // Current word is x and word 2 after is y
                          "wdand2tagbfr",   // Current word is y and word 2 before is tagged x
                          "wdand2tagaft",   // Current word is x and word 2 after is tagged y
                          "lbigram",        // Current word is y and word before is x
                          "rbigram",        // Current word is x and word after is y
                          "prevbigram",     // Preceding word is tagged x and word before is tagged y
                          "nextbigram"      // Following word is tagged x and word after is tagged y
                        ))

  def tagsetTransform(tag: String, from: String = "penntreebank", to: String = "universal"): String = {
    val mapping = readFileToMap(currentDir + f"tagsets/$from%s-$to%s.txt")
    mapping.getOrElse(tag, "")
  }

  def morphologyRules(word: String, cmd: String, affix: String, previous: String = "", next: String = ""): Boolean = {
    var exist = false
    if((cmd == "word"       && affix == word)                               ||
       (cmd == "goodleft"   && word == next)                                ||
       (cmd == "goodright"  && word == previous)                            ||
       (cmd == "char"       && word.contains(affix))                        ||
       (cmd == "hassuf"     && word.endsWith(affix))                        ||
       (cmd == "haspref"    && word.startsWith(affix))                      ||
       (cmd == "addpref"    && _lexicon.contains(affix + word))             ||
       (cmd == "addsuf"     && _lexicon.contains(word + affix))             ||
       (cmd == "deletepref" && _lexicon.contains(word.stripPrefix(affix)))  ||
       (cmd == "deletesuf"  && _lexicon.contains(word.stripSuffix(affix))))   exist = true
    exist
  }

  def contextRules(taggedExpression: Buffer[(String, String)], cmd: String, i: Int, x: String, y: String): Boolean = {
    val tE = taggedExpression
    var exist = false
    if((cmd == "prevtag"        && x == tE(i-1)._2)                                               ||
       (cmd == "nexttag"        && x == tE(i+1)._2)                                               ||
       (cmd == "prev2tag"       && x == tE(i-2)._2)                                               ||
       (cmd == "next2tag"       && x == tE(i+2)._2)                                               ||
       (cmd == "curwd"          && x == tE(i+0)._1)                                               ||
       (cmd == "prevwd"         && x == tE(i-1)._1)                                               ||
       (cmd == "nextwd"         && x == tE(i+1)._1)                                               ||
       (cmd == "prevwdtag"      && x == tE(i-1)._1 && y == tE(i-1)._2)                            ||
       (cmd == "nextwdtag"      && x == tE(i+1)._1 && y == tE(i+1)._2)                            ||
       (cmd == "surroundtag"    && x == tE(i-1)._2 && y == tE(i+1)._2)                            ||
       (cmd == "wdprevtag"      && x == tE(i-1)._2 && y == tE(i+0)._1)                            ||
       (cmd == "wdnexttag"      && x == tE(i+0)._1 && y == tE(i+1)._2)                            ||
       (cmd == "wdand2aft"      && x == tE(i+0)._1 && y == tE(i+2)._1)                            ||
       (cmd == "wdand2tagbfr"   && x == tE(i-2)._2 && y == tE(i+0)._1)                            ||
       (cmd == "wdand2tagaft"   && x == tE(i+0)._1 && y == tE(i+2)._2)                            ||
       (cmd == "lbigram"        && x == tE(i-1)._1 && y == tE(i+0)._1)                            ||
       (cmd == "rbigram"        && x == tE(i+0)._1 && y == tE(i+1)._1)                            ||
       (cmd == "prevbigram"     && x == tE(i-2)._2 && y == tE(i-1)._2)                            ||
       (cmd == "nextbigram"     && x == tE(i+1)._2 && y == tE(i+2)._2)                            ||
       (cmd == "prev1or2tag"    && (tE(i-1)._2 + " " + tE(i-2)._2 contains x))                    ||
       (cmd == "next1or2tag"    && (tE(i+1)._2 + " " + tE(i+2)._2 contains x))                    ||
       (cmd == "prev1or2wd"     && (tE(i-1)._1 + " " + tE(i-2)._1 contains x))                    ||
       (cmd == "next1or2wd"     && (tE(i+1)._1 + " " + tE(i+2)._1 contains x))                    ||
       (cmd == "prev1or2or3tag" && (tE(i-1)._2 + " " + tE(i-2)._2 + " " + tE(i-3)._2 contains x)) ||
       (cmd == "next1or2or3tag" && (tE(i+1)._2 + " " + tE(i+2)._2 + " " + tE(i+3)._2 contains x)))  exist = true
    exist
  }

  def lexiconTagger(word: String): (String, String) = {
    //val posTag = tagsetTransform(_lexicon.getOrElse(word, ""))
    val posTag = _lexicon.getOrElse(word, "")
    (word, posTag)
  }

  def morphologyTagger(word: String, tag: String, previous: String = "", next: String = ""): (String, String) = {
    var posTag = tag
    for(rule <- _morphology){
      val r = rule.split(" ")
      if(_cmd.contains(r(1))) {
        if(morphologyRules(word, r(1).toLowerCase, r(0), previous, next)){
          posTag = r(r.length-2)
        }
      }
      if(_cmd.contains(r(2))){
        if(tag != r(0)){
          posTag = tag
        }
        else{
          if(morphologyRules(word, r(2).toLowerCase, r(1), previous, next)){
            posTag = r(r.length-2)
          }
        }
      }
    }
    (word, posTag)
  }

  def contextTagger(taggedExpression: Buffer[(String, String)]): Buffer[(String, String)] = {
    val start = Buffer.tabulate(3)(x =>("emptyWord", "emptyTag"))
    val end   = Buffer.tabulate(3)(x =>("emptyWord", "emptyTag"))
    val expression = start ++ taggedExpression ++ end
    for(i <- 3 until expression.length-3){
      for(rule <- _context){
        val r = rule.split(" ")
        if(expression(i)._2 == r(0) || r(0) == "*"){
          if(contextRules(expression, r(2).toLowerCase, i, r(3), if(r.length > 4) r(4) else "" )){
            expression(i) = (expression(i)._1, r(1))
          }
        }
      }
    }
    expression filter(x => x != ("emptyWord", "emptyTag"))
  }

  def tagWord(word: String): (String, String) = {
    var taggedWord = lexiconTagger(word)
    if(taggedWord._2 == ""){
      if(Character.isUpperCase(taggedWord._1(0))){
        taggedWord = (taggedWord._1, "NNP")
      }
      else{
        if(word.matches("""\d+""")){
          taggedWord = (taggedWord._1, "CD")
        }
        else{
          taggedWord = (taggedWord._1, "NN")
        }
      }
    }
    taggedWord
  }

  def tagExpression(expression: Buffer[String]): Buffer[(String, String)] = {
    val taggedExpression = expression.zipWithIndex map { case (v,i) => { var taggedWord = tagWord(v);
                                                                             taggedWord = morphologyTagger(taggedWord._1, taggedWord._2,
                                                                                                          if(i > 0) expression(i-1) else "",
                                                                                                          if(i < expression.length-1) expression(i+1) else "")
                                                                             taggedWord
                                                                            }}
    contextTagger(taggedExpression)
  }
}

object PoSTagger{
  def apply(lang: String) = new PoSTagger(lang)
}
