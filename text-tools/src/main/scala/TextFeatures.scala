package tinga.nlp.texttools


import Tokenizer.splitToWords

object FeatureExtractor{
  def ngrams(text: String, n: Int = 1): List[String] = {
    def group(splittedText: List[String]): List[List[String]] = splittedText match{
      case(head :: tail) if(splittedText.length >= n) => splittedText.take(n) :: group(tail)
      case(_) => Nil
    }
    group(splitToWords(text).toList) map (x => x.mkString(" "))
  }

  def skipgram(text: String, n: Int, k: Int): List[String] = {
    val m = splitToWords(text).zipWithIndex.groupBy(x => x._2%k)
    m.mapValues(x => ngrams(x.unzip._1.mkString(" "), n)).toList.flatMap(t => t._2)
  }

  def vocabulary(l: List[Any]): Map[Any, Int] = l.groupBy(identity).mapValues(_.size)

  def topN(m: Map[Any, Int], n: Int): List[(Any, Int)] = {
    val sorted = m.toList.sortBy(_._2)
    sorted.reverse.take(n)
  }

  def bottomN(m: Map[Any, Int], n: Int): List[(Any, Int)] = {
    val sorted = m.toList.sortBy(_._2)
    sorted.take(n)
  }
}
