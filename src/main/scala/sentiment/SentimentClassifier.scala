/**
 * @author Ernesto Gutiérrez Corona- ernesto.g.corona@gmail.com
 */

package tinga.sentiment

import tinga.ml.classification._
import tinga.nlp.texttools.TextPreprocessor._
import scala.collection.mutable.Buffer
import scala.collection.immutable.ListMap

class SentimentClassifier(lang: String){
  private val _lang = lang
  val svmObject = new LibSVM()
  val GOOD = 1
  val BAD = -1
  val EXCELLENT = 2
  val TERRIBLE = -2

  val features = readFileToStringList(lexiconDir + "classification/features/pos_features.txt").reverse
  val modelBadTerrible = svmObject.loadRunningModel(lexiconDir + "classification/model/es-bad-terrible-rbfSVM.model")
  val modelGoodBad = svmObject.loadRunningModel(lexiconDir + "classification/model/es-good-bad-rbfSVM.model")
  val modelExcellentGood = svmObject.loadRunningModel(lexiconDir + "classification/model/es-excellent-good-rbfSVM.model")

  def classify(sentimentGroups: Buffer[Buffer[(String, Double)]]): Double ={
    val vector = vectorize(sentimentGroups)
    var sentiment = svmObject.testVector(vector, modelGoodBad, 2)
    if(sentiment < 0)
      sentiment = svmObject.testVector(vector, modelBadTerrible, 2)
    else
      sentiment = svmObject.testVector(vector, modelExcellentGood, 2)
    sentiment.toDouble
  }

  def vectorize(sentimentGroups: Buffer[Buffer[(String, Double)]]): ListMap[String,Double] = {
    var mmap =  ListMap[String, Double]()
    for(e <- features) mmap = mmap + (e -> 0.0)
    for(i <- 0 to sentimentGroups.length - 1){
      if(!sentimentGroups(i).isEmpty){
        val maps = sentimentGroups(i) map ((x:(String, Double)) => ListMap(x._1 + "_" + i -> scala.math.abs(x._2)))
        val map = maps.reduceLeft(sumMap)
        mmap = sumMap(map, mmap)
      }
    }
    mmap
  }

  def sumMap(m1: ListMap[String, Double], m2: ListMap[String, Double]): ListMap[String, Double] = {
    m1 ++ m2.map{ case (k,v) => k -> (v + m1.getOrElse(k,0.0)) }
  }

}
