/*LibSVM -- A Library for Support Vector Machines in SCALA

  Java version: Chih-Chung Chang and Chih-Jen Lin (http://www.csie.ntu.edu.tw/~cjlin/libsvm/)
  Scala version: Esteban Castillo*/
package tinga.ml.classification

import libsvm.svm
import libsvm.svm_model
import libsvm.svm_node
import libsvm.svm_parameter
import libsvm.svm_problem
import java.io.Serializable
import java.io.File
import java.io.OutputStreamWriter
import java.io.FileOutputStream
import java.io.FileInputStream
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import scala.collection.mutable.Buffer
import scala.collection.mutable.ListBuffer

class LibSVM( ) 
{
  /*Function that extracts the features vectors from a tuple in a Buffer (Training phase)
   * In:  Buffer[(Int, Map[String, Double])]
   * Out: Buffer[list[Double]] */
  def extractVectorsTuple(x: Buffer[(Int, Map[String, Double])]) = 
     for(i <- x) yield  i._2.toList.map (y => y._2)
     
  /*Function that extracts the feature vector from a Map in a buffer (Test phase)
   * In:   Buffer[Map[String, Double]]
   * Out:   Buffer[list[Double]] */   
     
  def extractVectorsMap(x: Buffer[Map[String, Double]]) =
     for(i <- x) yield  i.toList.map (y => y._2)  
     
  /*Function that extracts the feature vector from a Map (Test phase)
   * In:    Map[String, Double]
   * Out:   list[Double] */     
          
  def extractVector(x: Map[String, Double]) =
       x.toList.map (y => y._2)    
     
  /*Function that extracts the feature classes (desired value)  from a tuple ina buffer (Training phase)
   * In:    Buffer[(Int, Map[String, Double])]
   * Out:   list[Int] */          
       
  def extractVectorClass(x: Buffer[(Int, Map[String, Double])]): List[Int] = 
     (for(i <- x) yield i._1).toList
      
  /* 
   * Function that creates the model from the training vectors 
   * 
   * In:
   *    
   * cache_size is the size of the kernel cache, specified in megabytes
   
   * C is the cost of constraints violation
   
   * eps is the stopping criterion. (we usually use 0.00001 in nu-SVC, 0.001 in others)
   
   * nu is the parameter in nu-SVM, nu-SVR, andone-class-SVM
   
   * gamma in kernel function (default 1/num_features)
   
   * probability,  whether to train a SVC or SVR model for probability estimates, 0 or 1 
   
   * svmType can be one of C_SVC, NU_SVC, ONE_CLASS, EPSILON_SVR, NU_SVR.

          C_SVC:		C-SVM classification
          NU_SVC:		nu-SVM classification
          ONE_CLASS:	one-class-SVM
          EPSILON_SVR:	epsilon-SVM regression
          
   * kernelType can be one of LINEAR, POLY, RBF, SIGMOID.

          LINEAR:	   u'*v
          POLY:	       (gamma*u'*v + coef0)^degree
          RBF:	       exp(-gamma*|u-v|^2)
          SIGMOID:	   tanh(gamma*u'*v + coef0)  
          
    Out: svm_model (Java object)    
  */   
     
  def training(trainingSet: Buffer[(Int,Map[String,Double])], probability: Int = 1, gamma: Double = 0.5, nu: Double = 0.5, C: Double = 1.0, cache_size: Int = 20000, eps: Double = 0.001, kernelType: String = "LINEAR", svmType: String = "C_SVC"): svm_model = {
       val listsTraining=extractVectorsTuple(trainingSet)
       val featureClasses =extractVectorClass(trainingSet)     
       val prob = new svm_problem()
       val dataCount : Int = trainingSet.length
       prob.y =  new Array[Double](dataCount)
       prob.l = dataCount
       prob.x = Array.ofDim[svm_node](dataCount,dataCount)
       var i : Int = 0
       for(features <- listsTraining)
       {	              
	     prob.x(i)= new Array[svm_node](features.length)
	     for( j <- 1 to features.length)
	     {
	       val node = new svm_node()
	       node.index = j
	       node.value = features(j-1)
	       prob.x(i)(j-1) = node   
         }
	     prob.y(i) = featureClasses(i)
         i=i+1
       }  
       val param = new svm_parameter()
	   param.probability = probability
	   param.gamma = gamma
	   param.nu = nu
	   param.C = C
	   if(svmType == "NU_SVC" ){
         param.svm_type = svm_parameter.NU_SVC
       }else if(svmType == "ONE_CLASS" ){
         param.svm_type = svm_parameter.ONE_CLASS
       }else if(svmType == "EPSILON_SVR" ){
         param.svm_type = svm_parameter.EPSILON_SVR
       }else{
         param.svm_type = svm_parameter.C_SVC
       }	
	   
	   if( kernelType == "POLY" ){
         param.kernel_type = svm_parameter.POLY
       }else if(kernelType == "RBF" ){
         param.kernel_type = svm_parameter.RBF
       }else if( kernelType == "SIGMOID" ){
         param.kernel_type = svm_parameter.SIGMOID
       }else{
         param.kernel_type = svm_parameter.LINEAR
       }	
	   param.cache_size = cache_size
	   param.eps = eps
	   val model = svm.svm_train(prob, param)
	   return model
   }
  
  /*Function that save an existing model in a file
   * In:    
   *      location: file location
   *      name:     file name (file to be created)
   *      model:    model created in the training phase
   * Out: Unit (nothing) */          
  
  def saveModel(location: String ,name: String, model: svm_model): Unit={
    val fileObject= new File(location+name)
    val fileStream=new FileOutputStream(fileObject)
    val serializeModel = new ObjectOutputStream(fileStream)
    serializeModel.writeObject(model)   
    serializeModel.close()
  }
  
   /*Function that load an existing model from a file
   * In:    
   *      location: file location
   *      name:     file name (file to be created)
   * Out: svm_model (Java object)  */    
  
  def loadModel(location: String ,name: String): svm_model={
    val fileObject= new File(location+name)
    val fileStream=new FileInputStream(fileObject)
    val serializeModel = new ObjectInputStream(fileStream)
    val model:svm_model = (serializeModel.readObject()).asInstanceOf[svm_model]
    return model     
  }
  
   /*Function that test a vector (only one)
   * In:    
   *      Map[String,Double]
   *      model:    model created in the training phase
   *      numberClasses: number of different classes in the training phase
   * Out: Prediction (Int)*/    
  
  def testVector(testVector: Map[String,Double], model: svm_model, numberClasses: Int): Int = {  
    val listTest=extractVector(testVector)
    val listLength= listTest.length
    val nodes= new Array[svm_node](listLength)
    for( i <- 1 to listLength)
	{
      val node= new svm_node()
	  node.index = i;
	  node.value = listTest(i-1)
	  nodes(i-1) = node
	}
    val labels= new Array[Int](numberClasses)
	svm.svm_get_labels(model,labels);
    val prob_estimates= new Array[Double](numberClasses)
    val Prediction: Double= svm.svm_predict_probability(model, nodes, prob_estimates)
    return Prediction.toInt
 
  }
  
   /*Function that test vectors (set of vectors)
   * In:    
   *      Buffer[Map[String,Double]]
   *      model:    model created in the training phase
   *      numberClasses: number of different classes in the training phase
   * Out: List[Int] (Predictions) */   
  
  def testVectors(testVector: Buffer[Map[String,Double]], model: svm_model, numberClasses: Int): List[Int] = {
    var predictionList = new ListBuffer[Int]()     
    val listsTest=extractVectorsMap(testVector) 
    for(listTest <- listsTest)
    {
      val listLength= listTest.length
      val nodes= new Array[svm_node](listLength)
      for( i <- 1 to listLength)
	  {
        val node= new svm_node()
	    node.index = i;
	    node.value = listTest(i-1)
	    nodes(i-1) = node
	  }
      val labels= new Array[Int](numberClasses)
	  svm.svm_get_labels(model,labels);
      val prob_estimates= new Array[Double](numberClasses)
      val Prediction: Double= svm.svm_predict_probability(model, nodes, prob_estimates)
      predictionList += Prediction.toInt
    }
    return predictionList.toList
  }
}//end class

