 TINGA

Tinga is an scala library for Natural Language Processing (NLP). Also, it integrates some Machine Learning (ML) libraries. Furthermore, some applications of NLP and ML like Sentiment Polarity detections (Opinion Mining) are integrated as modules of tinga. Currently we have developed three modules: 

  - Natural Language Processing 
  - Machine Learning
  - Sentiment Analysis

### Natural Language Processing
Natural Language Processing module have some basi tools for NLP typical tasks:
* Text Preprocessing: remove stopwords, diacritics and punctuation, enable or disable language-specific characters. Currently we can handle spanish, english, french, italian and german.
* Tokenizer: in tinga the basic token is the Word. Word is considered as the atomic unit of text. After word comes the Sentence token and finally Paragraph token.
* Part-of-Speech Tagger: Every word token is labeled with its PoS tag. But it is also easy to create your own tokens and tag them with our Brill's algorithm implementation of PoS Tagger.
* Spell-Checker: fast and easy implementation of Norvig spellchecking algorithm (bayes theorem based).
* Featurizer: frequency of ngrams, skipgrams and vocabulary from text.
* SentimentTagger: it tags sentiment words with its PoS Tag.

### Machine Learning
Machine learning classic task are classification and clusterization. At this point, we only have integrated libSVM (Support Vector Machine library):
* Classification with SVM (kernel selection available: LINEAR, POLYNOMIAL, RBF, SIGMOID).
* Regression with SVM (kernel selection available: LINEAR, POLYNOMIAL, RBF, SIGMOID).

### Sentiment Analysis
Sentiment analysis is a library for opinion mining (also known as sentiment analysis) of spanish comments (until this version, later version will detect on other languages). It analizes a comment, tweet, post or sentence and determines sentiment polarity such as negative, positive or neutral according to the vocabulary used. With this library you can:
* Analize a comment and obtain global sentiment polarity
* Detect sentiment per sentence
* Detect global sentiment (per comment)
* Obtain keywords used in sentiment opinion

Example (spanish text):
> Me encanta el nuevo iphone 6. Lo único malo es el precio. 

Me encanta el nuevo iphone 6 (positive). Lo único malo es el precio (negative). Overall Sentiment (neutral). Keywords in first sentence "nuevo iphone", while in second sentence is "precio". Therefore, positive sentiment for iphone negative for price.

### Version
0.1.1


### Using the API
#####In this section (NLP and ML use)

##### Sentiment Analysis
:)
```scala
scala> import tinga.sentiment._

scala> val s = new Sentiment("es")
scala> s.sentiment("Me encanta el nuevo iphone. Lo malo es el precio")
res0: scala.collection.mutable.Buffer[(String, Double, String, Int)] = ArrayBuffer((Me encanta el nuevo iphone .,1.0,sentiment,1), (" Lo malo es el precio .",-1.0,sentiment,1))
scala>
scala> s.globalParagraphScore(res0)
res1: (String, Double, String, Int) = (Me encanta el nuevp iphone . Lo malo es el precio .,0.0,sentiment,1)
scala>
scala> s.wordCloud("Me encanta el nuevo iphone. Lo malo es el precio")
res2: scala.collection.mutable.Buffer[String] = ArrayBuffer(nuevo iphone, precio)
```
### Installation
```sh
$ git clone tinga 
```


### Dependencies

Currently the unique dependency is "libsvm" and is unmanaged and included in source directory


### Development

Want to contribute? Great!

### Todo's

 - Write Tests
 - Rethink Github Save
 - Add Code Comments
 - Add Night Mode

License
----

MIT


**Free Software, Hell Yeah!**

[john gruber]:http://daringfireball.net/
[@thomasfuchs]:http://twitter.com/thomasfuchs
[1]:http://daringfireball.net/projects/markdown/
[marked]:https://github.com/chjj/marked
[Ace Editor]:http://ace.ajax.org
[node.js]:http://nodejs.org
[Twitter Bootstrap]:http://twitter.github.com/bootstrap/
[keymaster.js]:https://github.com/madrobby/keymaster
[jQuery]:http://jquery.com
[@tjholowaychuk]:http://twitter.com/tjholowaychuk
[express]:http://expressjs.com
[AngularJS]:http://angularjs.org
[Gulp]:http://gulpjs.com
