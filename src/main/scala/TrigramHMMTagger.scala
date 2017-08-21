import java.io._
import java.text.DecimalFormat

import io.circe.parser._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

object TrigramHMMTagger extends App {

  val StartToken: String = "<<StartToken>>"
  val StopToken: String = "<<StopToken>>"
  val HashTagTester = """#.*""".r
  val HashTagUnknownToken: String = "<<HashTagUnknown>>"
  val UrlTester = """(?i)http[s]?://.*|www\..*""".r
  val UrlUnknownToken: String = "<<UrlUnknown>>"
  val NumericTester = """.*\d+.*""".r
  val NumericUnknownToken: String = "<<NumericUnknownToken>>"
  val UsernameTester = """@.*""".r
  val UsernameUnknownToken: String = "<<UsernameUnknownToken>>"
  val ExistentialVerbalTester = """(?i)there's|theres'|alls'|all's""".r
  val ExistentialVerbalToken: String = "<<ExistentialVerbalToken>>"
  val ProperNounPossessiveTester = """[A-Z]+.*'s|[A-Z]+.*s'""".r
  val ProperNounPossessiveToken: String = "<<ProperNoundPossessiveToken>>"
  val NominalPossessiveTester = """[a-z]+.*'s|[a-z]+.*s'""".r
  val NominalPossessiveToken: String = "<<NominalPossessiveToken>>"
  val OtherUnknownToken: String = "<<OtherUnknown>>"

  val emissionProbabilityStr: String = "emissionProbability"
  val backPointerStr: String = "backPointer"
  val transitionProbabilityStr: String = "transitionProbability"
  val finalProbabilityStr: String = "finalProbability"
  val tagStr: String = "tag"
  val wordStr: String = "word"

  val UnknownThreshold: Int = 1

  var hyperParameters: Tuple3[Double, Double, Double] = (0.2, 0.7, 0.1)

  val dictionary: mutable.Map[String, Int] = mutable.Map[String, Int]()
  val dictionaryPosCount: mutable.Map[String, mutable.Map[String, Int]] = mutable.Map[String, mutable.Map[String, Int]]()
  val posUnigramCount: mutable.Map[String, Int] = mutable.Map[String, Int]()
  val posBigramCount: mutable.Map[String, mutable.Map[String, Int]] = mutable.Map[String, mutable.Map[String, Int]]()
  val posTrigramCount: mutable.Map[String, mutable.Map[String, mutable.Map[String, Int]]] = mutable.Map[String, mutable.Map[String, mutable.Map[String, Int]]]()
  var allUnknowns: mutable.Map[String, Int] = null
  val unknowns: mutable.Map[String, mutable.Map[String, Int]] = mutable.Map[String, mutable.Map[String, Int]]()
  var trainingSetWordCount: Int = 0

  val StartTime = System.currentTimeMillis()

  prepareModel(args(0))
  val (testingData, testingLabels) = loadTestingData(args(1))
  val predictedLabels = labelTestingData(testingData)
  evaluate(testingLabels, predictedLabels)
  writePredictedLabelsToDisk(testingData, predictedLabels)
  println((System.currentTimeMillis() - StartTime) / 1000)

  def writePredictedLabelsToDisk(testingData: List[List[String]], predictedLabels: List[List[String]]) = {
    val writer = new PrintWriter("trigramHmm-" + new File(System.currentTimeMillis() + ".json"))
    testingData.zip(predictedLabels).foreach({ case (wordList: List[String], labelList: List[String]) => {
      writer.print("[")
      var position = 0
      wordList.zip(labelList).foreach({ case (word: String, label: String) => {
        val wordSafe = word.replaceAllLiterally("\"", """\"""")
        writer.print(s"[\42$wordSafe\42, \42$label\42]")
        if (position < wordList.size - 1) {
          writer.print(", ")
          position = position + 1
        }
      }
      })
      writer.println("]")
    }
    })
    writer.close()
  }

  def evaluate(testingData: List[List[String]], labeledData: List[List[String]]) = {
    var incorrectMatches: Int = 0
    var incorrectMatchesByPos: mutable.Map[String, Int] = mutable.Map()

    var totalWords: Int = 0
    var totalWordsByPos: mutable.Map[String, Int] = mutable.Map()

    var posDistribution: mutable.Map[String, mutable.Map[String, Int]] = mutable.Map[String, mutable.Map[String, Int]]()

    testingData.zip(labeledData).foreach({ case (actualLine: List[String], predictedLine: List[String]) => {
      actualLine.zip(predictedLine).foreach({ case (actual: String, predicted: String) => {
        totalWords = totalWords + 1
        totalWordsByPos(actual) = totalWordsByPos.getOrElse(actual, 0) + 1
        posDistribution(actual) = posDistribution.getOrElseUpdate(actual, mutable.Map())
        posDistribution(actual)(predicted) = posDistribution(actual).getOrElse(predicted, 0) + 1
        if (actual != predicted) {
          incorrectMatches = incorrectMatches + 1
          incorrectMatchesByPos(actual) = incorrectMatchesByPos.getOrElse(actual, 0) + 1
        }
      }
      })
    }
    })

    val writer = new PrintWriter(s"distribution-${hyperParameters}" + new File(System.currentTimeMillis() + ".csv"))
    val tags = posUnigramCount.keySet.toList
    val df = new DecimalFormat("###.##")
    writer.println("\42tag\42," + tags.mkString("\42", "\42,\42", "\42"))
    posDistribution.foreach({ case (pos: String, distribution: mutable.Map[String, Int]) => {
      writer.print("\42" + pos + "\42")
      tags.foreach((tag: String) => {
        writer.print(",\42" + df.format((100.0 * distribution.getOrElse(tag, 0)) / totalWordsByPos(pos)) + "\42")
      })
      writer.println()
    }
    })

    val totalAccuracy = df.format((100.0 * (totalWords - incorrectMatches)) / totalWords)
    writer.print("totalAccuracy," + totalAccuracy)

    writer.close()
    println("Total Accuracy: " + totalAccuracy)

    println("Breakdown by POS Tag:")
    totalWordsByPos.keySet.foreach({ case (posTag: String) => {
      println(posTag + ": " + df.format((100.0 * (totalWordsByPos(posTag) - incorrectMatchesByPos.getOrElse(posTag, 0))) / totalWordsByPos(posTag)))
    }
    })
  }

  def labelTestingData(testingData: List[List[String]]): List[List[String]] = {
    var labeledData: ListBuffer[List[String]] = ListBuffer[List[String]]()
    testingData.zipWithIndex.foreach({ case (line: List[String], linePosition: Int) => {
      val lookupTable: mutable.Map[Int, mutable.Map[String, mutable.Map[String, Any]]] = mutable.Map[Int, mutable.Map[String, mutable.Map[String, Any]]]()
      line.zipWithIndex.foreach({ case (word: String, wordPosition: Int) => {
        lookupTable(wordPosition) = mutable.Map[String, mutable.Map[String, Any]]()
        posUnigramCount.keySet.filter((pos: String) => pos != StopToken).foreach((previousPosTag: String) => {
          posUnigramCount.keySet.filter((pos: String) => pos != StopToken).foreach((posTag: String) => {
            val currentTagPair = previousPosTag + posTag
            lookupTable(wordPosition)(currentTagPair) = mutable.Map[String, Any]()
            lookupTable(wordPosition)(currentTagPair)(wordStr) = word
            lookupTable(wordPosition)(currentTagPair)(tagStr) = currentTagPair
            lookupTable(wordPosition)(currentTagPair)(emissionProbabilityStr) = Math.log((1.0 * dictionaryPosCount.getOrElse(word, mutable.Map()).getOrElse(posTag, 0)) / posUnigramCount.get(posTag).get) / Math.log(2.0)
            lookupTable(wordPosition)(currentTagPair)(transitionProbabilityStr) = Double.MinValue
            if (wordPosition == 0) {
              lookupTable(wordPosition)(currentTagPair)(transitionProbabilityStr) = trigramProbability(StartToken, StartToken, posTag)
            }
            else if (wordPosition == 1) {
              lookupTable(wordPosition)(currentTagPair)(transitionProbabilityStr) = trigramProbability(StartToken, previousPosTag, posTag) + lookupTable(wordPosition - 1)(previousPosTag + previousPosTag)(finalProbabilityStr).asInstanceOf[Double]
              lookupTable(wordPosition)(currentTagPair)(backPointerStr) = lookupTable(wordPosition - 1)(previousPosTag + previousPosTag)
            }
            else {
              posUnigramCount.keySet.filter((pos: String) => pos != StopToken).foreach((penultimatePosTag: String) => {
                val transitionProbability = trigramProbability(penultimatePosTag, previousPosTag, posTag) + lookupTable(wordPosition - 1)(penultimatePosTag + previousPosTag)(finalProbabilityStr).asInstanceOf[Double]
                if (transitionProbability > lookupTable(wordPosition)(currentTagPair)(transitionProbabilityStr).asInstanceOf[Double]) {
                  lookupTable(wordPosition)(currentTagPair)(transitionProbabilityStr) = transitionProbability
                  lookupTable(wordPosition)(currentTagPair)(backPointerStr) = lookupTable(wordPosition - 1)(penultimatePosTag + previousPosTag)
                }
              })
            }
            if (wordPosition == line.size - 1) {
              lookupTable(wordPosition)(currentTagPair)(transitionProbabilityStr) = lookupTable(wordPosition)(currentTagPair)(transitionProbabilityStr).asInstanceOf[Double] + trigramProbability(previousPosTag, posTag, StopToken)
            }

            lookupTable(wordPosition)(currentTagPair)(finalProbabilityStr) = lookupTable(wordPosition)(currentTagPair)(emissionProbabilityStr).asInstanceOf[Double] + lookupTable(wordPosition)(currentTagPair)(transitionProbabilityStr).asInstanceOf[Double]
          })
        })
      }
      })
      val winningCell = lookupTable(line.size - 1).valuesIterator.reduceLeft((x, y) => if (x(finalProbabilityStr).asInstanceOf[Double] > y(finalProbabilityStr).asInstanceOf[Double]) x else y)
      labeledData += backTrack(lookupTable, winningCell)
    }
    })
    return labeledData.toList
  }

  def backTrack(lookupTable: mutable.Map[Int, mutable.Map[String, mutable.Map[String, Any]]], winningCell: mutable.Map[String, Any]): List[String] = {
    var wordsWithPos: ListBuffer[String] = ListBuffer[String]()
    wordsWithPos += winningCell(tagStr).asInstanceOf[String].takeRight(1)
    var currentTagPair = winningCell(tagStr).asInstanceOf[String]
    for (index <- (1 to lookupTable.size - 1).reverse) {
      val backPointer = lookupTable(index)(currentTagPair)(backPointerStr).asInstanceOf[mutable.Map[String, Any]]
      wordsWithPos += backPointer(tagStr).asInstanceOf[String].takeRight(1)
      currentTagPair = backPointer(tagStr).asInstanceOf[String]
    }
    return wordsWithPos.reverse.toList
  }

  def trigramProbability(penultimatePosTag: String, previousPosTag: String, posTag: String): Double = {
    val trigramBase: Double = if (penultimatePosTag == StartToken) posUnigramCount(StopToken) else posBigramCount.getOrElse(penultimatePosTag, mutable.Map()).getOrElse(previousPosTag, 1).asInstanceOf[Double]
    val bigramBase: Double = if (penultimatePosTag == StartToken) posUnigramCount(StopToken) else posUnigramCount(previousPosTag)
    val trigramScore: Double = (hyperParameters._1 * posTrigramCount.getOrElse(penultimatePosTag, mutable.Map()).getOrElse(previousPosTag, mutable.Map()).getOrElse(posTag, 0)) / trigramBase
    val bigramScore: Double = (hyperParameters._2 * posBigramCount.getOrElse(previousPosTag, mutable.Map()).getOrElse(posTag, 0)) / bigramBase
    val unigramScore: Double = (hyperParameters._3 * posUnigramCount(posTag)) / trainingSetWordCount

    return Math.log(trigramScore + bigramScore + unigramScore) / Math.log(2.0)
  }

  def loadTestingData(testingDataFilePath: String): (List[List[String]], List[List[String]]) = {
    var words = ListBuffer[List[String]]()
    var labels = ListBuffer[List[String]]()
    for (line <- Source.fromFile(testingDataFilePath).getLines()) {
      val parsedLine: List[List[String]] = parse(line).right.get.as[List[List[String]]].right.get
      words += parsedLine.map((wordPair: List[String]) => if (dictionary.contains(wordPair(0))) wordPair(0) else {
        wordPair(0) match {
          case UrlTester() => UrlUnknownToken
          case UsernameTester() => UsernameUnknownToken
          case HashTagTester() => HashTagUnknownToken
          case NumericTester() => NumericUnknownToken
          case ExistentialVerbalTester() => ExistentialVerbalToken
          case ProperNounPossessiveTester() => ProperNounPossessiveToken
          case NominalPossessiveTester() => NominalPossessiveToken
          case _ => OtherUnknownToken
        }
      })
      labels += parsedLine.map((wordPair: List[String]) => wordPair(1))

    }
    return (words.toList, labels.toList)
  }

  def prepareModel(trainingDataFilePath: String) = {

    for (line <- Source.fromFile(trainingDataFilePath).getLines()) {

      val parsedLine: List[List[String]] = parse(line).right.get.as[List[List[String]]].right.get
      val adjustedLine: List[List[String]] = (List(List(StartToken, StartToken), List(StartToken, StartToken)) ++ parsedLine ::: List(List(StopToken, StopToken)))

      adjustedLine.zipWithIndex.foreach({ case (wordPosPair: List[String], position: Int) => {
        if (position > 1) {
          val word = wordPosPair(0)
          val posTag = wordPosPair(1)
          val previousPosTag = adjustedLine(position - 1)(1)
          val penultimatePosTag = adjustedLine(position - 2)(1)

          // Adding word to dictionary or increasing count if already present.
          dictionary(word) = dictionary.getOrElse(word, 0) + 1

          // Adding (word, posTag) to dictionaryPosCount or increasing count if already present.
          val dictionaryPosMap = dictionaryPosCount.getOrElseUpdate(word, mutable.Map[String, Int]())
          dictionaryPosMap(posTag) = dictionaryPosMap.getOrElse(posTag, 0) + 1
          dictionaryPosCount(word) = dictionaryPosMap

          // Adding posTag to posUnigramCount or increasing count if already present.
          posUnigramCount(posTag) = posUnigramCount.getOrElse(posTag, 0) + 1

          // Adding (previousPosTag, posTag) to posBigramCount or increasing count if already present.
          val bigramMap = posBigramCount.getOrElseUpdate(previousPosTag, mutable.Map[String, Int]())
          bigramMap(posTag) = bigramMap.getOrElse(posTag, 0) + 1
          posBigramCount(previousPosTag) = bigramMap

          // Adding (penultimatePosTag, previousPosTag, posTag) to posTrigramCount or increasing count if already present.
          val trigramMap = posTrigramCount.getOrElseUpdate(penultimatePosTag, mutable.Map[String, mutable.Map[String, Int]]()).getOrElseUpdate(previousPosTag, mutable.Map[String, Int]())
          trigramMap(posTag) = trigramMap.getOrElse(posTag, 0) + 1
          posTrigramCount(penultimatePosTag)(previousPosTag) = trigramMap
        }
      }
      })
    }

    // Find the unknowns.
    allUnknowns = dictionary.filter({ case (word: String, count: Int) => (count <= UnknownThreshold) })

    allUnknowns.keySet.foreach { word =>
      word match {
        case UrlTester() => {
          val count = unknowns.getOrElseUpdate(UrlUnknownToken, mutable.Map()).getOrElse(word, 0) + 1
          unknowns(UrlUnknownToken)(word) = count
        }
        case UsernameTester() => {
          val count = unknowns.getOrElseUpdate(UsernameUnknownToken, mutable.Map()).getOrElse(word, 0) + 1
          unknowns(UsernameUnknownToken)(word) = count
        }
        case HashTagTester() => {
          val count = unknowns.getOrElseUpdate(HashTagUnknownToken, mutable.Map()).getOrElse(word, 0) + 1
          unknowns(HashTagUnknownToken)(word) = count
        }
        case NumericTester() => {
          val count = unknowns.getOrElseUpdate(NumericUnknownToken, mutable.Map()).getOrElse(word, 0) + 1
          unknowns(NumericUnknownToken)(word) = count
        }
        case ExistentialVerbalTester() => {
          val count = unknowns.getOrElseUpdate(ExistentialVerbalToken, mutable.Map()).getOrElse(word, 0) + 1
          unknowns(ExistentialVerbalToken)(word) = count
        }
        case ProperNounPossessiveTester() => {
          val count = unknowns.getOrElseUpdate(ProperNounPossessiveToken, mutable.Map()).getOrElse(word, 0) + 1
          unknowns(ProperNounPossessiveToken)(word) = count
        }
        case NominalPossessiveTester() => {
          val count = unknowns.getOrElseUpdate(NominalPossessiveToken, mutable.Map()).getOrElse(word, 0) + 1
          unknowns(NominalPossessiveToken)(word) = count
        }
        case _ => {
          val count = unknowns.getOrElseUpdate(OtherUnknownToken, mutable.Map()).getOrElse(word, 0) + 1
          unknowns(OtherUnknownToken)(word) = count
        }
      }
    }

    unknowns.foreach({ case (unknownType: String, unks: mutable.Map[String, Int]) => {
      dictionary(unknownType) = unks.values.sum
      val unknownPosCount = mutable.Map[String, Int]()
      unks.keySet.foreach((word: String) => {
        val posCounts = dictionaryPosCount(word)
        posCounts.foreach({ case (posTag, count) => {
          unknownPosCount(posTag) = unknownPosCount.getOrElse(posTag, 0) + count
        }
        })
        dictionary.remove(word)
        dictionaryPosCount.remove(word)
      })
      dictionaryPosCount(unknownType) = unknownPosCount
    }
    })

    trainingSetWordCount = posUnigramCount.values.sum
  }
}
