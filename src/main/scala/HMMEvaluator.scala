import java.io.{File, PrintWriter}
import java.text.DecimalFormat

import io.circe.parser._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

object HMMEvaluator extends App {

  val StartTime = System.currentTimeMillis()
  evaluate(args(0), args(1))
  println("Took " + ((System.currentTimeMillis() - StartTime) / 1000) + " seconds")

  def evaluate(actualDataFilepath: String, predictedDataFilepath: String) = {
    var actualPOSTags = ListBuffer[List[String]]()
    var predictedPOSTags = ListBuffer[List[String]]()
    for (line <- Source.fromFile(actualDataFilepath).getLines()) {
      val parsedLine: List[List[String]] = parse(line).right.get.as[List[List[String]]].right.get
      actualPOSTags += parsedLine.map((wordPair: List[String]) => wordPair(1))
    }

    for (line <- Source.fromFile(predictedDataFilepath).getLines()) {
      val parsedLine: List[List[String]] = parse(line).right.get.as[List[List[String]]].right.get
      predictedPOSTags += parsedLine.map((wordPair: List[String]) => wordPair(1))
    }

    var incorrectMatches: Int = 0
    var incorrectMatchesByPos: mutable.Map[String, Int] = mutable.Map()

    var totalWords: Int = 0
    var totalWordsByPos: mutable.Map[String, Int] = mutable.Map()

    var posDistribution: mutable.Map[String, mutable.Map[String, Int]] = mutable.Map[String, mutable.Map[String, Int]]()

    actualPOSTags.zip(predictedPOSTags).foreach({ case (actualLine: List[String], predictedLine: List[String]) => {
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

    val df = new DecimalFormat("###.##")
    val totalAccuracy = df.format((100.0 * (totalWords - incorrectMatches)) / totalWords)

    println("Total Accuracy: " + totalAccuracy)

    println("Breakdown by POS Tag:")
    totalWordsByPos.keySet.foreach({ case (posTag: String) => {
      println(posTag + ": " + df.format((100.0 * (totalWordsByPos(posTag) - incorrectMatchesByPos.getOrElse(posTag, 0))) / totalWordsByPos(posTag)))
    }
    })

    val writer = new PrintWriter(s"distribution-" + new File(System.currentTimeMillis() + ".csv"))
    val tags = totalWordsByPos.keySet.toList
    writer.println("\42tag\42," + tags.mkString("\42", "\42,\42", "\42"))
    posDistribution.foreach({ case (pos: String, distribution: mutable.Map[String, Int]) => {
      writer.print("\42" + pos + "\42")
      tags.foreach((tag: String) => {
        writer.print(",\42" + df.format((100.0 * distribution.getOrElse(tag, 0)) / totalWordsByPos(pos)) + "\42")
      })
      writer.println()
    }
    })
    writer.close()
  }
}
