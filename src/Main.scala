import java.io.PrintWriter

import scala.math.min

object Main extends App{

  val foreignSwaps = Map("Á" -> "A", "É" -> "E", "Í" -> "I", "Ó" -> "O", "Ú" -> "U", "Â" -> "A", "Ê" -> "E", "Î" -> "I",
    "Ô" -> "O", "Û" -> "U", "À" -> "A", "È" -> "E", "Ù" -> "U", "Ä" -> "A", "Ë" -> "E", "Ï" -> "I",
    "Ö" -> "O", "Ü" -> "U", "Ÿ" -> "Y", "Ç" -> "C", "ẞ" -> "SS", "Ñ" -> "N", "Ã" -> "A")

  var total = 0
  var noRef = 0
  var zeroScores = 0
  var failMatches = 0
  var imperfectMatches = 0
  var perfectMatches = 0

  val inputFilePath = "/Users/Vincent/IdeaProjects/SimpleTest/src/spotify_titles.tsv"
  val outputFilePath = "/Users/Vincent/IdeaProjects/SimpleTest/src/stats.txt"
  val pw = new PrintWriter(outputFilePath)

  val linesIterator = io.Source.fromFile(inputFilePath).getLines
  val regex = """(.*)\t(.*)""".r
  val tupleIterator = linesIterator.flatMap(line => line match {
    case regex(inputTitle, refDataTitle) => Some((inputTitle, refDataTitle))
    case _ => None
  })

  val L = tupleIterator.toList

  L.foreach (e => {
    val t1 = e._1.substring(0,min(e._1.length,60)).toUpperCase
    val t2 = foreignSwaps.foldLeft(" " + t1) { case (t, (k, v)) => t.replaceAll(k, v) }
      .trim.replaceAll(" +", " ")
    val inputTitle = t2
    val refDataTitle = e._2
    pw.write("----------------------------\n")
    total += 1
    if (refDataTitle.isEmpty)
      noRef += 1
    else {
      val x = TitleCompare(inputTitle, refDataTitle)
      //if (x < 1) pw.write(s" ${inputTitle} <=> ${refDataTitle}  JW -> $x \n")
      pw.write(s" ${inputTitle} <=> ${refDataTitle}\n  JW -> $x \n")
      if (x == 0) zeroScores += 1
      else
      if (x < 0.93) failMatches += 1
      else
      if (x < 1) imperfectMatches += 1 else perfectMatches += 1
    }
  })

  pw.write(s"\nTotal = $total\n" +
    s"No Reference Title = $noRef\n" +
    s"Zero Scores = $zeroScores\n" +
    s"Failed Match = $failMatches\n" +
    s"Match - Imperfect  $imperfectMatches\n" +
    s"Match - Perfect = $perfectMatches")
  pw.close()

}