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
  var firstCompareCount = 0
  var compareAllCount = 0
  var noSplitCount = 0
  var compareAttempts = 0
  var collapsedCompareCount = 0
  var featCount = 0
  var liveCount = 0
  var jwEmptyTitles = 0
  var skippedTitles = 0
  var jwCalls = 0
  var jwExact = 0
  var introCount = 0
  var lastWinfKey = ""
  var matchFound = false
  var inputTitle = ""
  var winfKeyCount = 0
  var inputTitleCount = 0
  var lastInputTitle = ""
  //val inputFilePath = "/Users/Vincent/IdeaProjects/APRA-TitleCompare-Debug/src/spotify_titles.tsv"
  //  /home/k1/IdeaProjects/APRA-TitleCompare-Debug/src/spotify_titles.tsv
  val inputFilePath = "sorted-spotify-titles-1m.tsv"
  val debugFilePath = "debug-output.txt"
  val statsFilePath = "stats-output.txt"
 // val testFilePath = "test.txt"
  val dw = new PrintWriter(debugFilePath)
  val sw = new PrintWriter(statsFilePath)
  //val tw = new PrintWriter(testFilePath)
/*
  val linesIterator = io.Source.fromFile(inputFilePath).getLines
  val regex = """(.*)\t(.*)""".r
  val tupleIterator = linesIterator.flatMap(line => line match {
    case regex(inputTitle, refDataTitle) => Some((inputTitle, refDataTitle))
    case _ => None
  })
*/
  val linesIterator = io.Source.fromFile(inputFilePath).getLines
  val regex = """(.*)\t(.*)\t(.*)""".r
  val tupleIterator = linesIterator.flatMap(line => line match {
    case regex(inputTitle, refDataTitle, winfKey) => Some((inputTitle, refDataTitle, winfKey))
    case _ =>
      println(line)
      None
  })

  tupleIterator.foreach(e => {
    total += 1
    val (inputTitle, refDataTitle, winfKey) = e
    if (lastInputTitle.toUpperCase != inputTitle.toUpperCase) {
    //if (lastInputTitle != inputTitle) {
      lastInputTitle = inputTitle
      inputTitleCount += 1
    //  tw.write(s"$inputTitleCount\t$inputTitle\t$lastInputTitle\n")
    }
    if (inputTitle.toUpperCase != "INTRO") {
      if (lastWinfKey.isEmpty) lastWinfKey = winfKey //initialise lastWinfKey

      if (!matchFound)
          matchFound = !(processTitles(inputTitle, refDataTitle, winfKey) < 0.93)
        else if (lastWinfKey != winfKey) {
          lastWinfKey = winfKey
          matchFound = !(processTitles(inputTitle, refDataTitle, winfKey) < 0.93)
        } else
          skippedTitles += 1
      } else
      introCount += 1
  })

  def processTitles(t1: String, t2: String, t3: String): Double = {
    dw.write("------------------------------------------------------------------------------------\n")
    dw.write(s" $t1  <-->  $t2\n")
    dw.write("------------------------------------------------------------------------------------\n")
    val x = TitleCompare(t1.substring(0,min(t1.length,60)).toUpperCase, t2)
    if (x == 0)
      zeroScores += 1
    else
    //if (x < 0.93) { failMatches += 1; sw.write(s" ${inputTitle} <=> ${refDataTitle}\n$failMatches   (JW -> $x) \n") }
      if (x < 0.93)
        failMatches += 1
      else {
        dw.write(s"                                                              WINF_key =  $t3\n")
        if (x < 1) imperfectMatches += 1
        else perfectMatches += 1
        //sw.write(s" ${inputTitle} <=> ${refDataTitle}\n   (JW -> $x) \n")
      }
    x
  }

  val totalCompares = firstCompareCount+compareAllCount+collapsedCompareCount+featCount+liveCount
  val attemptedMatch = total-skippedTitles-introCount
  sw.write(s"\n$inputTitleCount\n")
  sw.write(s"\nTotal = $total\n" +
    s"Skipped Intro = $introCount\n" +
    s"Skipped Title = $skippedTitles\n" +
    s"Attempted Match = $attemptedMatch\n" +
    f"  ${noRef.toDouble/ attemptedMatch *100}%5.2f%%  No Reference Title = $noRef\n" +
    f"  ${jwEmptyTitles.toDouble/ attemptedMatch *100}%5.2f%%  Blank Title -> JW = $jwEmptyTitles%d\n" +
    f"  ${zeroScores.toDouble/ attemptedMatch *100}%5.2f%%  Zero Scores = $zeroScores%d\n" +
    f"  ${failMatches.toDouble/ attemptedMatch *100}%5.2f%%  Failed Match = $failMatches%d\n" +
    f"  ${imperfectMatches.toDouble/attemptedMatch*100}%5.2f%%  Match (Imperfect) = $imperfectMatches%d\n" +
    f"  ${perfectMatches.toDouble/attemptedMatch*100}%5.2f%%  Match (Perfect) = $perfectMatches%d\n" +
    f"  ${(perfectMatches+imperfectMatches).toDouble/attemptedMatch*100}%5.2f%%  Matched = ${imperfectMatches+perfectMatches}\n\n" +
    s"Successful Comparisons :\n" +
    f"  ${firstCompareCount.toDouble/totalCompares*100}%5.2f%%  FirstCompare = $firstCompareCount\n" +
    f"  ${compareAllCount.toDouble/totalCompares*100}%5.2f%%  CompareAll = $compareAllCount\n" +
    f"  ${collapsedCompareCount.toDouble/totalCompares*100}%5.2f%%  CollapsedCompare = $collapsedCompareCount\n" +
    f"  ${featCount.toDouble/totalCompares*100}%5.2f%%  Feat = $featCount\n" +
    f"  ${liveCount.toDouble/totalCompares*100}%5.2f%%  Live = $liveCount\n\n" +
    s"JW Called = $jwCalls\n" +
    f"- %% exact = ${jwExact.toDouble/jwCalls*100}%5.2f%%\n" +
    f"Split %% = ${(1-noSplitCount.toDouble/compareAttempts)*100}%5.2f%%\n"
  )
  sw.close()
  dw.close()
  //tw.close()
}