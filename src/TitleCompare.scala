
import Main.dw

import scala.util.Try
import math.max

object TitleCompare {

  case class MatchTokens(title: Option[String], cleanTitle: Option[String], numeral: Option[String], keySignature: Option[String])

  object MatchTokens {
    def apply(): MatchTokens = MatchTokens(None, None, None, None)
  }

  case class MatchSet(main: MatchTokens, split1: MatchTokens, split2: MatchTokens)

  def fuzzyMatch(s1: String, s2: String): Double = CustomJaroWinkler(s1, s2)

  def isAcceptableScore(score: Double): Boolean = score >= 0.93

  final val PerfectScore: Double = 1d

  final val WorstScore: Double = 0d

  // compareTitles is called for every (input title, reference data title) pair to be tested for a possible match
  // It's assumed for this implementation is that the calling application has already generated the matching tokens
  // for the input title - using Titles.createTuples
  // As the same input title is used for multiple reference data titles
  // A Jaro-Winkler score is returned in the range 0 to 1, where 1 indicates an exact match and 0 a complete mismatch
  def compareTitles(inputSet: MatchSet, refDataTitle: String): Double = {
    // found a match with firstCompare so nothing more to do => return empty set for tokens


    inputSet.main.title.map(t => firstCompare(t, refDataTitle)) match {
      case Some(doesMatch) if doesMatch =>
        Main.firstCompareCount += 1
        Main.dw.write(" Matched at First Compare\n")
        return PerfectScore
      case _ =>
    }

    val refDataSet = createTuples(refDataTitle)
    var fuzzyScore: Double = compareAllTitles(inputSet, refDataSet)

    if (isAcceptableScore(fuzzyScore)) { Main.CompareAllCount += 1; return fuzzyScore }

    val zipped: Iterable[(String, String)] = inputSet.main.cleanTitle.zip(refDataSet.main.cleanTitle)
    //val yipped: Iterable[(String, String)] = inputSet.main.title.zip(refDataSet.main.title)

    val feat: Option[Double] = zipped.flatMap { case (s1, s2) => checkFeat(s1, s2) }.headOption

    fuzzyScore = max(fuzzyScore, feat.getOrElse(WorstScore))
    if (isAcceptableScore(fuzzyScore)) return fuzzyScore

    val live: Option[Double] = zipped.flatMap { case (s1, s2) => checkLive(s1, s2) }.headOption

    fuzzyScore = max(fuzzyScore, live.getOrElse(WorstScore))
    if (isAcceptableScore(fuzzyScore))
      fuzzyScore
    else {
      val ccScore = inputSet.main.title.zip(refDataSet.main.title)
        .map { case (s1, s2) => collapsedCompare(s1, s2) }
        .headOption
        .getOrElse(WorstScore)
      if (isAcceptableScore(ccScore)) Main.collapsedCompareCount += 1
      max(fuzzyScore, ccScore)
    }
  }

  // checks if title needs to be split into two titles to increase probability of matching
  def createTuples(title: String): MatchSet =
    if (title.startsWith("(")) {
      // split required so generate cleaned titles and numeral strings and key signature strings
      generateSplits(title) match {
        case (s1, s2) => MatchSet(MatchTokens(Some(title), None, None, None), s1, s2)
      }
    } else {
      // no splits required so set split tokens to empty
      getOtherTokens(title) match {
        case (o1, o2, o3) => MatchSet(MatchTokens(Some(title), o1, o2, o3), MatchTokens(), MatchTokens())
      }
    }

  // check if titles are an exact match, then
  // check left-sides of parenthesis partial title for exact match
  // check if there's an exact match when the title are stripped of all chars except alphanumeric
  private def firstCompare(inpTitle: String, refTitle: String): Boolean = {
    def stripAll(title: String): String = title.replaceAll("[^0-9A-Z]", "")

    def testPartials(title1: String, title2: String): Boolean =
      title1.split("""\(""").head == title2.split("""\(""").head

    (inpTitle == refTitle) || testPartials(inpTitle, refTitle) || (stripAll(inpTitle) == stripAll(refTitle))
  }

  private def compareAllTitles(inputSet: MatchSet, refDataSet: MatchSet): Double =
    (inputSet.main.title.map(_.take(1)), refDataSet.main.title.map(_.take(1))) match {
      case (Some("("), Some("(")) =>
        Seq(
          getFuzzyScore(inputSet.split1, refDataSet.split1),
          getFuzzyScore(inputSet.split1, refDataSet.split2),
          getFuzzyScore(inputSet.split2, refDataSet.split1),
          getFuzzyScore(inputSet.split2, refDataSet.split2)
        ).max
      case (Some("("), _) =>
        max(getFuzzyScore(inputSet.split1, refDataSet.main), getFuzzyScore(inputSet.split2, refDataSet.main))
      case (_, Some("(")) =>
        max(getFuzzyScore(inputSet.main, refDataSet.split1), getFuzzyScore(inputSet.main, refDataSet.split2))
      case (_, _) =>
        if (inputSet.main.cleanTitle == refDataSet.main.cleanTitle)
          {
            Main.noFuzzyCount += 1
            PerfectScore
          }
        else
        getFuzzyScore(inputSet.main, refDataSet.main)
    }

  private def getFuzzyScore(m1: MatchTokens, m2: MatchTokens): Double = {
    m1.cleanTitle.zip(m2.cleanTitle).map {
      case (ct1, ct2) if testNumeralsAndKeySignatures(m1, m2) =>
        val x = fuzzyMatch(ct1, ct2) //remove
        Main.dw.write(s" $ct1  ~fuzz~  $ct2   (JW ->  $x)\n")   //remove
        //fuzzyMatch(ct1, ct2)
        x   //remove
      case _ => WorstScore
    }.headOption.getOrElse(WorstScore)
  }
  private def checkFeat(title1: String, title2: String): Option[Double] =
    if (title1.contains(" FEAT ") || title2.contains(" FEAT "))
      Some(fuzzyMatch(title1.split(" FEAT ").head, title2.split(" FEAT ").head))
    else
      None

  private def checkLive(title1: String, title2: String): Option[Double] = {
    val foldF = (s: String) => ReplacementWords.DeLiveList.foldLeft(s) { (t, d) => t.split(d).head }
    val (newTitle1, newTitle2) = (foldF(title1), foldF(title2))
    if ((newTitle1 != title1) || (newTitle2 != title2))
      Some(fuzzyMatch(newTitle1, newTitle2))
    else
      None
  }

  private def collapsedCompare(title1: String, title2: String): Double = {
    def checkStartParenthesis(title: String): String = {
      val newTitle =
        if (title.startsWith("("))
          title.take(1) + title.tail.split("""\(""").head
        else
          title.split("""\(""").head
      newTitle.replaceAll("[^A-Z0-9]|(S\\s)|(S$)", "")
    }

    val x: Double = fuzzyMatch(checkStartParenthesis(title1), checkStartParenthesis(title2))
    Main.dw.write(s" ${checkStartParenthesis(title1)}  -coll-  ${checkStartParenthesis(title2)}   (JW ->  $x)\n")
    x
  }

  private def testNumeralsAndKeySignatures(m1: MatchTokens, m2: MatchTokens): Boolean = {
    if (m1.keySignature.isEmpty && m2.keySignature.isEmpty) {
      val regexF = (m: MatchTokens) => m.cleanTitle.flatMap(t => """(.*)([\(| \(])(PART)([\) |\)])(.*)""".r.findFirstIn(t)).isDefined
      val x = !(regexF(m1) || regexF(m2)) || (m1.numeral == m2.numeral)
      val y = if (x) "same" else "diff"
      if (regexF(m1) || regexF(m2)) Main.dw.write(s" found PART - testing numerals :  $y\n")
      else Main.dw.write(s" no PART nor Classical\n")
      x
    }
    else {
      val x = (m1.keySignature == m2.keySignature) && (m1.numeral == m2.numeral)
      val y = if (x) "same" else "diff"
      Main.dw.write(s" Classical - testing key sig and numerals:  $y\n")
      x
    }
   // val regexF = (m: MatchTokens) => m.cleanTitle.flatMap(t => """(.*)([\(| \(])(PART)([\) |\)])(.*)""".r.findFirstIn(t)).isDefined
   // val z = (m1.numeral != m2.numeral) || !((m1.keySignature != m2.keySignature) || !(regexF(m1) || regexF(m2)))
   // z
  }
/*
  private def testNumerals(m1: MatchTokens, m2: MatchTokens): Boolean = {
    val regexF = (m: MatchTokens) => m.cleanTitle.flatMap(t => """(.*)([\(| \(])(PART)([\) |\)])(.*)""".r.findFirstIn(t)).isDefined
    !(regexF(m1) || regexF(m2)) || (m1.numeral == m2.numeral)
  }
*/

  // generate all split titles strings (tokens) used to test against reference data tokens
  private def generateSplits(title: String): (MatchTokens, MatchTokens) = {
    // check for right parenthesis in title and grab text either side  e.g  "( textLeft ) textRight "
    val (title1: Option[String], title2: Option[String]) =
      if (title.contains(")"))
        getLeftRight(title).map {
          // construct the two (split) titles from the returned tuple
          case (s1, s2) => (Some(s"${s1.trim} ${s2.trim}"), Option(s2.trim).filter(_.nonEmpty))
        }.getOrElse((None, None))
      else
        (Try(title.tail).toOption, None)
    // maybe the right parenthesis was missing?
    (title1, title2) match {
      case (Some(t1), Some(t2)) => (getOtherTokens(t1), getOtherTokens(t2)) match {
        case ((o1, o2, o3), (s1, s2, s3)) =>
          (MatchTokens(title1, o1, o2, o3), MatchTokens(title2, s1, s2, s3))
      }
      case (Some(t1), None) => getOtherTokens(t1) match {
        case (o1, o2, o3) => (MatchTokens(title1, o1, o2, o3), MatchTokens())
      }
      case (None, Some(t2)) => getOtherTokens(t2) match {
        case (o1, o2, o3) => (MatchTokens(), MatchTokens(title2, o1, o2, o3))
      }
      case _ => (MatchTokens(), MatchTokens())
    }
  }

  private def getLeftRight(title: String): Option[(String, String)] = Try {
    val regex = """(?:\()(.*?)(?:\))(.*)""".r
    val regex(textLeft, textRight) = title
    (textLeft, textRight)
  }.toOption

  private def getOtherTokens(title: String): (Option[String], Option[String], Option[String]) = {
    val cleanF = deNoiseAndSwaps _ andThen insOPUS
    val cleanTitle = cleanF(title)
    Main.dw.write(s"         title  $title\n   clean title  $cleanTitle\n")
    // if classical work then generate a key signature and remove signature from title
    val (keyTitle, keySig) = getKeySignature(cleanTitle)
    val y = keySig.getOrElse("")  // remove
    val cleanerTitle = cleanWords(keyTitle.getOrElse(""))
    Main.dw.write(s" cleaner title  $cleanerTitle\n")
    // generate a string composed of all digits in the title
    val numStr = cleanerTitle.replaceAll("[^0-9]", "")
    // remove anything to the right of left parenthesis
    val Some(x) = Option(cleanerTitle).flatMap(splitStripAndDeDouble)
    Main.dw.write(s" cleaned title  $x\n key =>  $y   numStr =>  $numStr\n")
    (Option(cleanerTitle).flatMap(splitStripAndDeDouble), Option(numStr).filter(_.nonEmpty), keySig)

  }

  private def deNoiseAndSwaps(title: String): String = {
    // TODO: This does not strictly conform to STRCOMPT but it works
    val cleanF = ((t: String) => if (t.startsWith("L'")) t.replaceAll("L'", "") else t)
      .andThen((t: String) => ReplacementWords.DeNoiseList.foldLeft(t) { case (t1, n) => t1.replaceAll(n, "") })
    // book-end title with spaces to find instances to replace at the beginning and end of the title
    ReplacementWords.WordSwapMap.foldLeft(" " + cleanF(title) + " ") { case (t, (k, v)) => t.replaceAll(k, v) }
      .trim
      .replaceAll("""\s+""", " ")
  }

  private def cleanWords(title: String): String = {
    val pipelineF = ((s: String) => s.split(" ").toList.filterNot(_.trim.equals("")))
      // remove all articles - including foreign articles e.g. le, la, el, uno, eine, ...
      .andThen((lst: List[String]) => lst.filterNot(ReplacementWords.ArticlesList.contains))
      .andThen((lst: List[String]) => if (lst.take(2).mkString == "THEMEFROM") lst.drop(2) else lst)
      .andThen((lst: List[String]) => if (lst.takeRight(1).mkString == "THEME") lst.dropRight(1) else lst)

    def extractInParentheses(word: String): (String, String, String) = {
      val regex = """(\(?)(.*?)(\)?)""".r
      val regex(l, w, r) = word
      (l, w, r)
    }

    def parenthesesSwap(map: Map[String, String])(word: String): String = {
      val (l, w, r) = extractInParentheses(word)
      l + map.getOrElse(w, w) + r
    }

    def dropLastS(word: String): String = {
      val (l, w, r) = extractInParentheses(word)
      if ((w != "OPUS") && w.endsWith("S"))
        l + w.replaceAll(".$", "") + r
      else
        l + w + r
    }
    // replace all number words or roman numerals with digits
    pipelineF(title)
      .map(parenthesesSwap(ReplacementWords.NumberSwapMap))
      .map(parenthesesSwap(ReplacementWords.RomanNumeralSwapMap))
      .map(dropLastS)
      // TODO: haven't addressed "-IZE" to "-ISE". Did when smoothed title
      .map(parenthesesSwap(ReplacementWords.ContractionSwapMap))
      .mkString(" ")
  }

  private def insOPUS(title: String): String =
    """( OP | OP)(NO|[0-9])""".r.findFirstIn(s" $title")
      .map(_ => title.replaceFirst("""\s+OP\s?+""", " OPUS "))
      .getOrElse(title)

  private def getKeySignature(title: String): (Option[String], Option[String]) = {
    //strip parenthesis and split
    val words = title.replaceAll("""\(""", "")
      .replaceAll("""\)""", "")
      .split(" ")

    // check for classical words in the title
    if (!words.exists(ReplacementWords.ClassicalWordsList.contains)) return (Some(title), None)

    val noteRegex = ".*(?: IN )([A-G])(.*)".r
    title match {
      // if " IN " is found it's saved in conj, the note (from A to G) for the key signature in note, and rhs saves
      // the trailing characters of the title for further checking
      case noteRegex(note, rhs) =>
        if (rhs.isEmpty) (Some(title.replace(s" IN $note", "")), Some(note))
        //  otherwise, check that it's really part of a key and not just part of the title  e.g.  SYMPHONY IN BLUE
        //  - where the 'B' in BLUE is detected as the key
        else if (rhs.startsWith(" ")) {
          val acc = "( SHARP| FLAT)".r.findFirstIn(title).getOrElse("") // look for SHARP or FLAT
          val tone = "( MINOR| MAJOR| MIN| MAJ)".r.findFirstIn(title).getOrElse("") // look for MINOR/MIN  or MAJOR/MAJ
          val key = if (tone.startsWith(" MA")) note + acc else note + acc + tone
          (Some(title.replace(s" IN $note$acc$tone", "")), Some(key))
          }
          else
            (Some(title), Some("no key"))
      case _ => (Some(title), Some("no key"))  // found classical word but no key signature
    }
  }

  private def splitStripAndDeDouble(title: String): Option[String] =
  // *** title 2 complies with COBOL code but doesn't remove excess whitespace
  // *** - this line does remove excess whitespace ...
  // *** val cleanedTitle = title3.replaceAll("""[^0-9A-Z&\s]""","").trim.replaceAll(" +", " ")
    title.split("""\(""").headOption
      .map(_.replaceAll("""[^0-9A-Z&\s]""", ""))
      .map { cleanedTitle =>
        """(\D)\1+""".r.findAllIn(cleanedTitle).toList
          .foldLeft(cleanedTitle) { (t, d) => t.replaceAll(d, d.tail) }
          .trim
      }

 //  def compare(s1: String, s2: String): Double = compareTitles(createTuples(s1), s2)
   def apply(s1: String, s2: String): Double = compareTitles(createTuples(s1), s2)
}
