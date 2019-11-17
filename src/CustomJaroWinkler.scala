
import scala.annotation.tailrec
import scala.collection.mutable
import scala.math.{max, min}
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}

object CustomJaroWinkler {
  //  @transient private final val logger: Logger = LoggerFactory.getLogger(this.getClass)
  private val regexStore: mutable.Map[String, Regex] = mutable.HashMap[String, Regex]()

  private def getRegex(pattern: String) = {
    if (!regexStore.contains(pattern))
      regexStore += (pattern -> pattern.r)
    regexStore(pattern)
  }

  private def generateIndexArrays(minStr: String, maxStr: String): (Array[Boolean], Array[Boolean], Int) = {
    val matchingDistance = max((maxStr.length / 2) - 1, 0)
    val minIndexes = Array.fill[Boolean](minStr.length)(false)
    val maxIndexes = Array.fill[Boolean](maxStr.length)(false)
    val matches = minStr.zipWithIndex.map {
      case (c1, mi) =>
        @tailrec
        def findMatches(xi: Int, xn: Int, acc: Int = 0): Int =
          if (xi < xn) {
            if (!maxIndexes(xi) && c1 == maxStr(xi)) {
              minIndexes(mi) = true
              maxIndexes(xi) = true
              acc + 1
            } else findMatches(xi + 1, xn, acc)
          } else acc

        findMatches(max(mi - matchingDistance, 0), min(mi + matchingDistance + 1, maxStr.length))
    }.sum
    (minIndexes, maxIndexes, matches)
  }

  private def generateMatchArrays(minIndexes: Array[Boolean], maxIndexes: Array[Boolean])(minStr: String, maxStr: String): (Seq[Char], Seq[Char]) = {
    var minStrSeq: Seq[Char] = Seq.empty
    var maxStrSeq: Seq[Char] = Seq.empty
    maxStr.zipWithIndex.foreach {
      case (c, i) =>
        if (maxIndexes(i))
          maxStrSeq = maxStrSeq :+ c
        if (i < minIndexes.length && minIndexes(i))
          minStrSeq = minStrSeq :+ minStr(i)
    }
    (minStrSeq, maxStrSeq)
  }

  private def doCompare(s1: String, s2: String): Try[Double] = Try {
    require(null != s2 && null != s2, "input strings can't be null")
    require(s1.nonEmpty && s2.nonEmpty, "input strings can't be empty")

    val (minStr: String, maxStr: String) = {
      val seq = Seq(s1, s2)
        .map(s => getRegex("[^a-zA-Z0-9\\s]+").replaceAllIn(s, ""))
        .sorted(Ordering.String.reverse)
      (seq(0), seq(1))
    }

    if (minStr == maxStr) return Success(1.0)

    require(minStr.length > 0, "input doesn't conform to regex [a-zA-Z0-9\\s]+")

    val (minIndexes, maxIndexes, matches) = generateIndexArrays(minStr, maxStr)

    val (matchedMinStr, matchedMaxStr) = generateMatchArrays(minIndexes, maxIndexes)(minStr, maxStr)

    val transpositions = matchedMinStr.zip(matchedMaxStr).map {
      case (c1, c2) if c1 != c2 => 1
      case _ => 0
    }.sum / 2

    val prefix = {
      @tailrec
      def lcs(i1: Int = 0, i2: Int = 0, acc: Int = 0): Int =
        if (i1 >= s1.length || i2 >= s2.length || s1(i1) != s2(i2) || i1 >= 4) acc
        else lcs(i1 + 1, i2 + 1, acc + 1)

      lcs()
    }

    val distance =
    //                           1 (   m      m    (m - t) )
    // The Jaro distance is dj = - ( ---- + ---- + ------- )
    //                           3 ( |s1|   |s2|      m    )
      if (matches > 0)
        ((matches.toDouble / minStr.length)
          + (matches.toDouble / maxStr.length)
          + ((matches.toDouble - transpositions) / matches.toDouble)
          ) / 3
      else
        0

    //  logger.debug(s"[$s1, $s2] matches: $matches transpositions: $transpositions distance: $distance prefix: $prefix")
    // final score is s = dj + (prefix * p)(1 - dj) where p == 0.1
    distance + ((prefix * 0.1) * (1 - distance))
  }

  /**
   * Jaro Winkler Similarity implementation inspired by
   * {@see org.apache.commons.text.similarity.JaroWinklerSimilarity}
   *
   * @param s1 the first string
   * @param s2 the second string
   * @throws IllegalArgumentException if any of the arguments are null OR don't conform to `[a-zA-Z0-9\s]+`
   * @return the Jaro Winkler similarity score, in range [0,1]
   */
  def apply(s1: String, s2: String): Double = doCompare(s1, s2) match {
    //case Failure(exception) => logger.warn(s"couldn't match '$s1' and '$s2': ${exception.getMessage}"); 0d
    case Failure(exception) => println(s"couldn't match '$s1' and '$s2': ${exception.getMessage}"); 0d
    case Success(value) => value
  }
}
