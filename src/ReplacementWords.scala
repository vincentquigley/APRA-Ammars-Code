
object ReplacementWords {
  // TODO: a title that ends in DEMO is not removed
  // TODO: if any of these are enclosed in brackets =>  ()
  final val DeNoiseList = List("SOUNDTRACK VERSION", "SINGLE VERSION", "STEREO VERSION", "ALBUM VERSION",
    "ORIGINAL MIX", "BONUS TRACK", "EXPLICIT", "DUB MIX", "NEW MIX", "REMIX", "REMASTER", "DEMO ",
    "DEMO VERSION", "LIVE VERSION", "CLEAN VERSION", "KARAOKE VERSION", """\.""", "!", "\"", "\'")
  // TODO: curly brackets are not replaced?  other characters? " + ; _ # % @ "
  final val WordSwapMap: Map[String, String] = Map(" YOU ARE " -> " YOURE ", " WE ARE " -> " WERE ", " THEY ARE " -> " THEYRE ", " I AM " -> " IM ",
    " I WILL " -> " ILL ", " YOU WILL " -> " YOULL ", " WE WILL " -> " WELL ", " HE WILL " -> " HELL ",
    " SHE WILL " -> " SHELL ", " THEY WILL " -> " THEYLL ", " HE IS " -> " HES ", " SHE IS " -> " SHES ",
    " DO NOT " -> " DONT ", " IS NOT " -> " ISNT ", " WILL NOT " -> " WONT ", " ARE NOT " -> " ARENT ",
    "ING" -> "IN", """OP\.""" -> "OP ", "-" -> " ", ":" -> " ", "/" -> " ", "," -> " ", """\[""" -> "(", """\]""" -> ")")
  // TODO: L' has been removed from this list because it doesn't exist as a word - has a separate check
  final val ArticlesList = List("A", "AN", "DAS", "DEM", "DEN", "DER", "DES", "EIN", "EINE", "EINEM", "EINEN", "EINES", "EL", "GLI",
    "IL", "LA", "LAS", "LE", "LES", "LO", "LOS", "THE", "UN", "UNA", "UNE", "UNO")
  // TODO: what about "-IZE" to "-ISE" ?
  final val ContractionSwapMap: Map[String, String] = Map("AND" -> "&", "COLOR" -> "COLOUR", "N" -> "&", "N'" -> "&", "OH" -> "O",
    "LITE" -> "LIGHT", "NITE" -> "NIGHT", "PT" -> "PART", "PTS" -> "PART", "THRU" -> "THROUGH", "TONITE" -> "TONIGHT")
  final val NumberSwapMap: Map[String, String] = Map("ONE" -> "1", "TWO" -> "2", "THREE" -> "3", "FOUR" -> "4", "FIVE" -> "5", "SIX" -> "6", "SEVEN" -> "7",
    "EIGHT" -> "8", "NINE" -> "9", "TEN" -> "10", "ELEVEN" -> "11", "TWELVE" -> "12")
  final val RomanNumeralSwapMap: Map[String, String] = Map("II" -> "2", "III" -> "3", "IV" -> "4", "V" -> "5", "VI" -> "6", "VII" -> "7",
    "VIII" -> "8", "IX" -> "9", "X" -> "10", "XI" -> "11", "XII" -> "12", "XIII" -> "13", "XIV" -> "14")
  final val ClassicalWordsList = List("ADAGIO", "ARIA", "BOURREE", "BVW", "CADENZA", "CANON", "CANTABILE", "CANTATA",
    "CAPRICCIO", "CAPRICE", "CHORALE", "CONCERTINO", "CONCERTO", "CONTRAPUNCTUS", "DANZA", "DIVERTIMENTO",
    "ETUDE", "FUGUE", "IMPROMPTU", "LARGO", "MASS", "MINUET", "MISSA", "NOCTURNE", "NONET", "OCTET", "OPUS",
    "ORATORIO", "ORCH", "ORCHESTRA", "PASTORAL", "PRELUDE", "QUARTET", "QUINTET", "REQUIEM", "RONDO",
    "SCHERZO", "SEPTET", "SERENADE", "SEXTET", "SINFONIA", "SONATA", "SUITE", "SYMPHONY", "TRIO", "VARIATION")
  final val DeLiveList = List(" LIVE   ", " LIVE 19", " LIVE 20", " LIVE IN ", " LIVE AT ", " LIVE EDIT ", " LIVE FROM ", " LIVE VERSION ")
}

