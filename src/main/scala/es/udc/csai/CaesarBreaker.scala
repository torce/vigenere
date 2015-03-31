package es.udc.csai

import scala.collection._

object CaesarBreaker {
  def decipherBestMatch(text: String)(implicit lang: Language): String = {
    val textFreqTable = sortFrequencies(TextUtils.charFrequencies(text))
    val langFreqTable = sortFrequencies(lang.frequencyTable)
    val replacementTable = (textFreqTable zip langFreqTable).foldLeft(Map.empty[Char, Char]) {
      case (replacements, ((textChar, _), (langChar, _))) => replacements + (textChar -> langChar)
    }
    text.map(replacementTable)
  }

  def decipherSnippet(text: String, snippetLength: Int, index: Int)(implicit lang: Language): (Int, String) = {
    val mostRepeatedChar = sortFrequencies(TextUtils.charFrequencies(text)).foldLeft(-1) {
      case (v, (c, _)) => if(lang.value(c) != -1) lang.value(c) else v
    }
    if(mostRepeatedChar < 0) {
      throw new RuntimeException("None of the characters of the snippet exists in the language.")
    }
    val mostRepeatedCharLanguage = sortFrequencies(lang.frequencyTable)
    val mrcl = lang.value(mostRepeatedCharLanguage(index)._1)
    val delta = (((mostRepeatedChar - mrcl) % lang.charset.length) + lang.charset.length) % lang.charset.length
    val snippet = Vigenere.decipher(text.substring(0, math.min(snippetLength, text.length)), String.valueOf(lang.character(delta)))
    (delta, snippet)
  }

  def decipherBruteForce(text: String)(implicit lang: Language): Seq[(Int, String)] = {
    val parText = text.toSeq.par
    for {
      key <- 0 to (lang.charset.length - 1)
      decipheredText = parText.map(t => lang.character(lang.value(t) - key)).mkString
    } yield {
      (key, decipheredText)
    }
  }

  def decipherBruteForceWithScore(text: String)(implicit lang: Language): Seq[(Int, Int, String)] = {
    decipherBruteForce(text).map(t => (t._1, TextUtils.findWordMatches(t._2), t._2)).sortWith(_._2 > _._2)
  }

  private def sortFrequencies(table: Map[Char, Double]): Seq[(Char, Double)] = {
    table.map(identity)(collection.breakOut).sortWith(_._2 > _._2)
  }
}
