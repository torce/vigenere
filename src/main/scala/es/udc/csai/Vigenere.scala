package es.udc.csai

import scala.collection.immutable.HashMap

trait Language {
  def charset: String
  def frequencyTable: Map[Char, Double]
  def commonWords: Seq[String]
  def value(char: Char) = {
    charset.indexOf(char)
  }
  def hasWord(word: String) = {
    commonWords.contains(word.toUpperCase)
  }
  def character(value: Int): Char = {
    charset(((value % charset.length) + charset.length) % charset.length)
  }
}

object Language {
  implicit object English extends Language {
    val charset: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ "
    val frequencyTable: Map[Char, Double] = HashMap(
      'A' -> 0.08167, 'B' -> 0.01492, 'C' -> 0.02782, 'D' -> 0.04253, 'E' -> 0.12702, 'F' -> 0.02228,
      'a' -> 0.08167, 'b' -> 0.01492, 'c' -> 0.02782, 'd' -> 0.04253, 'e' -> 0.12702, 'f' -> 0.02228,
      'G' -> 0.02015, 'H' -> 0.06094, 'I' -> 0.06966, 'J' -> 0.00153, 'K' -> 0.00772, 'L' -> 0.04025,
      'g' -> 0.02015, 'h' -> 0.06094, 'i' -> 0.06966, 'j' -> 0.00153, 'k' -> 0.00772, 'l' -> 0.04025,
      'M' -> 0.02406, 'N' -> 0.06749, 'O' -> 0.07507, 'P' -> 0.01929, 'Q' -> 0.00095, 'R' -> 0.05987,
      'm' -> 0.02406, 'n' -> 0.06749, 'o' -> 0.07507, 'p' -> 0.01929, 'q' -> 0.00095, 'r' -> 0.05987,
      'S' -> 0.06327, 'T' -> 0.09056, 'U' -> 0.02758, 'V' -> 0.00978, 'W' -> 0.02360, 'X' -> 0.00150,
      's' -> 0.06327, 't' -> 0.09056, 'u' -> 0.02758, 'v' -> 0.00978, 'w' -> 0.02360, 'x' -> 0.00150,
      'Y' -> 0.01974, 'Z' -> 0.00074, ' ' -> 0.09, 'y' -> 0.01974, 'z' -> 0.00074)
    /*val frequencyTable: Map[Char, Double] = HashMap(
      'a' -> 7.52766, 'e' -> 7.0925, 'o' -> 5.17, 'r' -> 4.96032,
      'i' -> 4.69732, 's' -> 4.61079, 'n' -> 4.56899, '1' -> 4.35053,
      't' -> 3.87388, 'l' -> 3.77728, '2' -> 3.12312, 'm' -> 2.99913,
      'd' -> 2.76401, '0' -> 2.74381, 'c' -> 2.57276, 'p' -> 2.45578,
      '3' -> 2.43339, 'h' -> 2.41319)*/
    val commonWords: Seq[String] = Seq("THE", "BE", "TO", "OF", "AND", "IN", "THAT", "HAVE", "IT", "FOR",
                                       "NOT", "ON", "WITH", "HE", "AS", "YOU", "DO", "AT", "THIS", "BUT")

  }

  case class Custom(charset: String, frequencyTable: Map[Char, Double], commonWords: Seq[String]) extends Language
}

object Vigenere {
  def cipher(text: String, key: String)(implicit lang: Language): String = {
    transform(text, key, _ + _)
  }

  def decipher(text: String, key: String)(implicit lang: Language): String = {
    transform(text, key, _ - _)
  }

  private def transform(text: String, key: String, f: (Int, Int) => Int)(implicit lang: Language): String = {
    (for {
      indexed <- text.view.zipWithIndex
      cipheredChar = if (lang.value(indexed._1) >= 0) {
        val charValue = (lang.value(indexed._1), indexed._2)
        val keyValue = lang.value(key(charValue._2 % key.length))
        lang.character(f(charValue._1, keyValue))
      } else {
        indexed._1
      }
    } yield {
      cipheredChar
    }).mkString
  }
}
