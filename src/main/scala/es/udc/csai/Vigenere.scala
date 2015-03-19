package es.udc.csai

import scala.collection.immutable.HashMap

trait Language {
  def charset: String
  def value(char: Char): Int
  def character(value: Int): Char
  def frequencyTable: Map[Char, Double]

  def commonWords: Seq[String]
}

object Language {
  implicit object English extends Language {
    val charset: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ "
    val frequencyTable: Map[Char, Double] = HashMap(
      'A' -> 0.08167, 'B' -> 0.01492, 'C' -> 0.02782, 'D' -> 0.04253, 'E' -> 0.12702, 'F' -> 0.02228,
      'G' -> 0.02015, 'H' -> 0.06094, 'I' -> 0.06966, 'J' -> 0.00153, 'K' -> 0.00772, 'L' -> 0.04025,
      'M' -> 0.02406, 'N' -> 0.06749, 'O' -> 0.07507, 'P' -> 0.01929, 'Q' -> 0.00095, 'R' -> 0.05987,
      'S' -> 0.06327, 'T' -> 0.09056, 'U' -> 0.02758, 'V' -> 0.00978, 'W' -> 0.02360, 'X' -> 0.00150,
      'Y' -> 0.01974, 'Z' -> 0.00074, ' ' -> 0.09)
    val commonWords: Seq[String] = Seq("THE", "BE", "TO", "OF", "AND", "A", "IN", "THAT", "HAVE", "I")
    def value(char: Char) = {
      charset.indexOf(char)
    }

    def character(value: Int): Char = {
      charset(((value % charset.length) + charset.length) % charset.length)
    }
  }
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
