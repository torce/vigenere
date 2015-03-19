package es.udc.csai

import scala.collection.Map

object TextUtils {
  def charFrequencies(text: String): Map[Char, Double] = {
    text.foldLeft(Map.empty[Char, Int]) {
      (count, char) => count + (char -> (count.getOrElse(char, 0) + 1))
    } map (t => (t._1, t._2 / text.length.toDouble))
  }

  def findWordMatches(text: String, separator: Char = ' ')(implicit lang: Language): Int = {
    val wordMap = text.split(separator).foldLeft(Map.empty[String, Int]) {
      case (map, word) => map + (word -> (map.getOrElse(word, 0) + 1))
    }
    lang.commonWords.foldLeft(0) {
      case (acc, word) => acc + wordMap.getOrElse(word, 0)
    }
  }

  def encode(text: String)(implicit lang: Language): Seq[Int] = {
    text.map(lang.value)
  }

  def decode(vector: Seq[Int])(implicit lang: Language): String = {
    vector.foldLeft(new StringBuffer()) {
      case (sb, i) => sb.append(lang.character(i))
    }.toString
  }
}
