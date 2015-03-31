package es.udc.csai

import scala.collection.Map

object TextUtils {

  private def countTable(text: String): Map[Char, Int] = {
    text.foldLeft(Map.empty[Char, Int]) {
      (count, char) => count + (char -> (count.getOrElse(char, 0) + 1))
    }
  }

  def charFrequencies(text: String): Map[Char, Double] = {
    countTable(text).map (t => (t._1, t._2 / text.length.toDouble))
  }

  def sortedCharFrequencies(text: String): Seq[(Char, Double)] = {
    charFrequencies(text).map(identity)(collection.breakOut).sortWith(_._2 > _._2)
  }

  def wordCountTable(text: String, separator: Char = ' '): Map[String, Int] = {
    text.split(separator).foldLeft(Map.empty[String, Int]) {
      case (map, word) => map + (word.toUpperCase -> (map.getOrElse(word, 0) + 1))
    }
  }

  def findWordMatches(text: String, separator: Char = ' ')(implicit lang: Language): Int = {
    val wordMap = wordCountTable(text, separator)
    lang.commonWords.foldLeft(0) {
      case (acc, word) => acc + wordMap.getOrElse(word, 0)
    }
  }

  def coincidenceIndex(text: String): Double = {
    countTable(text).foldLeft(0) {
      case (sum, (_, value)) => sum + value * (value - 1)
    } / (text.length * (text.length - 1)).toDouble
  }

  def encode(text: String)(implicit lang: Language): Seq[Int] = {
    text.map(lang.value)
  }

  def decode(vector: Seq[Int])(implicit lang: Language): String = {
    val a = vector.foldLeft(new StringBuffer()) {
      case (sb, i) => sb.append(lang.character(i))
    }.toString
    a
  }
}
