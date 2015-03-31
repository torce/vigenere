package es.udc.csai

object VigenereBreaker {

  case class TextPartition(text: String, snippetLength: Int)(implicit lang: Language) {
    //Get the most repeated character in the text that belongs to the language
    val mostRepeatedChar: Int = {
      lang.value(TextUtils.sortedCharFrequencies(text).find(t => lang.value(t._1) > 0).get._1)
    }
    val snippet: String = text.substring(0, math.min(snippetLength, text.length))

    def decipherSnippet(index: Int): (Int, String) = {
      def mod(a: Int, b: Int) = (a % b + b) % b // Real modulo operator
      val delta = mod(mostRepeatedChar - lang.value(lang.commonCharacters(index)._1), lang.charset.length)
      val decipheredSnippet = Vigenere.decipher(snippet, String.valueOf(lang.character(delta)))
      (delta, decipheredSnippet)
    }
  }

  def guessLength(text: String, maxLength: Int): Seq[(Int, Int)] = {

    def getMatches(text: String, gap: Int): Int = {
      text.view.zipWithIndex.foldLeft(0) {
        case (shiftCount, (char, index)) =>
          if ((index + gap < text.length) && (char == text(index + gap))) {
            shiftCount + 1
          } else {
            shiftCount
          }
      }
    }

    (for {
      gap <- 1 to Math.min(text.length - 1, maxLength)
      matches = getMatches(text, gap)
      result = matches if matches != 0
    } yield {
        (gap, result)
      }).sortWith(_._2 > _._2)
  }

  def decipherBruteForce(text: String, matches: Int, maxKeyLength: Int, output: (String, String) => Unit)(implicit lang: Language) {
    def generate(i: Int): StringBuilder = {
      val char = lang.charset(i % lang.charset.length)
      val n = i / lang.charset.length
      if (n == 0) {
        new StringBuilder().append(char)
      } else {
        generate(n - 1).append(char)
      }
    }

    var i = 0
    var key = generate(i).toString()
    while (key.length <= maxKeyLength) {
      val tt = Vigenere.decipher(text, key)(lang)
      if (TextUtils.findWordMatches(tt) >= matches) {
        output(key, tt)
      }
      i += 1
      key = generate(i).toString()
    }
  }

  class Combinations(length: Int, maxCipher: Int) extends Iterator[Seq[Int]] {
    var i: Int = 0
    var mi: Int = 0
    var v = Vector.fill(length)(0)
    var first: Boolean = true

    override def hasNext: Boolean = !v.forall(_ == maxCipher)

    override def next(): Seq[Int] = {
      if (!first) {
        if (v.forall(_ == v.head)) {
          v = Vector.fill[Int](length)(0).updated(0, mi + 1)
          i = 0
          mi += 1
        } else if (v(i + 1) < v(i)) {
          v = v.updated(i, v(i + 1)).updated(i + 1, v(i))
          if (i + 1 == length - 1) {
            i = 0
          } else {
            i += 1
          }
        } else {
          v = v.updated(0, v.head + 1)
          i = 0
        }
      }
      first = false
      v
    }
  }

  def decipherPartitions(text: String, matches: Int, lengths: Seq[Int], snippetLength: Int, numCharsTested: Int, output: (String, String) => Unit)(implicit lang: Language) {
    lengths.foreach { l =>
      val partitions = split(text, l).map(new TextPartition(_, snippetLength)(lang))
      new Combinations(l, numCharsTested).foreach { c: Seq[Int] =>
        val combinationResults = (partitions zip c).map {
          case (partition, index) => partition.decipherSnippet(index)
        }
        val (key, decipheredPartitions) = combinationResults.unzip
        val decipheredText = join(decipheredPartitions, text.length)
        if (TextUtils.findWordMatches(decipheredText) >= matches) {
          output(TextUtils.decode(key), Vigenere.decipher(text, TextUtils.decode(key))(lang))
        }
      }
    }
  }

  private def split(text: String, partitions: Int): Seq[String] = {
    text.view.zipWithIndex.foldLeft(Vector.fill(partitions)(new StringBuilder())) {
      case (v, (c, i)) =>
        v.updated(i % partitions, v(i % partitions).append(c))
    }.map(_.toString())
  }

  private def join(partitions: Seq[String], textLength: Int): String = {
    val decipherText = new StringBuffer()
    val max = partitions.foldLeft(0)(_ + _.length)
    for (index <- 0 until max) {
      decipherText.append(partitions(index % partitions.size)(index / partitions.size))
    }
    decipherText.toString
  }
}
