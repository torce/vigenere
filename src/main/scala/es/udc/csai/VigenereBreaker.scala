package es.udc.csai

object VigenereBreaker {

  case class KeyNotFoundException() extends Exception

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

  def decipherBruteForce(text: String, matches: Int, maxKeyLength: Int)(implicit lang: Language): String = {
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
      val tt = Vigenere.decipher(text, key)
      if (TextUtils.findWordMatches(tt) >= matches) {
        return tt
      }
      i += 1
      key = generate(i).toString()
    }
    throw new KeyNotFoundException
  }

  class Combinations(length: Int, maxCipher: Int) extends Iterator[Seq[Int]] {
    var i: Int = 0
    var mi: Int = 0
    var v = Vector.fill(length)(0)
    val max = math.pow(maxCipher + 1, length).toInt - 1
    var current = 0

    override def hasNext: Boolean = v != Vector.fill(length)(maxCipher)

    override def next(): Seq[Int] = {
      if (current != 0) {
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
      current += 1
      v
    }
  }

  def decipherPartitions(text: String, matches: Int, snippetLength: Int, numCharsTested: Int, maxLength: Int = 8)(implicit lang: Language): String = {
    var result = Seq[Int]()
    val gl = guessLength(text, maxLength)
    gl.forall { l =>
      val partitions = split(text, l._1)
      new Combinations(l._1, numCharsTested).forall { c: Seq[Int] =>
        val combinationResults = (partitions zip c).map {
          case t =>
            if (t._2 > numCharsTested) {
              println(c)
            }
            CaesarBreaker.decipherSnippet(t._1, snippetLength, t._2)

        }

        val (key, decipheredPartitions) = combinationResults.unzip
        val decipheredText = join(decipheredPartitions, text.length)
        if (TextUtils.findWordMatches(decipheredText) < matches) {
          true
        } else {
          result = key
          false
        }
      }
    }
    Vigenere.decipher(text, TextUtils.decode(result))
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