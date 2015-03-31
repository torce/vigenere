package es.udc.csai

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import es.udc.csai.Language.{Custom, English}
import org.rogach.scallop.LazyScallopConf
import org.rogach.scallop.exceptions.{Help, ScallopException}

object Main extends App {

  object Conf extends LazyScallopConf(args.toList) {
    val cipher = opt[Boolean](
      name = "cipher",
      descr = "Cipher the current input")

    val key = opt[String](
      name = "key",
      descr = "Key used for cipher")

    val decipher = opt[Boolean](
      name = "decipher",
      descr = "Decipher the current input")

    val output = opt[String](
      name = "output",
      descr = "File to save the output")

    val string = opt[String](
      name = "string",
      descr = "Set the provide string as input")

    val file = opt[String](
      name = "file",
      descr = "File input for cipher/decipher")

    val break = opt[Boolean](
      name = "break",
      descr = "Try to decipher the input without the key")

    val maxKeyLength = opt[Int](
      name = "max-key-length",
      descr = "Maximum length for the key",
      short = 'l',
      validate = _ > 0)

    val numberOfMatches = opt[Int](
      name = "number-of-matches",
      descr = "Minimum number of dictionary matches required by a text to be " +
        "considered a deciphered text",
      short = 'm',
      validate = _ > 0)

    val guessKeyLength = opt[Boolean](
      name = "guess-key-length",
      descr = "With this option, the program will try to guess the key length " +
        "in order to use frequency analysis over the partitions")

    val snippetLength = opt[Int](
      name = "snippet-length",
      descr = "Length of the snippets evaluated. By default 64 characters.",
      short = 'n',
      validate = _ > 0,
      default = Some(64))

    val numCharsTested = opt[Int](
      name = "num-chars-tested",
      descr = "Number of characters of the language used to guess the delta of " +
        "each snippet. By default all the characters of the language are tested.",
      short = 'a',
      validate = _ > 0)

    val charset = opt[String](
      name = "charset",
      short = 'x',
      descr = "Charset used. A for uppercase characters, a for lowercase characters, " +
        "0 for numbers and $ for symbols. All options can be used together. Default: Uppercase and spaces",
      default = None)

    requireOne(cipher, decipher, break)
    requireOne(file, string)
    conflicts(cipher, List(maxKeyLength, numberOfMatches, guessKeyLength, snippetLength, numCharsTested))
    conflicts(decipher, List(maxKeyLength, numberOfMatches, guessKeyLength, snippetLength, numCharsTested))
    conflicts(break, List(key))
    dependsOnAll(break, List(maxKeyLength, numberOfMatches))
    mutuallyExclusive(cipher, decipher, break)
  }

  Conf.initialize {
    case Help(_) =>
      Conf.printHelp()
      System.exit(0)
    case e: ScallopException =>
      println(e.message)
      System.exit(0)
  }

  val input = if (Conf.string.isDefined) {
    Conf.string()
  } else {
    io.Source.fromFile(Conf.file()).mkString
  }

  val output: (String) => Unit = if (Conf.output.isDefined) {
    (s) => Files.write(Paths.get(Conf.output()), s.getBytes(StandardCharsets.UTF_8))
  } else {
    (s) => println(s)
  }

  val keyOutput: (Long) => (String, String) => Unit =
    (init) => (key, result) =>
      output(s"Timestamp: ${System.currentTimeMillis() - init}s\nKey: $key\nResult:\n${result.substring(0, math.min(256, result.length))}")

  val lang = if(Conf.charset.isSupplied) {
    val charsetOptions = Map(
      'A' -> "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
      'a' -> "abcdefghijklmnopqrstuvwxyz",
      '0' -> "0123456789",
      '$' -> "\\ºª!|\"@·#$~%½&¬/{([)]=}?¿'¡ ")

    val charset = charsetOptions.foldLeft("") {
      case (acc, (c, a)) =>
        if(Conf.charset().contains(c)) {
          acc + a
        } else {
          acc
        }
    }
    new Custom(charset, English.frequencyTable.filter(t => charset.contains(t._1)), English.commonWords)
  } else {
    English
  }

  if (Conf.cipher.isSupplied) {
    val text = Vigenere.cipher(input, Conf.key())(lang)
    println(s"Coincidence index: ${TextUtils.coincidenceIndex(text)}")
    output(text)
  }

  if (Conf.decipher.isSupplied) {
    output(Vigenere.decipher(input, Conf.key())(lang))
  }


  if (Conf.break.isSupplied) {

    if (Conf.guessKeyLength.isSupplied) {
      val lengths = VigenereBreaker.guessLength(input, Conf.maxKeyLength())

      println("Possible key lengths:")
      lengths.foreach {
        t =>
          println(s"${t._1} with score: ${t._2}")
      }

      val init = System.currentTimeMillis()
      VigenereBreaker.decipherPartitions(
        text = input,
        lengths = lengths.map(_._1),
        matches = Conf.numberOfMatches(),
        snippetLength = Conf.snippetLength(),
        numCharsTested = Conf.numCharsTested.get.getOrElse(lang.charset.length - 1),
        output = keyOutput(init))(lang)
      println(s"${System.currentTimeMillis() - init} ms")
    } else {
      val init = System.currentTimeMillis()
      VigenereBreaker.decipherBruteForce(input, Conf.numberOfMatches(), Conf.maxKeyLength(), keyOutput(init))(lang)
      println(s"${System.currentTimeMillis() - init} ms")
    }

  }
}
