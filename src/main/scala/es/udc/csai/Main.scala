package es.udc.csai

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

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
        "each snippet. By default the 5 most used characters of the language are tested.",
      short = 'c',
      validate = _ > 0,
      default = Some(5))

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


  if (Conf.cipher.isSupplied) {
    output(Vigenere.cipher(input, Conf.key()))
  }

  if (Conf.decipher.isSupplied) {
    output(Vigenere.decipher(input, Conf.key()))
  }

  if (Conf.break.isSupplied) {
    val init = System.currentTimeMillis()
    if (Conf.guessKeyLength.isSupplied) {
      output(VigenereBreaker.decipherPartitions(
        text = input,
        matches = Conf.numberOfMatches(),
        snippetLength = Conf.snippetLength(),
        numCharsTested = Conf.numCharsTested(),
        Conf.maxKeyLength()))
    } else {
      output(VigenereBreaker.decipherBruteForce(input, Conf.numberOfMatches(), Conf.maxKeyLength()))
    }
    println(s"${System.currentTimeMillis() - init} ms")
  }
}
