package es.udc.csai

import org.scalatest.{Matchers, WordSpecLike}

class CaesarBreakerTest extends Matchers with WordSpecLike {
  "A CaesarBreaker object" should {
    import Language.English
    "Replace the characters in the text with the most likely coincidence" in {
      CaesarBreaker.decipherBestMatch("AAAAA") shouldBe "EEEEE"
    }
    "Return the results of testing all the possible keys" in {
      val result = for {
        i <- 0 to (English.charset.length - 1)
        char = English.character(English.charset.length - i)
      } yield {
          (i, new StringBuilder().append(char).append(char).toString())
        }
      CaesarBreaker.decipherBruteForce("AA") shouldBe result
    }
    "Return a deciphered snippet" in {
      val key = 'B'
      val cipheredText = Vigenere.cipher("THIS IS A SECRET MESSAGE", String.valueOf(key))
      val l = for (i <- 0 to (English.charset.length - 1)) yield {
        CaesarBreaker.decipherSnippet(cipheredText, 10, i)
      }
      l should contain((English.value(key), "THIS IS A "))
    }
    "Return keys sorted by score, based on common words found" in {
      CaesarBreaker.decipherBruteForceWithScore("UIF")(Language.English)(0) shouldBe(1, 1, "THE")
    }
  }
}
