package es.udc.csai

import org.scalatest.{Matchers, WordSpecLike}

class VigenereTest extends Matchers with WordSpecLike{
  "A Vigenere object" should {
    import es.udc.csai.Language.English
    "cipher a text with the key provided" in {
      Vigenere.cipher("THIS IS SPARTA", "AB") shouldBe "TIIT JSASQASTB"
    }
    "decipher a text with the key provided" in {
      Vigenere.decipher("TIIT JSASQASTB", "AB") shouldBe "THIS IS SPARTA"
    }
    "cipher and decipher should be inverse" in {
      Vigenere.decipher(Vigenere.cipher("THIS IS A TEST", "PASSWORD"), "PASSWORD") shouldBe "THIS IS A TEST"
      Vigenere.cipher(Vigenere.decipher("THIS IS A TEST", "PASSWORD"), "PASSWORD") shouldBe "THIS IS A TEST"
    }
    "do not cipher characters that not belong to the language" in {
      Vigenere.cipher(";;", "AB") shouldBe ";;"
    }
  }
}
