package es.udc.csai

import org.scalatest.{WordSpecLike, Matchers, FlatSpec}

class VigenereTest extends Matchers with WordSpecLike{
  "A Vigenere objet" should {
    import es.udc.csai.Language.English
    "cipher a text with the key provided" in {
      Vigenere.cipher("ATTACKATDAWN", "LEMON") shouldBe "LXFOPVEFRNHR"
    }
    "decipher a text with the key provided" in {
      Vigenere.decipher("LXFOPVEFRNHR", "LEMON") shouldBe "ATTACKATDAWN"
    }
  }
}
