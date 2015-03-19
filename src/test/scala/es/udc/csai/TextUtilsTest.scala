package es.udc.csai

import org.scalatest.{Matchers, WordSpecLike}

class TextUtilsTest extends Matchers with WordSpecLike {
  "A TestAnalysis object" should {
    "Calculate the frequency table of a text" in {
      TextUtils.charFrequencies("HELLO") shouldBe Map('H' -> 0.2, 'E' -> 0.2, 'L' -> 0.4, 'O' -> 0.2)
    }

    "Calculate the number of matches of common words in a text" in {
      import Language.English
      TextUtils.findWordMatches("“I LOVE THE SMELL OF NAPALM IN THE MORNING") shouldBe 4
    }
  }
}
