import org.scalatest.FlatSpec

import scala.io.Source

class SpaceImageFormatTest extends FlatSpec {

  "The transmission 123456789012 with 3 pixel wide and 2 pixel tall " should "be 1" in {
    assert(SpaceImageFormat.checkImage("123456789012", 3, 2) === 1)
  }

  "The transmission from file with 25 pixels wide and 6 pixels tall " should "be 1" in {
    assert(SpaceImageFormat.checkImage(Source.fromResource("SpaceImageFormat.txt").getLines().next(), 25, 6) === 1690)
  }

  "The image decoded for 0222112222120000 with 2 wide and 2 tall size" should "be 0110" in {
    assert(SpaceImageFormat.decode("0222112222120000", 2, 2) === "0110")
  }

  "The decoded image from file with 25 pixels wide and 6 pixels tall " should "be 111101110011110100101110000010100100001010010100100010010010001001001011100010001110001000100101001010000100001000010010100101111010000111100110011100" in {
    assert(SpaceImageFormat.decode(Source.fromResource("SpaceImageFormat.txt").getLines().next(), 25, 6) === "111101110011110100101110000010100100001010010100100010010010001001001011100010001110001000100101001010000100001000010010100101111010000111100110011100")
  }
}
