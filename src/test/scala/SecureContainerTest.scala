import org.scalatest.FlatSpec

class SecureContainerTest extends FlatSpec {

  "The number of password for the input 1 to 20 " should "be 1" in {
    assert(SecureContainer.count(1, 20) === 1)
  }

  "The number of password for the input 1 to 100 " should "be 9" in {
    assert(SecureContainer.count(1, 100) === 9)
  }

  "The number of password for the input 357253 to 892942 " should "be 530" in {
    assert(SecureContainer.count(357253, 892942) === 530)
  }

  "The number of password for the input 357253 to 892942 with the  new algioritme " should "be 324" in {
    assert(SecureContainer.countStrict(357253, 892942) === 324)
  }
}
