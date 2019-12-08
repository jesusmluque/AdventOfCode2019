import org.scalatest.FlatSpec

import scala.io.Source

class UniversalOrbitTest extends FlatSpec {

  "The following map COM)B,B)C,C)D,D)E,E)F,B)G,G)H,D)I,E)J,J)K,K)L " should "has 42 orbits " in {
    assert(UniversalOrbit.count("COM)B,B)C,C)D,D)E,E)F,B)G,G)H,D)I,E)J,J)K,K)L".split(",").toList) ===  Some(42))
  }

  "The map from file " should "has 42 orbits " in {
    assert(UniversalOrbit.count(Source.fromResource("UniversalOrbit.txt").getLines().toList) == Some(140608))
  }

  "The number of transfer from you to san with COM)B,B)C,C)D,D)E,E)F,B)G,G)H,D)I,E)J,J)K,K)L,K)YOU,I)SAN " should "be 4" in {
    assert(UniversalOrbit.countOrbitTransfer("COM)B,B)C,C)D,D)E,E)F,B)G,G)H,D)I,E)J,J)K,K)L,K)YOU,I)SAN".split(",").toList) === Some(4))
  }

  "The number of transfer for the input in file " should "has 337 orbits " in {
    assert(UniversalOrbit.countOrbitTransfer(Source.fromResource("UniversalOrbit.txt").getLines().toList) == Some(337))
  }
}
