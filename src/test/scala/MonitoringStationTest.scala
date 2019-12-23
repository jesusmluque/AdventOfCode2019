import org.scalatest.FlatSpec

import scala.io.Source

class MonitoringStationTest extends FlatSpec {

  "The best location for a monitoring station for the map .#..# " should  "be (3,4)" in {
    assert(MonitoringStation(".#..#\n.....\n#####\n....#\n...##").findBest() ===  (8,(4,3)))
  }

  "The best location for a monitoring station for the map ......#.#.n.##.#..###....#....#### " should "be (5,8)" in {
    assert(MonitoringStation("......#.#.\n#..#.#....\n..#######.\n.#.#.###..\n.#..#.....\n..#....#.#\n#..#....#.\n.##.#..###\n##...#..#.\n.#....####").findBest() === (33,(8,5)))
  }

  "The best location for a monitoring station for the map ......#.#.n.##.#..###....#....#### " should "be (1,2)" in {
    assert(MonitoringStation("#.#...#.#.\n.###....#.\n.#....#...\n##.#.#.#.#\n....#.#.#.\n.##..###.#\n..#...##..\n..##....##\n......#...\n.####.###.").findBest() === (35,(2,1)))
  }

  "The best location for a monitoring station for the map ......#.#.n.##.#..###....#....#### " should "be (6,3)" in {
    assert(MonitoringStation(".#..#..###\n####.###.#\n....###.#.\n..###.##.#\n##.##.#.#.\n....###..#\n..#.#..#.#\n#..#.#.###\n.##...##.#\n.....#.#..").findBest() === (41,(3,6)))
  }

  "The best locaion for a monitoring station for the map from file" should "be (12,11)" in {
    assert(MonitoringStation(Source.fromResource("MonitoringStation.txt").getLines().foldLeft("")((acc, n) => acc + "\n" + n)).findBest() == (221,(12,11)))
  }

  "The 36th element to disappear in the map .#....#####...#...#.....#....## " should "be (14, 3)" in {
    assert(MonitoringStation(".#....#####...#..\n##...##.#####..##\n##...#...#.#####.\n..#.....#...###..\n..#.#.....#....##").findAsteroid(36) === Some(3,14))
  }

  "The 200th element to disappear in the map from file" should "be (12,11)" in {
    assert(MonitoringStation(Source.fromResource("MonitoringStation.txt").getLines().foldLeft("")((acc, n) => acc + "\n" + n).tail).findAsteroid(200) === Some(6,8))
  }

  "The 200th element to disappear in the map from large one" should "be (12,11)" in {
    assert(MonitoringStation(".#..##.###...#######\n##.############..##.\n.#.######.########.#\n.###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n####################\n#.####....###.#.#.##\n##.#################\n#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n.#####..#.######.###\n##...#.##########...\n#.##########.#######\n.####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n#.#.#.#####.####.###\n###.##.####.##.#..##").findAsteroid(200) === Some(2,8))
  }

}
