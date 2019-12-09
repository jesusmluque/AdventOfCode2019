object SpaceImageFormat {

  def checkImage(image: String, wide: Int, tall: Int): Int = {
    val size = wide * tall
    val layers = image.split("").grouped(size).toList
    val index = layers.zipWithIndex.minBy(p => p._1.count(_ == "0"))._2
    val layer = layers(index)

    val  onesTwos = layer.foldLeft((0,0)) { (acc, n) =>
      if (n == "1")
        (acc._1  + 1, acc._2)
      else if (n == "2")
        (acc._1, acc._2 + 1)
      else
        acc

    }

    onesTwos._1 * onesTwos._2
  }

  def decode(image: String, wide: Int, tall: Int): String = {
    val size = wide * tall
    val layers = image.split("").grouped(size).toList

    val res = (0 until size).map { i =>
      layers.map(l => l(i)).foldLeft("2") { (acc, n) =>
        if (acc == "2")
          n
        else
          acc
      }
    }
    res.mkString("")
  }
}