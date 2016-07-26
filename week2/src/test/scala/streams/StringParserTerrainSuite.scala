package streams

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class StringParserTerrainSuite extends FunSuite {

  /**
    * A simple level constructed using the StringParserTerrain
    */
  abstract class Terrain extends StringParserTerrain

  object Terrain5x5 extends Terrain {
    val level =
      """------
        |--ST--
        |--oo--
        |--oo--
        |------""".stripMargin
  }

  for {
    x <- 1 to 5
  } yield test("terrainFunction on Terrain5x5 and invalid position (" + x + ",0)") {
    assert(!Terrain5x5.terrain(Terrain5x5.Pos(x, 0)))
  }

  test("terrainFunction on Terrain5x5 and outside of bounds") {
    assert(!Terrain5x5.terrain(Terrain5x5.Pos(10, 10)))
    assert(!Terrain5x5.terrain(Terrain5x5.Pos(5, 5)))
    assert(!Terrain5x5.terrain(Terrain5x5.Pos(0, 5)))
  }

  test("terrainFunction on Terrain5x5 and valid pos") {
    assert(Terrain5x5.terrain(Terrain5x5.Pos(1, 3)))
    assert(Terrain5x5.terrain(Terrain5x5.Pos(1, 2)))
    assert(Terrain5x5.terrain(Terrain5x5.Pos(2, 2)))
  }

  test("findChar on Terrain5x5 finds S and T") {
    assert(Terrain5x5.startPos === Terrain5x5.Pos(1, 2))
    assert(Terrain5x5.goal === Terrain5x5.Pos(1, 3))
  }
}
