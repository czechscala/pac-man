package com.czechscala.pacman

import org.scalatest.{FunSuite, Matchers}

class BoardSuite extends FunSuite with Matchers {

  val grid1 =
    """######
      |#    #
      |######
    """.stripMargin

  def parse(gridStr: String): Array[Array[Cell]] = gridStr split "\n" map (_ map {
    case '#' => Wall
    case _ => Empty
  } toArray)

  test("moveAllCharacters") {
    val grid = parse(grid1)
    val board = Board(grid,
      objects = Map(),
      characters = Map(PacMan -> (Position(1, 1), Some(Right)))
    )

    board.moveAllCharacters should be (Board(
      grid,
      objects = Map(),
      characters = Map(PacMan -> (Position(2, 1), Some(Right)))
    ))
  }
}
