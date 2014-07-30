package com.czechscala.pacman

import org.scalatest.{FunSuite, Matchers}

class BoardSuite extends FunSuite with Matchers {

  def parse(gridStr: String): Array[Array[Cell]] = gridStr split "\n" map (_ map {
    case '#' => Wall
    case _ => Empty
  } toArray)

  test("initialization - all rows the same length") {
    an [IllegalArgumentException] should be thrownBy { Board(Array(Array(Wall, Wall), Array(Wall))) }
  }

  type PositionWithDirection = (Position, Option[Direction])
  def pacManMoveTest(startPosition: PositionWithDirection, endPosition: PositionWithDirection) = {
    val grid = parse("######\n" +
                     "#    #\n" +
                     "#    #\n" +
                     "#    #\n" +
                     "######\n")
    val board = Board(grid, characters = Map(PacMan -> startPosition))

    board.moveAllCharacters should be (Board(grid, characters = Map(PacMan -> endPosition)))
  }

  test("moveAllCharacters - PacMan moves right") {
    pacManMoveTest(Position(1, 1) -> Some(Right), Position(2, 1) -> Some(Right))
  }

  test("moveAllCharacters - PacMan moves left") {
    pacManMoveTest(Position(4, 1) -> Some(Left), Position(3, 1) -> Some(Left))
  }

  test("moveAllCharacters - PacMan moves up") {
    pacManMoveTest(Position(4, 3) -> Some(Up), Position(4, 2) -> Some(Up))
  }

  test("moveAllCharacters - PacMan moves down") {
    pacManMoveTest(Position(1, 1) -> Some(Down), Position(1, 2) -> Some(Down))
  }

  test("moveAllCharacters - PacMan moves left to wall and stops") {
    pacManMoveTest(Position(2, 1) -> Some(Left), Position(1, 1) -> None)
  }

  test("moveAllCharacters - PacMan moves right to wall and stops") {
    pacManMoveTest(Position(3, 1) -> Some(Right), Position(4, 1) -> None)
  }

  test("moveAllCharacters - PacMan moves up to wall and stops") {
    pacManMoveTest(Position(1, 2) -> Some(Up), Position(1, 1) -> None)
  }

  test("moveAllCharacters - PacMan moves down to wall and stops") {
    pacManMoveTest(Position(1, 2) -> Some(Down), Position(1, 3) -> None)
  }

  test("moveAllCharacters - multiple ghosts") {
    val grid = parse("########\n" +
                     "#      #\n" +
                     "#      #\n" +
                     "########")
    val (ghost1, ghost2, ghost3) = (new Ghost, new Ghost, new Ghost)
    val board = Board(grid, characters = Map(
      ghost1 -> (Position(1, 1), Some(Right)),
      ghost2 -> (Position(2, 1), Some(Right)),
      ghost3 -> (Position(1, 3), Some(Up))
    ))

    board.moveAllCharacters should be (Board(grid, characters = Map(
      ghost1 -> (Position(2, 1), Some(Right)),
      ghost2 -> (Position(3, 1), Some(Right)),
      ghost3 -> (Position(1, 2), Some(Up))
    )))
  }

  test("setPacManDirection") {
    val ghost = new Ghost
    val gem = new Gem
    val grid = parse("  \n" +
                     "  ")
    val board = Board(grid,
      objects = Map(Position(1, 1) -> gem),
      characters = Map(
        PacMan -> (Position(0, 0), Some(Right)),
        ghost -> (Position(0, 1), Some(Up))
      )
    )

    board.setPacManDirection(Down) should be (Board(grid,
      objects = Map(Position(1, 1) -> gem),
      characters = Map(
        PacMan -> (Position(0, 0), Some(Down)),
        ghost -> (Position(0, 1), Some(Up))
      )
    ))
  }

  test("setPacManDirection - forbidden direction Right") {
    val grid = parse(" #\n" +
										 " #")
    val board = Board(grid,
      characters = Map(
        PacMan -> (Position(0, 0), Some(Down))
      )
    )

    board.setPacManDirection(Right) should be (Board(grid,
      characters = Map(
        PacMan -> (Position(0, 0), None)
      )
    ))
  }

  test("setPacManDirection - forbidden direction Left") {
    val grid = parse("# \n" +
										 "# ")
    val board = Board(grid,
      characters = Map(
        PacMan -> (Position(1, 0), Some(Down))
      )
    )

    board.setPacManDirection(Left) should be (Board(grid,
      characters = Map(
        PacMan -> (Position(1, 0), None)
      )
    ))
  }

  test("setPacManDirection - forbidden direction Up") {
    val grid = parse("##\n" +
										 "  ")
    val board = Board(grid,
      characters = Map(
        PacMan -> (Position(0, 1), Some(Right))
      )
    )

    board.setPacManDirection(Up) should be (Board(grid,
      characters = Map(
        PacMan -> (Position(0, 1), None)
      )
    ))
  }

  test("setPacManDirection - forbidden direction Down") {
    val grid = parse("  \n" +
										 "##")
    val board = Board(grid,
      characters = Map(
        PacMan -> (Position(0, 0), Some(Right))
      )
    )

    board.setPacManDirection(Down) should be (Board(grid,
      characters = Map(
        PacMan -> (Position(0, 0), None)
      )
    ))
  }

  test("setPacManDirection - forbidden direction Right out of map") {
    val grid = parse("  \n" +
                     "  ")
    val board = Board(grid,
      characters = Map(
        PacMan -> (Position(1, 0), Some(Down))
      )
    )

    board.setPacManDirection(Right) should be (Board(grid,
      characters = Map(
        PacMan -> (Position(1, 0), None)
      )
    ))
  }

  test("setPacManDirection - forbidden direction Left out of map") {
    val grid = parse("  \n" +
                     "  ")
    val board = Board(grid,
      characters = Map(
        PacMan -> (Position(0, 0), Some(Down))
      )
    )

    board.setPacManDirection(Left) should be (Board(grid,
      characters = Map(
        PacMan -> (Position(0, 0), None)
      )
    ))
  }

  test("setPacManDirection - forbidden direction Up out of map") {
    val grid = parse("  \n" +
                     "  ")
    val board = Board(grid,
      characters = Map(
        PacMan -> (Position(0, 0), Some(Down))
      )
    )

    board.setPacManDirection(Up) should be (Board(grid,
      characters = Map(
        PacMan -> (Position(0, 0), None)
      )
    ))
  }

  test("setPacManDirection - forbidden direction Down out of map") {
    val grid = parse("  \n" +
                     "  ")
    val board = Board(grid,
      characters = Map(
        PacMan -> (Position(1, 1), Some(Left))
      )
    )

    board.setPacManDirection(Down) should be (Board(grid,
      characters = Map(
        PacMan -> (Position(1, 1), None)
      )
    ))
  }
}
