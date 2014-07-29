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
                     "######\n")
    grid map (_.toList.mkString) foreach println
    val board = Board(grid, characters = Map(PacMan -> startPosition))

    board.moveAllCharacters should be (Board(grid, characters = Map(PacMan -> endPosition)))
  }

  test("moveAllCharacters - PacMan moves right") {
    pacManMoveTest(Position(1, 1) -> Some(Right), Position(2, 1) -> Some(Right))
  }

  test("moveAllCharacters - PacMan moves left") {
    pacManMoveTest(Position(4, 1) -> Some(Left), Position(3, 1) -> Some(Left))
  }

  test("moveAllCharacters - PacMan moves up to wall and stops") {
    pacManMoveTest(Position(1, 2) -> Some(Right), Position(1, 1) -> None)
  }

  test("moveAllCharacters - PacMan moves down to wall and stops") {
    pacManMoveTest(Position(1, 1) -> Some(Down), Position(1, 2) -> None)
  }
}
