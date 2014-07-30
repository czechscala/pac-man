package com.czechscala.pacman

case class Board(
  grid: Array[Array[Cell]],
  objects: Map[Position, Object] = Map(),
  characters: Map[Character, (Position, Option[Direction])] = Map()
) {
  require(grid.map(_.length).toSet.size == 1)

  def width = grid(0).length
  def height = grid.length

  def cellAt(pos: Position): Cell = grid(pos.y)(pos.x)

  def getNextPosition(pos: Position, d: Option[Direction]) = d match {
    case None => pos
    case Some(Right) => pos.copy(x = pos.x + 1)
    case Some(Left) => pos.copy(x = pos.x - 1)
    case Some(Up) => pos.copy(y = pos.y - 1)
    case Some(Down) => pos.copy(y = pos.y + 1)
  }

  def moveAllCharacters: Board = {
    val newChars = Map.newBuilder[Character, (Position, Option[Direction])]
    for((ch, (pos, dir)) <- characters) {
      val nextPos = getNextPosition(pos, dir)
      val nextDirection = checkDirectionValidity(nextPos, dir)
      newChars += (ch -> (nextPos, nextDirection))
    }
    this.copy(characters = newChars.result())
  }

  def isOnBoard(pos: Position): Boolean = if(pos.x < 0 || pos.x >= width || pos.y < 0 || pos.y >= height) false else true

  def checkDirectionValidity(pos: Position, dir: Option[Direction]): Option[Direction] = if(isOnBoard(getNextPosition(pos, dir)) && cellAt(getNextPosition(pos, dir)) == Empty) dir else None

  def setPacManDirection(direction: Direction): Board = copy(characters = characters map {
    case (PacMan, (position, _)) => (PacMan, (position, checkDirectionValidity(position, Some(direction))))
    case x => x
  })
}

case class Position(x: Int, y: Int)

trait Object
class Gem extends Object

trait Cell
object Wall extends Cell { override def toString = "#" }
object Empty extends Cell { override def toString = "_" }

trait Character
case object PacMan extends Character
class Ghost extends Character

sealed trait Direction
case object Up extends Direction
case object Down extends Direction
case object Left extends Direction
case object Right extends Direction
