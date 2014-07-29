package com.czechscala.pacman

case class Board(
  grid: Array[Array[Cell]],
  objects: Map[Position, Object] = Map(),
  characters: Map[Character, (Position, Option[Direction])] = Map()
) {
  require(grid.map(_.length).toSet.size == 1)

  def width = grid(0).length
  def height = grid.length

  def moveAllCharacters: Board = {
    val newChars = Map.newBuilder[Character, (Position, Option[Direction])]
    for((ch, (pos, dir)) <- characters) {
      newChars += (ch -> (pos.copy(pos.x + 1), dir))
    }
    this.copy(characters = newChars.result())
  }
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
