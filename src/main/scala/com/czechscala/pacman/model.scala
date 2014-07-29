package com.czechscala.pacman

case class Board(
  grid: Array[Array[Cell]],
  objects: Map[Position, Object] = Map(),
  characters: Map[Character, (Position, Option[Direction])] = Map()
) {
  require(grid.map(_.length).toSet.size == 1)

  def width = grid(0).length
  def height = grid.length

  def moveAllCharacters: Board = ???
}

case class Position(x: Int, y: Int)

trait Object
class Gem extends Object

trait Cell
object Wall extends Cell
object Empty extends Cell

trait Character
case object PacMan extends Character
class Ghost extends Character

sealed trait Direction
case object Up extends Direction
case object Down extends Direction
case object Left extends Direction
case object Right extends Direction
