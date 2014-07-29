package com.czechscala.pacman

case class Board(
  grid: Array[Array[Cell]],
  objects: Map[Position, Object],
  characters: Map[Character, (Position, Option[Direction])]
) {
  def width = grid(0).length
  def height = grid.length
}

case class Position(x: Int, y: Int)

trait Object
class Gem extends Object

trait Cell
case object Wall extends Cell
case object Empty extends Cell

trait Character
case object PacMan extends Character
class Ghost extends Character

sealed trait Direction
case object Up extends Direction
case object Down extends Direction
case object Left extends Direction
case object Right extends Direction
