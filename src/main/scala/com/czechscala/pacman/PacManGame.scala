package com.czechscala.pacman

import akka.actor.{Actor, ActorSystem, Props}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object PacManGame extends App {

  case object Tick

  case object NewGame

  case class Key(code: Int)

  class Engine extends Actor {
    private val renderer = new Swing(6, 5, self)
    private var state = Board(initialGrid, Map(Position(2, 2) -> new Gem), Map(PacMan -> (Position(1, 1), Some(Right)), new Ghost -> (Position(1,3), Some(Right))))
    private var delay = 1000

    override def preStart = {
      render
      tick
    }

    private def tick = context.system.scheduler.scheduleOnce(delay millis, self, Tick)

    def receive = {
      // case NewGame => state = State(Board(20, 30, Set()), None)

      case Tick =>
        state = state.moveAllCharacters
        render
        tick

      case dir: Direction =>
        state = state.setPacManDirection(dir)
        render
    }

    private def render = renderer render state

    def getDelay = delay
  }

  val system = ActorSystem("pacman")
  val engine = system.actorOf(Props[Engine])

  val initialGrid = parse(
    "######\n" +
    "#    #\n" +
    "#    #\n" +
    "#    #\n" +
    "######\n"
  )

  def parse(gridStr: String): Array[Array[Cell]] = gridStr split "\n" map (_ map {
    case '#' => Wall
    case _ => Empty
  } toArray)

}
