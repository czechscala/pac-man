package com.czechscala.pacman

import java.awt.event.{KeyEvent, KeyListener}
import java.awt.{Color, Dimension, Graphics}
import javax.swing.{JFrame, JPanel, SwingUtilities}

import akka.actor.ActorRef

class Swing(width: Int, height: Int, keyListener: ActorRef) extends Renderer {

  private val frame = new JFrame("PacMan")
  private val canvas = new Canvas

  swing {
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.addKeyListener(Keyboard)
    frame.add(canvas)
    frame.pack()
    frame.setVisible(true)
  }

  override def render(board: Board): Unit = {
    canvas.refresh(board)
  }

  private class Canvas extends JPanel {

    private final val RectSizePx = 30
    private final val PacManSizePx = 26
    private final val GemSizePx = 10
    private final val GhostEyeSizePx = 5
    private final val GhostSizePx = 26

    private final val PacManBorderPx = border(PacManSizePx)
    private final val GhostBorderPx = border(GhostSizePx)

    private final val BoardColor = Color.WHITE
    private final val GemColor = Color.RED
    private final val PacManColor = Color.ORANGE
    private final val WallColor = Color.BLACK

    var currentState: Option[Board] = None

    def refresh(board: Board): Unit = {
      currentState = Some(board)
      this.repaint()
    }

    override def paintComponent(graphics: Graphics) = {
      super.paintComponent(graphics)

      currentState match {
        case Some(board) =>
          clear(graphics)

          for {
            (row, y) <- board.grid.zipWithIndex
            (cell, x) <- row.zipWithIndex
          } {
            cell match {
              case Wall => drawRectangle(x, y, graphics)
              case Empty => // ignore
            }
          }

          for ((position, obj) <- board.objects) {
            obj match {
              case g: Gem => drawGem(position.x, position.y, graphics)
            }
          }

          for ((character, (position, direction)) <- board.characters) {
            character match {
              case PacMan => drawPacMan(position.x, position.y, direction, graphics)
              case ghost: Ghost => drawGhost(position.x, position.y, Color.DARK_GRAY, direction, graphics)
            }
          }

        case None => // ignore
      }
    }

    private def border(size: Int) = (RectSizePx - size) / 2

    private def centerOval(size: Int) = (RectSizePx / 2) - (size / 2)

    private def coord2px(coordinate: Int) = coordinate * RectSizePx

    private def drawRectangle(x: Int, y: Int, g: Graphics) = {
      g.setColor(WallColor)
      g.fillRect(coord2px(x), coord2px(y), RectSizePx, RectSizePx)
    }

    private def drawPacMan(x: Int, y: Int, direction: Option[Direction], g: Graphics) = {
      g.setColor(PacManColor)

      val angle = direction match {
        case None => 30
        case Some(dir) if dir == Up => 120
        case Some(dir) if dir == Down => 300
        case Some(dir) if dir == Right => 30
        case Some(dir) if dir == Left => 210
      }

      g.fillArc(coord2px(x) + PacManBorderPx, coord2px(y) + PacManBorderPx, PacManSizePx, PacManSizePx, angle, 300)
    }

    private def drawGhost(x: Int, y: Int, color: Color, direction: Option[Direction], g: Graphics) = {
      g.setColor(color)
      g.fillRect(coord2px(x) + GhostBorderPx, coord2px(y + 1) - (RectSizePx / 2), GhostSizePx, RectSizePx / 2)
      g.fillArc(coord2px(x) + GhostBorderPx, coord2px(y) + GhostBorderPx, GhostSizePx, GhostSizePx, 0, 180)

      val RectSizeThird = RectSizePx / 3
      val EyeSizeHalf = GhostEyeSizePx / 2
      val ((leftX, leftY), (rightX, rightY)) = direction match {
        case None => ((RectSizeThird, RectSizeThird), (2 * RectSizeThird, RectSizeThird))
        case Some(dir) if dir == Up => ((RectSizeThird - EyeSizeHalf, RectSizeThird - GhostEyeSizePx + 3),
            (2 * RectSizeThird - EyeSizeHalf, RectSizeThird - GhostEyeSizePx + 3))
        case Some(dir) if dir == Down => ((RectSizeThird - EyeSizeHalf, RectSizeThird + GhostEyeSizePx), (2 * RectSizeThird - EyeSizeHalf,
            RectSizeThird + GhostEyeSizePx))
        case Some(dir) if dir == Right => ((RectSizeThird, RectSizeThird), (2 * RectSizeThird, RectSizeThird))
        case Some(dir) if dir == Left => ((RectSizeThird - GhostEyeSizePx, RectSizeThird), (2 * RectSizeThird - GhostEyeSizePx, RectSizeThird))
      }

      g.setColor(Color.RED)
      g.fillOval(coord2px(x) + leftX, coord2px(y) + leftY, GhostEyeSizePx, GhostEyeSizePx)
      g.fillOval(coord2px(x) + rightX, coord2px(y) + rightY, GhostEyeSizePx, GhostEyeSizePx)
    }

    private def drawGem(x: Int, y: Int, g: Graphics) = {
      val centerOffset = centerOval(GemSizePx)
      g.setColor(GemColor)
      g.fillOval(coord2px(x) + centerOffset, coord2px(y) + centerOffset, GemSizePx, GemSizePx)
    }

    private def clear(g: Graphics) = {
      g.setColor(BoardColor)
      g.clearRect(0, 0, widthPx, heightPx)
    }

    override def getPreferredSize = new Dimension(widthPx, heightPx)

    private def widthPx: Int = width * RectSizePx

    private def heightPx: Int = height * RectSizePx

  }

  private object Keyboard extends KeyListener {
    override def keyPressed(e: KeyEvent): Unit = {
      val keyPress = e.getKeyCode match {
        case KeyEvent.VK_LEFT => Some(Left)
        case KeyEvent.VK_RIGHT => Some(Right)
        case KeyEvent.VK_DOWN => Some(Down)
        case KeyEvent.VK_UP => Some(Up)
        case _ => None
      }

      keyPress match {
        case Some(ev) => keyListener ! ev
        case None => // ignore
      }
    }

    override def keyTyped(e: KeyEvent): Unit = ()

    override def keyReleased(e: KeyEvent): Unit = ()
  }

  private def swing(block: => Unit): Unit = {
    SwingUtilities.invokeLater(
      new Runnable() {
        def run(): Unit = {
          block
        }
      }
    )
  }
}

object Swing extends App {

  /** TEST */
  val swing = new Swing(3, 3, null)
  swing.render(
    Board(
      Array(Array(Wall, Wall, Empty), Array(Wall, Empty, Wall), Array(Empty, Wall, Wall)),
      Map(Position(1, 1) -> new Gem), Map(PacMan ->(Position(2, 0), Some(Left)), new Ghost ->(Position(0, 2), Some(Up)))
    )
  )
}
