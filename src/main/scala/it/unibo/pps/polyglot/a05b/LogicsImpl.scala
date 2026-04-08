package it.unibo.pps.polyglot.a05b

import it.unibo.pps.polyglot.Pair
import it.unibo.pps.polyglot.a05b.Logics

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a05b/sol2/ */
class LogicsImpl(private val size: Int) extends Logics:

  private var tickCount = 0
  private val random = scala.util.Random()
  private val initial = Pair(random.nextInt(size - 2) + 1, random.nextInt(size - 2) + 1)

  override def tick(): Unit = tickCount += 1

  override def isOver: Boolean =
    val x = initial.getX
    val y = initial.getY
    y - tickCount < 0 || y + tickCount >= size || x - tickCount < 0 || x + tickCount >= size

  override def hasElement(x: Int, y: Int): Boolean =
    val dx = (x - initial.getX).abs
    val dy = (y - initial.getY).abs
    val isVerticalOrHorizontal = (x == initial.getX && dy <= tickCount) || (y == initial.getY && dx <= tickCount)
    val isDiagonal = dx == dy && dx <= tickCount

    isVerticalOrHorizontal || isDiagonal

