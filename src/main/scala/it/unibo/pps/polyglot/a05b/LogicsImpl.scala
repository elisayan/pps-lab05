package it.unibo.pps.polyglot.a05b

import it.unibo.pps.polyglot.Pair
import it.unibo.pps.polyglot.a05b.Logics

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a05b/sol2/ */
class LogicsImpl(private val size: Int) extends Logics:

  private var tickCount = 0
  private val random = scala.util.Random()
  private val initial = Pair[Int, Int](random.nextInt(size - 2) + 1, random.nextInt(size - 2) + 1)

  override def tick(): Unit = tickCount = tickCount + 1

  override def isOver: Boolean = initial.getY - tickCount < 0 || initial.getY + tickCount >= size || initial.getX - tickCount < 0 || initial.getX + tickCount >= size

  override def hasElement(x: Int, y: Int): Boolean =
    (x == initial.getX && ((y - initial.getY).abs <= tickCount)) ||
      (y == initial.getY && ((x - initial.getX).abs <= tickCount)) ||
      (x - y == initial.getX - initial.getY && (x - initial.getX).abs <= tickCount) ||
      (x + y == initial.getX + initial.getY && (x - initial.getX).abs <= tickCount)

