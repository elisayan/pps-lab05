package it.unibo.pps.polyglot.a01a

import it.unibo.pps.polyglot.a01a.Logics
import it.unibo.pps.polyglot.a01a.Logics.Result
import it.unibo.pps.util.Sequences.*
import Sequence.*

import scala.annotation.tailrec

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01a/sol2/ */
class LogicsImpl(private val size: Int, private val boat: Int) extends Logics:

  private val random = scala.util.Random()
  private val boatRow = random.nextInt(size)
  private val boatLeftCol = random.nextInt(size - boat + 1)
  private var hit: Sequence[Int] = Nil()
  private var misses = 0

  //println(s"Boat: Row $boatRow, Cols $boatLeftCol to ${boatLeftCol + boat - 1}")

  override def hit(row: Int, col: Int): Result = (row, col) match
    case (r, c) if r == boatRow && c >= boatLeftCol && c < boatLeftCol + boat =>
      if !contains(hit, c) then hit = Cons(c, hit)
      if length(hit) == boat then Result.WON else Result.HIT
    case _ => misses = misses + 1
      if misses == 5 then Result.LOST else Result.MISS

  @tailrec
  private def contains(seq: Sequence[Int], value: Int): Boolean = seq match {
    case Cons(h, t) => h == value || contains(t, value)
    case _ => false
  }

  private def length(seq: Sequence[Int]): Int = seq match {
    case Cons(_, t) => 1 + length(t)
    case _ => 0
  }

