package polyglot.a01a

import polyglot.Pair
import polyglot.a01a.Logics
import polyglot.a01a.Logics.Result

import scala.collection.mutable

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01a/sol2/ */
class LogicsImpl(private val size: Int, private val boat: Int) extends Logics:
  private final val FAILURES: Int = 5
  private var hit: mutable.Set[Pair[Int, Int]] = mutable.HashSet()

  private val random = scala.util.Random (42)
  private val boatRow: Int = random.nextInt(size)
  private val boatLeftCol: Int = random.nextInt(size - boat + 1)
  private val boatSize: Int = boat
  private var failures: Int = 0
  println("x = " + this.boatLeftCol + " y = " + this.boatRow)
  
  def hit(row: Int, col: Int) : Result =
    if row == this.boatRow && col >= this.boatLeftCol && col < this.boatLeftCol + boatSize then
      hit.add(new Pair(row, col))
      if hit.size == boatSize then Result.WON else Result.HIT
    else
      failures = failures + 1
      if failures == FAILURES then Result.LOST else Result.MISS

