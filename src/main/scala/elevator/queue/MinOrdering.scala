package elevator.queue

object MinOrdering extends Ordering[Int] {
  def compare(x: Int, y: Int): Int = y compare x
}
