package elevator.queue

object MaxOrdering extends Ordering[Int] {
  def compare(x: Int, y: Int): Int = x compare y
}
