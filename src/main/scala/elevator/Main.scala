package elevator

import akka.actor.ActorSystem
import elevator.api.MoveRequest

object Main {
  def main(args: Array[String]): Unit = {
    val system = ActorSystem.apply("test")

    val elevator = system.actorOf(Elevator.props(1, 199, 500))

    elevator ! MoveRequest(4)

    while(true){}

    val test = new Test(3)
    println("Hello World!")
    println(s"times 2 ${test.timeTwo}")
  }
}
