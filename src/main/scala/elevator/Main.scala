package elevator

import akka.actor.ActorSystem

object Main {
  def main(args: Array[String]): Unit = {
    val system = ActorSystem.apply("test")

    val numElevators: Int = 4
    val minFloor: Int = 1
    val maxFloor: Int = 100
    val maxWeightLbs: Int = 700


    val elevators = List.fill(numElevators)(system.actorOf(Elevator.props(minFloor, maxFloor, maxWeightLbs)))
    val controller = system.actorOf(Controller.props(elevators))


    while(true){}

    println("Hello World!")
  }
}
