package elevator

import akka.actor.{Actor, ActorRef}
import elevator.api._

class Controller(elevators: Map[ActorRef, Status]) extends Actor {

  def idleAtFloor(floor: Int): Option[ActorRef] = {
    elevators
      .filter { case (_, status) => status.direction == Idle && status.currentFloor == floor }
      .keys
      .headOption
  }

  def movingTowardsFloorInSameDirection(floor: Int, direction: Direction): Option[ActorRef] = {
    elevators
      .filter { case (_, status) =>
        direction match {
          case Up if direction == Up => status.currentFloor <= floor && floor <= status.destinationFloor
          case Down if direction == Down => status.currentFloor >= floor && floor >= status.destinationFloor
          case _ => false
        }
      }
      .keys
      .headOption
  }

  def idleAtClosestFloor(floor: Int): Option[ActorRef] = {
    elevators
      .toList
      .filter { case (_, status) => status.direction == Idle }
      .map { case (elevator, status) => (elevator, Math.abs(status.currentFloor - floor)) }
      .sortBy(_._2)
      .map(_._1)
      .headOption
  }

  def leastBusy: Option[ActorRef] = {
    elevators
      .toList
      .sortBy(_._2.requestCount)
      .map(_._1)
      .headOption
  }

  def receive: Receive = {
    case UserRequest(floor, direction) =>
      if

    case _ => ()
  }
}

object Controller {

}