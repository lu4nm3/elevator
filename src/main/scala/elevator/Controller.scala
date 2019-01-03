package elevator

import akka.actor.{Actor, ActorRef, Cancellable, Props, Timers}
import elevator.api._

import scala.concurrent.duration._
import scala.language.postfixOps

class Controller(elevators: List[ActorRef]) extends Actor with Timers {

  import context._

  private var statusSchedules = List.empty[Cancellable]
  private var elevatorStatuses = Map.empty[ActorRef, Status]

  override def preStart(): Unit = {
    statusSchedules = elevators.map(elevator => system.scheduler.schedule(5 second, 5 second, elevator, GetStatus))
  }

  override def postStop(): Unit = {
    statusSchedules.foreach(_.cancel())
  }

  def receive: Receive = {
    case status: Status => elevatorStatuses = elevatorStatuses + (sender() -> status)
    case UserRequest(floor, direction) =>
      idleAtFloor(floor) match {
        case Some(elevator) => elevator ! MoveRequest(floor)
        case None => movingTowardsFloorInSameDirection(floor, direction) match {
          case Some(elevator) => elevator ! MoveRequest(floor)
          case None => idleAtClosestFloor(floor) match {
            case Some(elevator) => elevator ! MoveRequest(floor)
            case None => leastBusy.foreach(_ ! MoveRequest(floor))
          }
        }
      }
    case _ => ()
  }

  private def idleAtFloor(floor: Int): Option[ActorRef] = {
    elevatorStatuses
      .filter { case (_, status) => status.direction == Idle && status.currentFloor == floor }
      .keys
      .headOption
  }

  private def movingTowardsFloorInSameDirection(floor: Int, direction: Direction): Option[ActorRef] = {
    elevatorStatuses
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

  private def idleAtClosestFloor(floor: Int): Option[ActorRef] = {
    elevatorStatuses
      .toList
      .filter { case (_, status) => status.direction == Idle }
      .map { case (elevator, status) => (elevator, Math.abs(status.currentFloor - floor)) }
      .sortBy(_._2)
      .map(_._1)
      .headOption
  }

  private def leastBusy: Option[ActorRef] = {
    elevatorStatuses
      .toList
      .sortBy(_._2.requestCount)
      .map(_._1)
      .headOption
  }
}

object Controller {
  def props(elevators: List[ActorRef]): Props = Props(new Controller(elevators))
}
