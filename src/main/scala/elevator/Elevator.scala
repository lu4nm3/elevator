package elevator

import akka.actor.{Actor, Props, Stash, Timers}
import com.typesafe.scalalogging.LazyLogging
import elevator.Elevator._
import elevator.api._
import elevator.queue.{MaxOrdering, MinOrdering}

import scala.collection.mutable
import scala.concurrent.duration._
import scala.language.postfixOps

class Elevator(minFloor: Int, maxFloor: Int, maxWeightLbs: Int) extends Actor with LazyLogging with Timers with Stash {

  import context._

  // min heap
  private val upQueue: mutable.PriorityQueue[Int] = mutable.PriorityQueue.empty[Int](MinOrdering)

  // max heap
  private val downQueue: mutable.PriorityQueue[Int] = mutable.PriorityQueue.empty[Int](MaxOrdering)

  var currentFloor: Int = 1
  var currentWeightLbs: Int = 0

  override def preStart(): Unit = {
    become(idle)
  }

  val receive: Receive = {
    case m@_ => logger.warn(s"Received message $m before initialization")
  }

  val idle: Receive = {
    case GetStatus => sender() ! Status(Idle, currentFloor, currentFloor, upQueue.length + downQueue.length)
    case MoveRequest(floor) =>
      if (floor < currentFloor) {
        downQueue += floor
        self ! ProcessRequest
        become(down)
      } else if (floor > currentFloor) {
        upQueue += floor
        self ! ProcessRequest
        become(up)
      } else {
        timers.startSingleTimer("OpenDoors", CloseDoors, 5 second)
        become(openDoors, discardOld = false)
      }
  }

  private val sharedUpDown: Receive = {
    case MoveRequest(floor) if floor >= minFloor && floor <= maxFloor =>
      if (floor < currentFloor) {
        downQueue += floor
        self ! ProcessRequest
      } else if (floor > currentFloor) {
        upQueue += floor
        self ! ProcessRequest
      } else {
        timers.startSingleTimer("OpenDoors", CloseDoors, 5 second)
        become(openDoors, discardOld = false)
      }
    case MoveRequest(floor) => logger.warn(s"Invalid floor $floor was requested.")
  }

  val up: Receive = sharedUpDown orElse {
    case GetStatus => sender() ! Status(Up, currentFloor, upQueue.last, upQueue.length + downQueue.length)
    case ProcessRequest =>
      if (upQueue.isEmpty && downQueue.isEmpty) {
        become(idle)
      } else if (upQueue.isEmpty) {
        become(down)
      } else if (currentFloor == upQueue.head) {
        currentFloor = upQueue.dequeue()
        timers.startSingleTimer("OpenDoors", CloseDoors, 5 second)
        become(openDoors, discardOld = false)
      } else {
        currentFloor += 1
        self ! ProcessRequest
      }
  }

  val openDoors: Receive = {
    case WeightChange(lbs) => currentWeightLbs += lbs
    case CloseDoors =>
      if (currentWeightLbs > maxWeightLbs) {
        soundAlarm()
        timers.startSingleTimer("OpenDoors", CloseDoors, 5 second)
      } else {
        self ! ProcessRequest
        unstashAll()
        unbecome()
      }
    case _ => stash()
  }

  private def soundAlarm(): Unit = {
    logger.error(s"Current elevator weight $currentWeightLbs is over the safety maximum")
  }

  val down: Receive = sharedUpDown orElse {
    case GetStatus => sender() ! Status(Down, currentFloor, downQueue.last, upQueue.length + downQueue.length)
    case ProcessRequest =>
      if (downQueue.isEmpty && upQueue.isEmpty) {
        become(idle)
      } else if (downQueue.isEmpty) {
        become(up)
      } else if (currentFloor == downQueue.head) {
        currentFloor = downQueue.dequeue()
        timers.startSingleTimer("OpenDoors", CloseDoors, 5 second)
        become(openDoors, discardOld = false)
      } else {
        currentFloor -= 1
        self ! ProcessRequest
      }
  }
}

object Elevator {

  def props(minFloor: Int,
            maxFloor: Int,
            maxWeightLbs: Int): Props = Props(new Elevator(minFloor, maxFloor, maxWeightLbs))


  case object CloseDoors

  case class WeightChange(lbs: Int)

}