package elevator

package object api {

  sealed trait Direction

  case object Up extends Direction

  case object Down extends Direction

  case object Idle extends Direction


  case object GetStatus

  case class Status(direction: Direction, currentFloor: Int, destinationFloor: Int, requestCount: Int)


  case class UserRequest(floor: Int, direction: Direction)

  case class MoveRequest(floor: Int)

  case object ProcessRequest

}
