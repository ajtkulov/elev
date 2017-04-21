package elevator

import Types._

import scala.collection.immutable.Seq
import Show._

/**
  * Basic types
  */
object Types {
  type ElevatorId = Int
  type Floor = Int
  type GoalFloor = Int
  type DirectionUp = Boolean
}

/**
  * Companion for ElevatorState
  */
object ElevatorState {
  def empty(id: ElevatorId): ElevatorState = {
    ElevatorState(id, 0, None, Nil, None)
  }
}

/**
  * Elevator state
  *
  * @param id        id
  * @param floor     current floor
  * @param goalFloor goal floor
  * @param people    info about people inside
  * @param direction movement direction
  * @param freeze    freeze time (in case loading/unloading)
  */
case class ElevatorState(id: ElevatorId, floor: Floor, goalFloor: Option[GoalFloor], people: Seq[Request], direction: Option[DirectionUp], freeze: Int = 0) {

  protected def nextGoalFloorAfterDeq(newDirection: Option[DirectionUp]): Option[GoalFloor] = {
    if (newDirection.isEmpty || goalFloor.get == floor) {
      None
    } else {
      goalFloor
    }
  }

  protected def nextFloorAfterPickup(request: Request): Option[GoalFloor] = {
    (goalFloor, direction) match {
      case (None, _) => Some(request.goalFloor)
      case (Some(goal), Some(true)) if goal < floor => Some(floor)
      case (Some(goal), Some(false)) if goal > floor => Some(floor)
      case _ => goalFloor
    }
  }

  protected def nextDirectionAfterPickup(request: Request): Option[DirectionUp] = {
    direction match {
      case None => Some(request.direction)
      case _ => direction
    }
  }

  def canConsume(request: Request): Boolean = {
    floor == request.floor && (direction.isEmpty || direction.get == request.direction)
  }

  def pickup(request: Request): ElevatorState = {
    assert(canConsume(request))

    copy(goalFloor = nextFloorAfterPickup(request), people = people :+ request, direction = nextDirectionAfterPickup(request), freeze = freeze + 1)
  }

  def needUnload: Boolean = {
    people.exists(person => person.goalFloor == floor)
  }

  def unload(): ElevatorState = {
    val filteredPeople = people.filterNot(person => person.goalFloor == floor)
    val newDirection = if (filteredPeople.isEmpty) {
      None
    } else {
      direction
    }

    copy(goalFloor = nextGoalFloorAfterDeq(newDirection), people = filteredPeople, direction = newDirection, freeze = freeze + 1)
  }

  def move(implicit system: ElevatorSystem): ElevatorState = {
    if (freeze > 0) {
      copy(freeze = freeze - 1)
    } else if (goalFloor.isDefined && floor == goalFloor.get) {
      copy(goalFloor = None, direction = None)
    } else {
      direction match {
        case None => ElevatorSystem.moveAfterHibernation(this, system)
        case Some(true) => copy(floor = floor + 1)
        case Some(false) => copy(floor = floor - 1)
      }
    }
  }
}

/**
  * Person request
  *
  * @param floor     current floor
  * @param goalFloor goal floor
  */
case class Request(floor: Floor, goalFloor: GoalFloor) {
  assert(floor != goalFloor)

  def direction: DirectionUp = goalFloor > floor
}

/**
  * Basic trait for elevator system controls
  */
trait ElevatorControlSystem {
  def status(): Seq[ElevatorState]

  def pickup(req: Request): Unit

  def step(): Unit
}

/**
  * Elevator system
  *
  * @param queue     queue of incoming request
  * @param elevators info about elevators
  * @param minFloor  lower floor in building
  * @param maxFloor  upper floor in building
  */
case class ElevatorSystem(queue: List[Request], elevators: List[ElevatorState], minFloor: Int = 0, maxFloor: Int = 10) {

  protected def updateElevatorStates(newState: List[ElevatorState]): List[ElevatorState] = {
    val toRemove = newState.map(_.id).toSet
    elevators.filterNot(p => toRemove.contains(p.id)) ++ newState
  }

  def pickup(request: Request): ElevatorSystem = {
    copy(queue = queue :+ request)
  }

  def move(): ElevatorSystem = {
    val toLeave = elevators.filter(e => e.needUnload).map(x => x.unload())
    if (toLeave.nonEmpty) {
      copy(elevators = updateElevatorStates(toLeave)).move()
    } else {
      val allPairs: Seq[(Request, ElevatorState)] =
        for (request <- queue; elevator <- elevators; if elevator.canConsume(request)) yield {
          (request, elevator)
        }

      val unique: Seq[(Request, ElevatorState)] = allPairs.groupBy(_._1).mapValues(x => x.head._2).toList
      val toRemoveRequest = unique.map(_._1).toSet
      val added: List[ElevatorState] = unique.map(x => x._2.pickup(x._1)).toList
      if (added.nonEmpty) {
        copy(queue.filterNot(request => toRemoveRequest.contains(request)), elevators = updateElevatorStates(added)).move()
      } else {
        val newStates = elevators.map(x => x.move(this))
        copy(elevators = newStates)
      }
    }
  }
}

/**
  * Companion object for system
  */
object ElevatorSystem {
  def moveAfterHibernation(elevator: ElevatorState, system: ElevatorSystem): ElevatorState = {
    val queue = system.queue
    if (queue.isEmpty) {
      elevator
    } else {
      val maxRequest: Request = queue.maxBy(_.floor)
      if (elevator.floor < maxRequest.floor) {
        elevator.copy(goalFloor = Some(maxRequest.floor), direction = Some(true))
      } else if (elevator.floor != system.minFloor) {
        elevator.copy(goalFloor = Some(system.minFloor), direction = Some(false))
      } else {
        elevator
      }
    }
  }
}

/**
  * Elevator system with inner state (whole history)
  */
trait ElevatorHolder extends ElevatorControlSystem {
  val initSystem: ElevatorSystem
  val systems: scala.collection.mutable.Stack[ElevatorSystem]

  override def status(): Seq[ElevatorState] = {
    systems.top.elevators
  }

  override def pickup(req: Request): Unit = {
    systems.push(systems.top.pickup(req))
  }

  override def step(): Unit = {
    systems.push(systems.top.move())
  }

  def show: String = systems.top.show(Show.graph)

  def last: ElevatorSystem = systems.top
}

/**
  * Implementation for ElevatorHolder
  *
  * @param initSystem initial state
  */
case class ElevatorHistory(initSystem: ElevatorSystem) extends ElevatorHolder {
  val systems = scala.collection.mutable.Stack[ElevatorSystem](initSystem)
}
