package elevator

/**
  * Simple simulator
  */
object Simulator {
  def handle(income: List[Option[Request]], system: ElevatorHistory)(implicit action: Option[(ElevatorHistory => Unit)] = None): ElevatorHistory = {
    for (optRequest <- income) {
      optRequest.foreach(req => system.pickup(req))
      system.step()
      action.foreach(f => f(system))
    }

    system
  }

  def isDone(system: ElevatorHistory): Boolean = {
    val elevatorSystem = system.last
    elevatorSystem.queue.isEmpty && elevatorSystem.elevators.forall(x => x.people.isEmpty)
  }
}
