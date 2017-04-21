package elevator

object Main extends App {
  override def main(args: Array[String]): Unit = {
    val system = ElevatorHistory(ElevatorSystem(List(), List(ElevatorState.empty(1), ElevatorState.empty(2)), 0, 5))

    Simulator.handle(
      List(Some(Request(0, 2)),
        None,
        None,
        None
      ),
      system)(Some((x: ElevatorHistory) => println(x.show)))

    println(Simulator.isDone(system))
  }
}
