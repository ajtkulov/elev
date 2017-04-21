import elevator._
import org.scalatest.FunSuite
import Show._

class Tests extends FunSuite {

  //just comment/uncomment this line to switch off/on output during test.
  implicit val action: Option[(ElevatorHistory => Unit)] = Some((x: ElevatorHistory) => println(x.show))
  //Otherwise, use doubleAction for precious info
  val doubleAction: Option[(ElevatorHistory => Unit)] = Some((x: ElevatorHistory) => {
    println(x.show)
    println(x.last.show(Show.simple))
  })

  test("Test - 1") {
    val system = ElevatorHistory(ElevatorSystem(List(), List(ElevatorState.empty(1), ElevatorState.empty(2)), 0, 5))

    Simulator.handle(
      List(Some(Request(0, 2)),
        None,
        None,
        None
      ),
      system)

    assert(Simulator.isDone(system))
  }

  test("Test - 2") {
    val system = ElevatorHistory(ElevatorSystem(List(), List(ElevatorState.empty(1), ElevatorState.empty(2)), 0, 5))

    Simulator.handle(
      List(Some(Request(0, 2)),
        None,
        Some(Request(1, 3)),
        None,
        None,
        None,
        None
      ),
      system)

    assert(Simulator.isDone(system))
  }

  test("Test - 3") {
    val system = ElevatorHistory(ElevatorSystem(List(), List(ElevatorState.empty(1), ElevatorState.empty(2)), 0, 5))

    Simulator.handle(
      List(Some(Request(0, 2)),
        None,
        Some(Request(0, 4)),
        None,
        None,
        None,
        None,
        None
      ),
      system)

    assert(Simulator.isDone(system))
  }

  test("Test - 4") {
    val system = ElevatorHistory(ElevatorSystem(List(), List(ElevatorState.empty(1), ElevatorState.empty(2)), 0, 5))

    Simulator.handle(
      List(Some(Request(0, 2)),
        None,
        Some(Request(0, 4)),
        Some(Request(5, 0)),
        None,
        None,
        None,
        None
      ) ++ List.fill(8)(None),
      system)

    assert(Simulator.isDone(system))
  }

  test("Test - 5") {
    val system = ElevatorHistory(ElevatorSystem(List(), List(ElevatorState.empty(1), ElevatorState.empty(2)), 0, 5))

    Simulator.handle(
      List(Some(Request(0, 2)),
        None,
        Some(Request(0, 4)),
        Some(Request(5, 0)),
        Some(Request(0, 5)),
        None,
        None,
        None
      ) ++ List.fill(15)(None),
      system)

    assert(Simulator.isDone(system))
  }
}