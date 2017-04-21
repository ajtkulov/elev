package elevator

import elevator.Types.Floor

/**
  * Typeclass for Visualization/introspection
  *
  * @tparam T type
  */
trait Show[T] {
  def show(value: T): String
}

/**
  * Implementation for Show-typeclass
  */
object Show {
  def apply[T](implicit t: Show[T]): Show[T] = t

  /**
    * Extension for typeclass availability
    *
    * @param value value
    * @tparam T type
    */
  implicit class Showable[T](value: T) {
    def show(implicit t: Show[T]): String = t.show(value)
  }

  implicit val simple: Show[ElevatorSystem] = new Show[ElevatorSystem] {
    override def show(value: ElevatorSystem): String = {
      s"""
         |elevs = ${value.elevators.mkString(", ")}
         |queue = ${value.queue.mkString(", ")}
       """.stripMargin
    }
  }

  val graph: Show[ElevatorSystem] = new Show[ElevatorSystem] {
    override def show(value: ElevatorSystem): String = {
      val output = Array.fill(value.maxFloor + 2, value.elevators.size + 3)(' ')

      val sorted = value.elevators.sortBy(_.id)
      for ((elevator, idx) <- sorted.zipWithIndex) {
        if (elevator.people.nonEmpty) {
          output(elevator.floor + 1)(idx) = '.'
        }
        output(elevator.floor)(idx) = (48 + idx).toChar
      }

      val toSet: Set[Floor] = value.queue.map(x => x.floor).toSet
      toSet.foreach((floor: Floor) => {
        output(floor)(sorted.size + 2) = '*'
      })

      "---------" + "\n" + output.reverse.map(_.mkString("")).mkString("\n") + "\n" + "^^^^^^^^^" + "\n\n"
    }
  }
}
