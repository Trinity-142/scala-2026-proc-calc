import scala.util.boundary
import boundary.break

/** Software implementation of PROC (PROstoy Calculator) mk. 1 (or mk. 2).
  *
  * You should finish this procedure according to
  * the reference described in `README.md` to complete
  * the assignment.
  */
@main def calculator(commands: String*): Unit = {
  /** Converts given string `s` to integer.
    *
    * Throws [[NumberFormatException]] if `s` can't be converted to integer,
    * but you shouldn't worry about it at this moment.
    */
  def parseInt(s: String): Int = s.toInt

  case class State(acc: Int = 0, A: Int = 0, B: Int = 0, blink: Boolean = false)
  val state = State()

  val finalState = boundary[State]:
    commands.foldLeft(state) { (currentState, command) =>
      command match {
        case "+" => currentState.copy(acc = currentState.A + currentState.B, blink = false)
        case "-" => currentState.copy(acc = currentState.A - currentState.B, blink = false)
        case "*" => currentState.copy(acc = currentState.A * currentState.B, blink = false)
        case "/" =>
          if (currentState.B == 0) currentState.copy(0, 0, 0)
          else currentState.copy(acc = currentState.A / currentState.B, blink = false)
        case "swap" => currentState.copy(A = currentState.B, B = currentState.A)
        case "blink" => currentState.copy(blink = !currentState.blink)
        case "acc" =>
          if (currentState.blink) currentState.copy(B = currentState.acc, blink = !currentState.blink)
          else currentState.copy(A = currentState.acc, blink = !currentState.blink)
        case "break" => break(currentState)
        case _ =>
          val X = parseInt(command)
          if (currentState.blink) currentState.copy(B = X, blink = !currentState.blink)
          else currentState.copy(A = X, blink = !currentState.blink)
      }
    }

  println(finalState.acc)
}
