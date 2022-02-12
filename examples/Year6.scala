import hrsm.*
import builtins.*

@main def year6: Unit =
  val program = hrassembly {
    var toAdd = uninitialized
    while true do
      toAdd = inbox
      outbox = inbox + toAdd
  }

  println(print(program))