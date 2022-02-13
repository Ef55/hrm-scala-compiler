import hrsm.Language.*

@main
def year06: Unit =
  Year(6){
    var toAdd = uninitialized
    while true do
      toAdd = inbox
      outbox = inbox + toAdd
  }
