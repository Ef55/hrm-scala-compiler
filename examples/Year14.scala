import hrsm.Language.*

// Statement: Output the max of every pair of inputs

@main
def year14: Unit =
  Year(14){
    var f = uninitialized
    var s = uninitialized
    while true do
      f = inbox
      s = inbox
      if s - f < 0 then
        outbox = f
      else
        outbox = s
  }