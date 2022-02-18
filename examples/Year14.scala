import hrsm.Language.*

// Statement: Output the max of every pair of inputs

@main
def year14: Unit =
  Year(14)(3){
    while true do
      val f = inbox
      val s = inbox
      if s < f then
        outbox = f
      else
        outbox = s
  }