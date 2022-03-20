import hrsm.Language.{*, given}

// Statement: Output the max of every pair of inputs

@main
def year14: Unit =
  Year(14)(3){
    while True do
      val f: Variable[Int] = inbox
      val s: Variable[Int] = inbox
      if s < f then
        outbox = f
      else
        outbox = s
  }