import hrsm.Language.{*, given}

// Statement: Output every zero from the input

@main
def year09: Unit =
  Year(9)(9){
    while True do
      val in: Variable[Int] = inbox
      if(in === 0) then outbox = in
  }