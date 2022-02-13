import hrsm.Language.*

// Statement: Output every non-zero from the input

@main
def year07: Unit =
  Year(7){
    while true do
      val in = inbox
      if(in != 0) then outbox = in
  }