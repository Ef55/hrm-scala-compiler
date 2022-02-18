import hrsm.Language.*

// Statement: Output every zero from the input

@main
def year09: Unit =
  Year(9)(9){
    while true do
      val in = inbox
      if(in == 0) then outbox = in
  }