import hrsm.Language.*
import hrsm.hrprocessor.given

// Statement: Output every non-zero from the input

@main
def year07: Unit =
  Year(7)(9){
    while True do
      val in: Variable[Int] = inbox
      if(in !== 0) then outbox = in
  }