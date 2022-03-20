import hrsm.Language.*
import hrsm.hrprocessor.given

// Statement: Sort (and output) every 3-uple from the input

@main
def year28Sorting: Unit =
  Year(28)(10){
    while True do
      var fst: Variable[Int] = inbox
      var snd: Variable[Int] = inbox
      var trd: Variable[Int] = inbox

      if snd < fst then
        val tmp = fst
        fst = snd
        snd = tmp

      if trd < snd then
        val tmp = trd
        trd = snd
        snd = tmp
        if snd < fst then
          val tmp = fst
          fst = snd
          snd = tmp
      
      outbox = fst
      outbox = snd
      outbox = trd
  }