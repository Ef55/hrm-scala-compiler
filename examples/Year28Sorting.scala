import hrsm.Language.*

// Statement: Sort (and output) every 3-uple from the input

@main
def year28Sorting: Unit =
  Year(28)(10){
    while true do
      var fst = inbox
      var snd = inbox
      var trd = inbox

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