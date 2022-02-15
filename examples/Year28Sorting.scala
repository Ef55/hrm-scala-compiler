import hrsm.Language.*

// Statement: Sort (and output) every 3-uple from the input

@main
def year28Sorting: Unit =
  Year(28){
    var fst = uninitialized
    var snd = uninitialized
    var trd = uninitialized
    var tmp = uninitialized
    while true do
      fst = inbox
      snd = inbox
      trd = inbox

      if snd < fst then
        tmp = fst
        fst = snd
        snd = tmp

      if trd < snd then
        tmp = trd
        trd = snd
        snd = tmp
        if snd < fst then
          tmp = fst
          fst = snd
          snd = tmp
      
      outbox = fst
      outbox = snd
      outbox = trd
  }