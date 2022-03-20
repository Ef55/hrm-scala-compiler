import hrsm.Language.*

// Statement: Sort (and output) every 3-uple from the input

@main
def year28Implicit: Unit =
  Year(28)(10){
    while true do
      val fst = inbox
      val snd = inbox
      val trd = inbox

      if fst < snd then
        if snd < trd then
          outbox = fst
          outbox = snd
          outbox = trd
        else
          if fst < trd then
            outbox = fst
            outbox = trd
            outbox = snd
          else
            outbox = trd
            outbox = fst
            outbox = snd
      else
        if fst < trd then
          outbox = snd
          outbox = fst
          outbox = trd
        else
          if snd < trd then
            outbox = snd
            outbox = trd
            outbox = fst
          else
            outbox = trd
            outbox = snd
            outbox = fst
  }