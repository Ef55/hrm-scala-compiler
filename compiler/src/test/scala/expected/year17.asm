l0:
    INBOX
    JUMPN l1
    INBOX
    JUMPN l3
    COPYFROM 4
    OUTBOX
    JUMP l4
l3:
    COPYFROM 5
    OUTBOX
l4:
    JUMP l2
l1:
    INBOX
    JUMPN l5
    COPYFROM 5
    OUTBOX
    JUMP l6
l5:
    COPYFROM 4
    OUTBOX
l6:
l2:
    JUMP l0
