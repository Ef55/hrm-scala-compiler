l0:
    INBOX
    COPYTO 0
    INBOX
    COPYTO 1
    SUB 0
    JUMPN l1
    COPYFROM 1
    OUTBOX
    JUMP l2
l1:
    COPYFROM 0
    OUTBOX
l2:
    JUMP l0
