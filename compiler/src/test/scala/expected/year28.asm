l0:
    INBOX
    COPYTO 0
    INBOX
    COPYTO 1
    INBOX
    COPYTO 2
    COPYFROM 1
    SUB 0
    JUMPN l1
    JUMP l2
l1:
    COPYFROM 0
    COPYTO 3
    COPYFROM 1
    COPYTO 0
    COPYFROM 3
    COPYTO 1
l2:
    COPYFROM 2
    SUB 1
    JUMPN l3
    JUMP l4
l3:
    COPYFROM 2
    COPYTO 3
    COPYFROM 1
    COPYTO 2
    COPYFROM 3
    COPYTO 1
    SUB 0
    JUMPN l5
    JUMP l6
l5:
    COPYFROM 0
    COPYTO 4
    COPYFROM 1
    COPYTO 0
    COPYFROM 4
    COPYTO 1
l6:
l4:
    COPYFROM 0
    OUTBOX
    COPYFROM 1
    OUTBOX
    COPYFROM 2
    OUTBOX
    JUMP l0
