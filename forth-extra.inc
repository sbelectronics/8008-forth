;    5 1+ prstack  -> 0006
;
;    11 2+ prstack  -> 000D
;
;    5 1- prstack  -> 0004
;
;    11 2- prstack  -> 0009
;
;    : foo 0 if 001 else 002 then ; foo prstack  -> 0002
;           
;    : bar 1 if 001 else 002 then ; bar prstack  -> 0001
;
;    : printx [ char x ] literal emit ; printx  -> x
;
;    : whiletest 3 BEGIN [ CHAR X ] LITERAL EMIT 1- DUP 0= UNTIL ; whiletest  -> XXX
;
;    : whiletruespace BEGIN SPACE AGAIN ;   -> spaces forever
;
;    : echokeys BEGIN KEY PRHEX SPACE AGAIN ;
;
;    : foo 0 KEY DUP [ CHAR ( ] LITERAL = IF DROP 1+ ELSE [ CHAR ) ] LITERAL = FI THEN PRSTACK ;   -> crashes
;
;    : bar 0 KEY DUP [ CHAR ( ] LITERAL = ;

name_DIV:       db lo(LASTWORD_KERNEL), hi(LASTWORD_KERNEL)
                db 1,'/'
cw_DIV:         db lo(DOCOL),hi(DOCOL)
                db lo(cw_DIVMOD),hi(cw_DIVMOD)
                db lo(cw_SWAP),hi(cw_SWAP)
                db lo(cw_DROP),hi(cw_DROP)
                db lo(EXIT),hi(EXIT)


name_MOD:       db lo(name_DIV), hi(name_DIV)
                db 3,'M','O','D'
cw_MOD:         db lo(DOCOL),hi(DOCOL)
                db lo(cw_DIVMOD),hi(cw_DIVMOD)
                db lo(cw_DROP),hi(cw_DROP)
                db lo(EXIT),hi(EXIT)


name_NEWLINE:   db lo(name_MOD), hi(name_MOD)
                db 4,"'","\\","n","'"
cw_NEWLINE:     db lo(code_NEWLINE), hi(code_NEWLINE)
code_NEWLINE:   pushconst NEWLINE
                jmp next


name_LPCONST:   linklast NEWLINE
                db 3,"'","(","'"
cw_LPCONST:     db lo(code_LPCONST), hi(code_LPCONST)
code_LPCONST:   pushconst '('
                jmp next


name_RPCONST:   linklast LPCONST
                db 3,"'",")","'"
cw_RPCONST:     db lo(code_RPCONST), hi(code_RPCONST)
code_RPCONST:   pushconst ')'
                jmp next


name_ZEROCONST: linklast RPCONST
                db 3,"'","0","'"
cw_ZEROCONST:   db lo(code_ZEROCONST), hi(code_ZEROCONST)
code_ZEROCONST: pushconst '0'
                jmp next


name_ACONST:    linklast ZEROCONST
                db 3,"'","A","'"
cw_ACONST:      db lo(code_ACONST), hi(code_ACONST)
code_ACONST:    pushconst 'A'
                jmp next


name_DASHCONST: linklast ACONST
                db 3,"'","-","'"
cw_DASHCONST:   db lo(code_DASHCONST), hi(code_DASHCONST)
code_DASHCONST: pushconst '-'
                jmp next


name_DQCONST:   linklast DASHCONST
                db 3,"'",'"',"'"
cw_DQCONST:     db lo(code_DQCONST), hi(code_DQCONST)
code_DQCONST:   pushconst '"'
                jmp next


name_COLCONST:  linklast DQCONST
                db 3,"':'"
cw_COLCONST:    db lo(code_COLCONST), hi(code_COLCONST)
code_COLCONST:  pushconst ':'
                jmp next


name_SEMICONST: linklast COLCONST
                db 3,"';'"
cw_SEMICONST:   db lo(code_SEMICONST), hi(code_SEMICONST)
code_SEMICONST: pushconst '"'
                jmp next


name_BL:        linklast SEMICONST
                db 2,'B','L'
cw_BL:          db lo(code_BL), hi(code_BL)
code_BL:        pushconst SPACE
                jmp next

     
name_CR:        db lo(name_BL), hi(name_BL)
                db 2,'C','R'
cw_CR:          db lo(code_CR), hi(code_CR)
code_CR:        mvi a,LF
                call _EMIT
                jmp next


name_SPACE:     db lo(name_CR), hi(name_CR)
                db 5,'S','P','A','C','E'
cw_SPACE:       db lo(code_SPACE), hi(code_SPACE)
code_SPACE:     mvi a, SPACE
                call _EMIT
                jmp next

name_NEGATE:    db lo(name_SPACE), hi(name_SPACE)
                db 6,'N','E','G','A','T','E'
cw_NEGATE:      db lo(DOCOL), hi(DOCOL)
                db lo(cw_LIT), hi(cw_LIT)
                db 0,0
                db lo(cw_SWAP), hi(cw_SWAP)
                db lo(cw_SUB), hi(cw_SUB)
                db lo(EXIT),hi(EXIT)


name_TRUE:      linklast NEGATE
                db 4,'T','R','U','E'
cw_TRUE:        codeword_DOCOL
                cwlit 1
                cw EXIT


name_FALSE:     linklast TRUE
                db 5,'F','A','L','S','E'
cw_FALSE:       codeword_DOCOL
                cwlit 0
                cw EXIT


name_NOT:       linklast FALSE
                db 3,'N','O','T'
cw_NOT:         codeword_DOCOL
                cw ZEQUAL
                cw EXIT


name_LITERAL:   linklast NOT
                db 7|F_IMMED,'L','I','T','E','R','A','L'
                codeword_DOCOL
                cw TICK
                cw LIT
                cw COMMA
                cw COMMA
                cw EXIT

name_COMPILE:   linklast LITERAL
                db 9|F_IMMED,'[','C','O','M','P','I','L','E',']'
cw_COMPILE:     db lo(DOCOL), hi(DOCOL)
                db lo(cw_WORD), hi(cw_WORD)
                db lo(cw_FIND), hi(cw_FIND)
                db lo(cw_TCFA), hi(cw_TCFA)
                db lo(cw_COMMA), hi(cw_COMMA)
                db lo(EXIT), hi(EXIT)

name_RECURSE:   db lo(name_COMPILE), hi(name_COMPILE)
                db 9|F_IMMED,'[','R','E','C','U','R','S','E',']'
cw_RECUSE:      db lo(DOCOL), hi(DOCOL)
                db lo(cw_LATEST), hi(cw_LATEST)
                db lo(cw_FETCH), hi(cw_FETCH)
                db lo(cw_TCFA), hi(cw_TCFA)
                db lo(cw_COMMA), hi(cw_COMMA)
                db lo(EXIT), hi(EXIT)

name_IF:        linklast RECURSE
                db 2|F_IMMED,'I','F'
cw_IF:          codeword_DOCOL
                cw TICK
                cw ZBRANCH
                cw COMMA
                cw HERE
                cw FETCH
                cwlit 0
                cw COMMA
                cw EXIT

name_THEN:      linklast IF
                db 4|F_IMMED,'T','H','E','N'
cw_THEN:        codeword_DOCOL
                cw DUP
                cw HERE
                cw FETCH
                cw SWAP
                cw SUB
                cw SWAP
                cw STORE
                cw EXIT

name_ELSE:      linklast THEN
                db 4|F_IMMED,'E','L','S','E'
cw_ELSE:        codeword_DOCOL
                cw TICK
                cw BRANCH
                cw COMMA
                cw HERE
                cw FETCH
                cwlit 0
                cw COMMA
                cw SWAP
                cw DUP
                cw HERE
                cw FETCH
                cw SWAP
                cw SUB
                cw SWAP
                cw STORE
                cw EXIT

name_BEGIN:     linklast ELSE
                db 5|F_IMMED,'B','E','G','I','N'
cw_BEGIN:       codeword_DOCOL
                cw HERE
                cw FETCH
                cw EXIT

name_UNTIL:     linklast BEGIN
                db 5|F_IMMED,'U','N','T','I','L'
cw_UNTIL:       codeword_DOCOL
                cw TICK
                cw ZBRANCH
                cw COMMA
                cw HERE
                cw FETCH
                cw SUB
                cw COMMA
                cw EXIT

name_AGAIN:     linklast UNTIL
                db 5|F_IMMED,'A','G','A','I','N'
cw_AGAIN:       codeword_DOCOL
                cw TICK
                cw BRANCH
                cw COMMA
                cw HERE
                cw FETCH
                cw SUB
                cw COMMA
                cw EXIT

name_WHILE:     linklast AGAIN
                db 5|F_IMMED,'W','H','I','L','E'
cw_WHILE:       codeword_DOCOL
                cw TICK
                cw ZBRANCH
                cw COMMA
                cw HERE
                cw FETCH
                cwlit 0
                cw COMMA
                cw EXIT

name_REPEAT:    linklast WHILE
                db 6|F_IMMED,'R','E','P','E','A','T'
cw_REPEAT:      codeword_DOCOL
                cw TICK
                cw BRANCH
                cw COMMA
                cw SWAP
                cw HERE
                cw FETCH
                cw SUB
                cw COMMA
                cw DUP
                cw HERE
                cw FETCH
                cw SWAP
                cw SUB
                cw SWAP
                cw STORE
                cw EXIT

name_UNLESS:    linklast REPEAT
                db 5|F_IMMED,'U','N','L','E','S','S'
cw_UNLESS:      codeword_DOCOL
                cw TICK
                cw ZEQUAL
                cw COMMA
                cw COMPILE
                cw IF
                cw EXIT


name_LPAREN:    linklast UNLESS
                db 1|F_IMMED,'('
cw_LPARAM:      codeword_DOCOL
                cwlit 1
                cw KEY
                cw DUP
                cw LPCONST
                cw EQUAL
                cw ZBRANCH
                db 0AH,0
                cw DROP
                cw INCR
                cw BRANCH
                db 0CH,0
                cw RPCONST
                cw EQUAL
                cw ZBRANCH
                db 04H,0
                cw DECR
                cw DUP
                cw ZEQUAL
                cw ZBRANCH
                db 0DCH,0FFH
                cw DROP
                cw EXIT

name_NIP:       linklast LPAREN
                db 3,"NIP"
cw_NIP:         codeword_DOCOL
                cw SWAP
                cw DROP
                cw EXIT


name_TUCK:      linklast NIP
                db 4,"TUCK"
cw_TUCK:        codeword_DOCOL
                cw SWAP
                cw OVER
                cw EXIT

name_PICK:      linklast TUCK
                db 4,"PICK"
cw_PICK:        codeword_DOCOL
                cw INCR
                cwlit 0004
                cw MULT
                cw DSPFETCH
                cw ADD
                cw FETCH
                cw EXIT

name_SPACES:    linklast PICK
                db 6,'S','P','A','C','E','S'
cw_SPACES:      codeword_DOCOL
                cw DUP
                cw ZGT
                cw ZBRANCH
                db 0AH,00H
                cw SPACE
                cw DECR
                cw BRANCH
                dw -0EH
                cw DROP
                cw EXIT

name_DECIMAL:   linklast SPACES
                db 7,'D','E','C','I','M','A','L'
cw_DECIMAL:     codeword_DOCOL
                cwlit 10
                cw BASE
                cw store
                cw EXIT

name_HEX:       linklast DECIMAL
                db 3,'H','E','X'
cw_HEX:         codeword_DOCOL
                cwlit 16
                cw BASE
                cw store
                cw EXIT

name_UDOT0:     linklast HEX
                db 2,'U','.'
cw_UDOT0:       codeword_DOCOL
                cw BASE
                cw FETCH
                cw DIVMOD
                cw QDUP
                cw ZBRANCH
                dw 4H
                cw UDOT0
                cw DUP
                cwlit 0AH
                cw LT
                cw ZBRANCH
                dw 08H
                cw ZEROCONST
                cw BRANCH
                dw 0AH
                cwlit 0AH
                cw SUB
                cw ACONST
                cw ADD
                cw EMIT
                cw EXIT

name_DOTS:      linklast UDOT0
                db 2,'.','S'
cw_DOTS:        codeword_DOCOL
                cw DSPFETCH
                cw DUP
                cw S0
                cw FETCH
                cw LT
                cw ZBRANCH
                dw 10H
                cw DUP
                cw FETCH
                cw UDOT0
                cw SPACE
                cw INCR2
                cw BRANCH
                dw -18H
                cw DROP
                cw EXIT

name_UWIDTH:    linklast DOTS
                db 6,'U','W','I','D','T','H'
cw_UWIDTH:      codeword_DOCOL
                cw BASE
                cw FETCH
                cw DIV
                cw QDUP
                cw ZBRANCH
                dw 0AH
                cw UWIDTH
                cw INCR
                cw BRANCH
                dw 06H
                cwlit 1
                cw EXIT

name_UR:        linklast UWIDTH
                db 3,'U','.','R'
cw_UR:          codeword_DOCOL
                cw SWAP
                cw DUP
                cw UWIDTH
                cw ROT
                cw SWAP
                cw SUB
                cw SPACES
                cw UDOT0
                cw EXIT


name_DOTR:      linklast UR
                db 2,'.','R'
cw_DOTR:        codeword_DOCOL
                cw SWAP
                cw DUP
                cw ZLT
                cw ZBRANCH
                dw 12H
                cw NEGATE
                cwlit 1
                cw SWAP
                cw ROT
                cw DECR
                cw BRANCH
                dw 0AH
                cwlit 0
                cw SWAP
                cw ROT
                cw SWAP
                cw DUP
                cw UWIDTH
                cw ROT
                cw SWAP
                cw SUB
                cw SPACES
                cw SWAP
                cw ZBRANCH
                dw 06H
                cw DASHCONST
                cw EMIT
                cw UDOT0
                cw EXIT

name_DOT:       linklast DOTR
                db 1,'.'
cw_DOT:         codeword_DOCOL
                cwlit 0
                cw DOTR
                cw SPACE
                cw EXIT


name_UDOT:      linklast DOT
                db 2,'U','.'
cw_UDOT:        codeword_DOCOL
                cw UDOT0
                cw SPACE
                cw EXIT

name_QUEST:     linklast UDOT
                db 1,'?'
cw_QUEST:       codeword_DOCOL
                cw FETCH
                cw DOT
                cw EXIT

     
name_WITHIN:    linklast QUEST
                db 6,'W','I','T','H','I','N'
cw_WITHIN:      codeword_DOCOL
                cw NROT
                cw OVER
                cw LTE
                cw ZBRANCH
                dw 14H
                cw GT
                cw ZBRANCH
                dw 08H
                cw TRUE
                cw BRANCH
                dw 04H
                cw FALSE
                cw BRANCH
                dw 06H
                cw 2DROP
                cw FALSE
                cw EXIT


name_DEPTH:     linklast WITHIN
                db 5,'D','E','P','T','H'
cw_DEPTH:       codeword_DOCOL
                cw S0
                cw FETCH
                cw DSPFETCH
                cw SUB
                cw DECR2
                cw EXIT


name_ALIGNED:   linklast DEPTH                          ; we're byte aligned
                db 7,'A','L','I','G','N','E','D'
cw_ALIGNED:     codeword_DOCOL
                cw EXIT


name_ALIGN:     linklast ALIGNED                        ; we're byte aligned
                db 5,'A','L','I','G','N'
cw_ALIGN:       codeword_DOCOL
                cw EXIT


name_CCOMMA:    linklast ALIGN
                db 2,'C',','
cw_CCOMMA:      codeword_DOCOL
                cw HERE
                cw FETCH
                cw STOREBYTE
                cwlit 1
                cw HERE
                cw ADDSTORE
                cw EXIT

name_SSTORE     linklast CCOMMA
                db 2|F_IMMED,'S','"'
cw_SSTORE:      codeword_DOCOL
                cw STATE
                cw FETCH
                cw ZBRANCH
                dw 3CH
                cw TICK
                cw LITSTRING
                cw COMMA
                cw HERE
                cw FETCH
                cwlit 0
                cw COMMA
                cw KEY
                cw DUP
                cw DQCONST
                cw NEQUAL
                cw ZBRANCH
                dw 08H
                cw CCOMMA
                cw BRANCH
                dw -10H
                cw DROP
                cw DUP
                cw HERE
                cw FETCH
                cw SWAP
                cw SUB
                cw DECR2
                cw SWAP
                cw STORE
                cw ALIGN
                cw BRANCH
                dw 2AH
                cw HERE
                cw FETCH
                cw KEY
                cw DUP
                cw DQCONST
                cw NEQUAL
                cw ZBRANCH
                dw 0CH
                cw OVER
                cw STOREBYTE
                cw INCR
                cw BRANCH
                dw -14H
                cw DROP
                cw HERE
                cw FETCH
                cw SUB
                cw HERE
                cw FETCH
                cw SWAP
                cw EXIT

name_SPRINT     linklast SSTORE
                db 2|F_IMMED,'.','"'
cw_SPRINT       codeword_DOCOL
                cw STATE
                cw FETCH
                cw ZBRANCH
                dw 0EH
                cw SSTORE
                cw TICK
                cw TELL
                cw COMMA
                cw BRANCH
                dw 18H
                cw KEY
                cw DUP
                cw DQCONST
                cw EQUAL
                cw ZBRANCH
                dw 06H
                cw DROP
                cw EXIT
                cw EMIT
                cw BRANCH
                dw -14H
                cw EXIT

name_CONSTANT:  linklast SPRINT
                db 8,'C','O','N','S','T','A','N','T'
cw_CONSTANT:    codeword_DOCOL
                cw WORD
                cw CREATE
                cw DOCOL
                cw COMMA
                cw TICK
                cw LIT
                cw COMMA
                cw COMMA
                cw TICK
                cw EXIT
                cw COMMA
                cw EXIT

name_ALLOT:     linklast CONSTANT
                db 5,'A','L','L','O','T'
cw_ALLOT:       codeword_DOCOL
                cw HERE
                cw FETCH
                cw SWAP
                cw HERE
                cw ADDSTORE
                cw EXIT

name_CELLS:     linklast ALLOT
                db 5,'C','E','L','L','S'
cw_CELLS:       codeword_DOCOL
                cwlit 2
                cw MULT
                cw EXIT

name_VARIABLE:  linklast CELLS
                db 8,'V','A','R','I','A','B','L','E'
cw_VARIABLE:    codeword_DOCOL
                cwlit 1
                cw CELLS
                cw ALLOT
                cw WORD
                cw CREATE
                cw DOCOL
                cw COMMA
                cw TICK
                cw LIT
                cw COMMA
                cw COMMA
                cw TICK
                cw EXIT
                cw COMMA
                cw EXIT

name_VALUE:     linklast VARIABLE
                db 5,'V','A','L','U','E'
cw_VALUE:       codeword_DOCOL
                cw WORD
                cw CREATE
                cw DOCOL
                cw COMMA
                cw TICK
                cw LIT
                cw COMMA
                cw COMMA 
                cw TICK
                cw EXIT
                cw COMMA
                cw EXIT

name_TDFA:      linklast VALUE
                db 4,'>','D','F','A'
cw_TDFA:        codeword_DOCOL
                cw TCFA
                cw INCR2
                cw EXIT

name_TO:        linklast TDFA
                db 2|F_IMMED,'T','O'
cw_TO:          codeword_DOCOL
                cw WORD
                cw FIND
                cw TDFA
                cw INCR2
                cw STATE
                cw FETCH
                cw ZBRANCH
                dw 14H
                cw TICK
                cw LIT
                cw COMMA
                cw COMMA
                cw TICK
                cw STORE
                cw COMMA
                cw BRANCH
                dw 4H
                cw STORE
                cw EXIT

name_IDDOT:     linklast TO
                db 3,'I','D','.'
cw_IDDOT:       codeword_DOCOL
                cw INCR2
                cw DUP
                cw FETCHBYTE
                cw F_LENMASK
                cw AND
                cw DUP
                cw ZGT
                cw ZBRANCH
                dw 14H
                cw SWAP
                cw INCR
                cw DUP
                cw FETCHBYTE
                cw EMIT
                cw SWAP
                cw DECR
                cw BRANCH
                dw -18H
                cw 2DROP
                cw EXIT

name_QHIDDEN:   linklast IDDOT
                db 7,'?','H','I','D','D','E','N'
cw_QHIDDEN:     codeword_DOCOL
                cw incr2
                cw FETCHBYTE
                cw F_HIDDEN
                cw AND
                cw EXIT

name_QIMMEDIATE: linklast QHIDDEN
                db 10,"?IMMEDIATE"
cw_QIMMEDIATE:  codeword_DOCOL
                cw INCR2
                cw FETCHBYTE
                cw F_IMMED
                cw AND
                cw EXIT

name_WORDS:     linklast QIMMEDIATE
                db 5,'W','O','R','D','S'
cw_WORDS:       codeword_DOCOL
                cw LATEST
                cw FETCH
                cw QDUP
                cw ZBRANCH
                dw 18H
                cw DUP
                cw QHIDDEN
                cw NOT
                cw ZBRANCH
                dw 8H
                cw DUP
                cw IDDOT
                cw SPACE
                cw FETCH
                cw BRANCH
                dw -1AH
                cw CR
                cw EXIT

name_FORGET:    linklast WORDS
                db 6,'F','O','R','G','E','T'
cw_FORGET:      codeword_DOCOL
                cw WORD
                cw FIND
                cw DUP
                cw FETCH
                cw LATEST
                cw STORE
                cw HERE
                cw STORE
                cw EXIT

name_CASE:      linklast FORGET
                db 4|F_IMMED,"CASE"
cw_CASE:        codeword_DOCOL
                cwlit 0000
                cw EXIT

name_OF:        linklast CASE
                db 2|F_IMMED,"OF"
cw_OF:          codeword_DOCOL
                cw TICK
                cw OVER
                cw COMMA
                cw TICK
                cw EQUAL
                cw COMMA
                cw IF
                cw TICK
                cw DROP
                cw COMMA
                cw EXIT

name_ENDOF:     linklast OF
                db 5|F_IMMED,"ENDOF"
cw_ENDOF:       codeword_DOCOL
                cw ELSE
                cw EXIT

LASTWORD_EXTRA:
name_ENDCASE:   linklast ENDOF
                db 7|F_IMMED,"ENDCASE"
cw_ENDCASE:     codeword_DOCOL
                cw TICK
                cw DROP
                cw COMMA
                cw QDUP
                cw ZBRANCH
                dw 0008H
                cw THEN
                cw BRANCH
                dw -00AH
                cw EXIT

                ifdef notdefined

name_CFAT: linklast ENDCASE
                db 4,"CFA>"
cw_CFAT:        codeword_DOCOL
                cw LATEST
                cw FETCH
                cw QDUP
                cw ZBRANCH
                dw 0016H
                cw 2DUP
                cw SWAP
                cw LT
                cw ZBRANCH
                dw 0006H
                cw NIP
                cw EXIT
                cw FETCH
                cw BRANCH
                dw -018H
                cw DROP
                cwlit 0000
                cw EXIT


name_SEE:       linklast CFAT
                db 3,"SEE"
cw_SEE:         codeword_DOCOL
                cw WORD
                cw FIND
                cw HERE
                cw FETCH
                cw LATEST
                cw FETCH
                cwlit 0002
                cw PICK
                cw OVER
                cw NEQUAL
                cw ZBRANCH
                dw 000CH
                cw NIP
                cw DUP
                cw FETCH
                cw BRANCH
                dw -016H
                cw DROP
                cw SWAP
                cw COLCONST
                cw EMIT
                cw SPACE
                cw DUP
                cw IDDOT
                cw SPACE
                cw DUP
                cw QIMMEDIATE
                cw ZBRANCH
                dw 000EH
                cw SSTORE
                dw 000AH
                db "IMMEDIATE ",'"x'
                cw TELL
                cw TDFA
                cw 2DUP
                cw GT
                cw ZBRANCH
                dw 0136H
                cw DUP
                cw FETCH
                cw TICK
                cw LIT
                cw OVER
                cw EQUAL
                cw ZBRANCH
                dw 0014H
                cw DROP
                cwlit 0002   ; XXX 4
                cw ADD
                cw DUP
                cw FETCH
                cw DOT
                cw BRANCH
                dw 010AH
                cw TICK
                cw LITSTRING
                cw OVER
                cw EQUAL
                cw ZBRANCH
                dw 003CH
                cw DROP
                cwlit 0053
                cw EMIT
                cw DQCONST
                cw EMIT
                cw SPACE
                cwlit 0002  ; XXX 4
                cw ADD
                cw DUP
                cw FETCH
                cw SWAP
                cwlit 0002  ; XXX 4
                cw ADD
                cw SWAP
                cw 2DUP
                cw TELL
                cw DQCONST
                cw EMIT
                cw SPACE
                cw ADD
                cw ALIGNED
                cwlit 0002   ; XXX 4
                cw SUB
                cw BRANCH
                dw 00C4H
                cw TICK
                cw ZBRANCH
                cw OVER
                cw EQUAL
                cw ZBRANCH
                dw 0028H
                cw DROP
                cw SSTORE
                dw 000AH
                db "0BRANCH ( ",'"x'
                cw TELL
                cwlit 0002   ; XXX 4
                cw ADD
                cw DUP
                cw FETCH
                cw DOT
                cw SSTORE
                dw 0002H
                db ") ",'"x'
                cw TELL
                cw BRANCH
                dw 0092H
                cw TICK
                cw BRANCH
                cw OVER
                cw EQUAL
                cw ZBRANCH
                dw 0028H
                cw DROP
                cw SSTORE
                dw 0009H
                db "BRANCH ( ",'"xx'
                cw TELL
                cwlit 0002   ; XXX 4
                cw ADD
                cw DUP
                cw FETCH
                cw DOT
                cw SSTORE
                dw 0002H
                db ") ",'"x'
                cw TELL
                cw BRANCH
                dw 0060H
                cw TICK
                cw TICK
                cw OVER
                cw EQUAL
                cw ZBRANCH
                dw 0020H
                cw DROP
                cwlit 0027
                cw EMIT
                cw SPACE
                cwlit 0002    ; XXX 4
                cw ADD
                cw DUP
                cw FETCH
                cw CFAT
                cw IDDOT
                cw SPACE
                cw BRANCH
                dw 0036H
                cw TICK
                cw EXIT
                cw OVER
                cw EQUAL
                cw ZBRANCH
                dw 0020H
                cw DROP
                cw 2DUP
                cwlit 0002   ; XXX 4
                cw ADD
                cw NEQUAL
                cw ZBRANCH
                dw 000CH
                cw SSTORE
                dw 0005H
                db "EXIT ",'"xx'
                cw TELL
                cw BRANCH
                dw 000CH
                cw DUP
                cw CFAT
                cw IDDOT
                cw SPACE
                cw DROP
                cwlit 0002   ; XXX 4
                cw ADD
                cw BRANCH
                dw -13AH
                cw SEMICONST
                cw EMIT
                cw CR
                cw 2DROP
                cw EXIT

                endif