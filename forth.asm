            PAGE 0             ; suppress page headings in ASW listing file
            
;-----------------------------------------------------------------------------
; 8008-forth
; Scott Baker, http://www.smbaker.com/
;
; A straightforward rewrite of Jonesforth for the 8008 CPU.
;
; Notes
;
; 1. I thought I would be clever and try to use D and E as my datastack 
;    and returnstack pointers. This turned out to be not so clever as I
;    probably wasted more time preserving these registers and working with
;    a smaller set of registers than it would have taken to simply put
;    these pointers in memory variables.
;
; 2. The second argument for multiplication and division must be 8-bits.
;    For example "1000 250 /" is fine and "1000 250 *" is fine, but
;    "1000 256 /" or "1000 256 *' are a no-no. It's an exercise for the
;    reader to do better...
;
; Memory map
;    0000-00FF  page0 - reserved for bootstrap code, vectors, etc
;    0100-01FF  page1 - variables and the return stack
;    0200-02FF  page2 - data stack
;    0300-03FF  page3 - line input buffer, for cooked mode
;    0400 1FFF        - user program area
;    2000-3FFF        - forth kernel, standard words, extended words, etc. (ROM)
;
;    It's assumed that 0000-1FFF is RAM and 2000-3FFF is ROM.
;-----------------------------------------------------------------------------

            include "bitfuncs.inc" 

            cpu 8008new             ; use "new" 8008 mnemonics

forthversion    equ     0100H

rstackpage      equ     01H
cur             equ     00H             ; CUR holds the next element in the body to execute
e_temp2         equ     02H
jmpa_instr      equ     03H
jmpa_addr       equ     04H
islit           equ     06H
numerr          equ     08H
neg             equ     0AH
a_temp          equ     0CH
b_temp          equ     0DH
c_temp          equ     0EH
d_temp          equ     0FH
e_temp          equ     10H
l_temp          equ     11H
value           equ     12H
state           equ     14H
here            equ     16H
latest          equ     18H
s0              equ     1AH
base            equ     1CH
matchptr        equ     1EH
testptr         equ     20H
callee          equ     22H             ; Callee points to the codeword of a subroutine to execute
temp1           equ     24H
temp2           equ     26H
temp3           equ     28H
temp4           equ     2AH
bufhead         equ     2CH
buftail         equ     2DH
bufrdy          equ     2EH
fill3           equ     2FH
wordbuf         equ     30H
wordbuf_end     equ     4FH

rstack_top      equ     0FEH            ; It's easiest to just burn one cell

dstackpage      equ     02H
dstack_top      equ     0FEH            ; It's easiest to just burn one cell

bufpage         equ     03H             ; line input buffer

codepage        equ     04H             ; first page containing code

jmpa_jump       equ     (rstackpage*100H)+jmpa_instr
wordbuf_addr    equ     (rstackpage*100H)+wordbuf
state_addr      equ     (rstackpage*100H)+state
here_addr       equ     (rstackpage*100H)+here
latest_addr     equ     (rstackpage*100H)+latest
s0_addr         equ     (rstackpage*100H)+s0
basE_addr       equ     (rstackpage*100H)+base

rstack_top_addr equ     (rstackpage*100H)+rstack_top

F_IMMED         equ     080H
F_HIDDEN        equ     020H
F_LENMASK       equ     01FH

BS              equ     08H
CR              equ     0DH
LF              equ     0AH
NEWLINE         equ     LF
SPACE           equ     20H
LINETERM        equ     CR

;------------------------------------------------------------------------
; useful macros
;------------------------------------------------------------------------

rstackptr_get   macro
                mov l,e
                endm

rstackptr_put   macro
                mov e,l
                endm

dstackptr_get   macro
                mov l,d
                endm

dstackptr_put   macro
                mov d,l
                endm

inr_hl          macro
                inr l
                jnz nowrap
                inr h
nowrap:
                endm

dcr_hl          macro
                inr l                   ; we need to test L to see if it is zero.
                dcr l                   ; inc followed by dec should work.
                jnz nowrap
                dcr h                   ; we wrapped
nowrap:         dcr l                   ; finally, decrement l
                endm

dcr_ba          macro
                ora a
                jnz nowrap
                dcr b                   ; we wrapped
nowrap:         dcr a                   ; finally, decrement l
                endm

pushconst       macro v
                mvi h,dstackpage
                dstackptr_get           ; dstack pointer into L
                dcr l
                mvi m,hi(v)             ; store MSB
                dcr l
                mvi m,lo(v)             ; store LSB
                dstackptr_put           ; update dstack pointer
                endm

peekab          macro                   ; a=lsb, b=msb
                mvi h,dstackpage
                dstackptr_get
                mov a,m                 ; get LSB
                inr l
                mov b,m                 ; get MSB
                endm                

popab           macro                   ; a=lsb, b=msb
                mvi h,dstackpage
                dstackptr_get
                mov a,m                 ; get LSB
                inr l
                mov b,m                 ; get MSB
                inr l
                dstackptr_put
                endm

pophl           macro
                popab                   ; could optimize...
                mov h,b
                mov l,a
                endm                

pushab          macro                   ; a=lsb, b=msb
                mvi h,dstackpage
                dstackptr_get
                dcr l
                mov m,b                 ; store MSB
                dcr l
                mov m,a                 ; store LSB
                dstackptr_put
                endm

push_0c         macro                   ; a=lsb, b=msb
                mvi h,dstackpage
                dstackptr_get
                dcr l
                mvi m,0                 ; store MSB
                dcr l
                mov m,c                 ; store LSB
                dstackptr_put
                endm

pushvar         macro addr
                mvi h,hi(addr)
                mvi l,lo(addr)
                mov a,m                 ; LSB into A
                inr l
                mov b,m                 ; MSB into B
                pushab
                endm

rspopab         macro                   ; a=lsb, b=msb
                mvi h,rstackpage
                rstackptr_get
                mov a,m                 ; get LSB
                inr l
                mov b,m                 ; get MSB
                inr l
                rstackptr_put
                endm

rspushab        macro                   ; a=lsb, b=msb
                mvi h,rstackpage
                rstackptr_get
                dcr l
                mov m,b                 ; store MSB
                dcr l
                mov m,a                 ; store LSB
                rstackptr_put
                endm

jmp_hl_indir    macro
                m_to_ba                 ; Get address in (HL)
                mvi h,rstackpage
                mvi l,jmpa_addr
                mov m,a
                inr l
                mov m,b
                jmp jmpa_jump           ; jump to it
                endm

islit_check     macro
                mvi h,rstackpage
                mvi l,islit
                mov a,m
                ora a
                endm

islit_set       macro
                mvi h,rstackpage
                mvi l,islit
                mvi m,1
                endm

islit_clear     macro
                mvi h,rstackpage
                mvi l,islit
                mvi m,0
                endm

state_check     macro
                mvi h,rstackpage
                mvi l,state
                mov a,m
                ora a
                endm

state_set       macro
                mvi h,rstackpage
                mvi l,state
                mvi m,1
                endm

state_clear     macro
                mvi h,rstackpage
                mvi l,state
                mvi m,0
                endm

neg_check     macro
                mvi h,rstackpage
                mvi l,neg
                mov a,m
                ora a
                endm

neg_set         macro
                mvi h,rstackpage
                mvi l,neg
                mvi m,1
                endm

neg_clear       macro
                mvi h,rstackpage
                mvi l,neg
                mvi m,0
                endm

consume_de      macro
                mov h,d
                mov l,e
                mov a,m
                inr e
                jnz nowrap
                inr d
nowrap:
                endm

swap_dehl       macro
                mov b,h
                mov h,d
                mov d,b
                mov b,l
                mov l,e
                mov e,b
                endm

uni_dehl        macro
                mov h,d
                mov l,e
                endm

uni_hlde        macro
                mov d,h
                mov e,l
                endm

consume_de_safe macro
                swap_dehl
                consume_hl
                swap_dehl
                endm


peek_de         macro
                mov h,d
                mov l,e
                mov a,m
                endm

consume_hl      macro
                mov a,m
                inr_hl
                endm

m_to_hl:        macro
                mov a,m
                inr_hl
                mov h,m
                mov l,a                 ; HL = (m)
                endm

m_to_ba:        macro
                mov a,m
                inr_hl
                mov b,m
                inr_hl                  ; usually unnecessary, but just to be safe and preseve semantics
                endm

ba_to_m:        macro
                mov m,a
                inr_hl
                mov m,b
                inr_hl                  ; usually unnecessary, but just to be safe and preseve semantics
                endm


e_save          macro
                mvi h,rstackpage
                mvi l,e_temp
                mov m,e
                endm

e_restore       macro
                mvi h,rstackpage
                mvi l,e_temp
                mov e,m
                endm

e_save2         macro
                mvi h,rstackpage
                mvi l,e_temp2
                mov m,e
                endm

e_restore2      macro
                mvi h,rstackpage
                mvi l,e_temp2
                mov e,m
                endm

de_save         macro
                mvi h,rstackpage
                mvi l,d_temp
                mov m,d
                inr l
                mov m,e
                endm

de_restore      macro
                mvi h,rstackpage
                mvi l,d_temp
                mov d,m
                inr l
                mov e,m
                endm

a_save          macro
                mvi h,rstackpage
                mvi l,a_temp
                mov m,a
                endm

a_restore       macro
                mvi h,rstackpage
                mvi l,a_temp
                mov a,m
                endm

c_save          macro
                mvi h,rstackpage
                mvi l,c_temp
                mov m,c
                endm

c_restore       macro
                mvi h,rstackpage
                mvi l,c_temp
                mov c,m
                endm

double_inc_hl   macro
                inr_hl
                inr_hl
                endm

                ;; double_inc_m: double-increment the 16-bit value at (m), then jump to `donelabel`

double_inc_m    macro donelabel
                mov a,m                 ; get LSB

                adi 1                   ; increment LSB by 1
                jz nextwrap1
                adi 1                   ; increment LSB by 1
                jz nextwrap2
                mov m,a                 ; store the double-incremented LSB
                jmp donelabel

nextwrap1:      adi 1                   ; we still need to do our second increment of LSB
nextwrap2:      mov m,a                 ; store the double-incremented LSB
                inr_hl
                mov b,m
                inr b                   ; increment MSB
                mov m,b
                jmp donelabel
                endm

double_inc_cur  macro donelabel
                mvi h,rstackpage
                mvi l,cur
                double_inc_m donelabel
                endm

variable_check  macro variable
                mvi h,rstackpage
                mvi l,variable
                xra a
                ora m
                endm

                ;; value_ab: read the contents of `value` into AB

variable_ab     macro variable
                mvi h,rstackpage
                mvi l,variable
                mov a,m
                inr l
                mov b,m
                endm

ab_variable     macro variable
                mvi h,rstackpage
                mvi l,variable
                mov m,a
                inr l
                mov m,b
                endm

a_variable      macro variable
                mvi h,rstackpage
                mvi l,variable
                mov m,a
                inr l
                mvi m,0
                endm

variable_c      macro variable
                mvi h,rstackpage
                mvi l,variable
                mov c,m
                endm

value_ab        macro
                variable_ab value
                endm

ab_value        macro
                ab_varioble value
                endm

variable_hl     macro var
                mvi h,rstackpage
                mvi l,var
                mov a,m
                inr l
                mov h,m
                mov l,a
                endm

variable_de     macro variable
                mvi h,rstackpage
                mvi l,variable
                mov e,m
                inr l
                mov d,m
                endm

de_variable     macro variable
                mvi h,rstackpage
                mvi l,variable
                mov m,e
                inr l
                mov m,d
                endm

symbol_variable macro symbol,variable
                mvi h,rstackpage
                mvi l,variable
                mvi m,lo(symbol)
                inr l
                mvi m,hi(symbol)
                endm

value_complement macro
                mvi h,rstackpage
                mvi l,value
                mov a,m                 ; flip all bits in LSB
                xri 0FFH
                mov m,a
                inr l
                mov a,m                 ; flip all bits in MSB
                xri 0FFH
                mov m,a
                dcr l
                mov a,m                 ; add 1 to LSB
                adi 1
                mov m,a
                inr l
                mov a,m                 ; add carry to MSB
                aci 0
                mov m,a
                endm


                ;; value_add_a: adds register `a` to `value`
                ;; destroys the contents of `a`

value_add_a     macro
                mvi h,rstackpage
                mvi l,value
                add m
                mov m,a
                inr l
                mov a,m
                aci 0
                mov m,a
                endm


linklast        macro prevword
                db lo(name_prevword), hi(name_prevword)
                endm

codeword_DOCOL  macro x
                db lo(DOCOL), hi(DOCOL)
                endm

cw              macro x
                db lo(cw_x), hi(cw_x)
                endm

cwlit           macro x
                db lo(cw_LIT), hi(cw_LIT)
                db lo(x), hi(x)
                endm

; Input:  B = dividend, C = divisor, L = carry
; Output: B = dividend, L = carry
div8            macro           
                rept    8
                mov a,b     ; A <- B
                add a       ; asl (A = A + A)
                mov b,a     ; B <- A, dividend lo
                mov a,l     ; A <- L, dividend hi
                ral         ; rol A (hi)
                jc m0       ; 9th bit hi?  Sub it.
                cmp c       ; A - C, if A < C set carry
                jc m1       ; too small
m0:             sub c       ; A = A - C
                inr b       ; B++
m1:             mov l,a     ; L <- A
                endm
                endm

; Input BA = dividend, C = divisor
; Output BA = quotient, C = remainder
; Destroys H,L

div16_8         macro
                mov h,a         ; store LSB into H
                mvi l,0         ; initialize carry to 0
                div8            ; divide B by C
                mov a,b         ; swap B and H
                mov b,h
                mov h,a
                div8
                mov a,b         ; LSB into A
                mov b,h         ; MSB into B
                mov c,l         ; remainder into C
                endm

; Input BA = multiplicand, C = multiplier
; Output BA = product
; Destroys C,E

mul16_8         macro
                mvi h,0         ; HL is partial product
                mvi l,0
                mov e,a         ; BE is multiplicand

                rept 8
                mov a,c         ; Rotate multiplier right into carry
                rar
                mov c,a
                jnc m0          ; LSB in multiplicand was not set

                ;; add multiplicand to partial product

                mov a,l         ; Add multiplicand LSB
                add e
                mov l,a
                mov a,h         ; Add multiplicand MSB
                adc b
                mov h,a

m0:             ;; shift multiplicand to the left

                ora a           ; clear carry
                mov a,e         ; rotate LSB left into carry
                ral
                mov e,a         ; save it back to E
                mov a,b         ; now rotate carry into LSB
                ral
                mov b,a
                endm
                mov b,h         ; product into BA
                mov a,l
                endm

;------------------------------------------------------------------------
; entry point
;------------------------------------------------------------------------

                ifdef serscel

                ;; serscel is bitbang serial for the SIMH Scelbi
                ;; simulator. We set the origin to address 0H and we
                ;; install bootstrap code (rst 1) and an RST1 handler
                ;; at 0x08H that jumps us up to rom_start.

                org 0h
                rst 1

                org 08H
                jmp rom_start

                else

                ;; H8 CPU Board
                ;; Code starts at address 2000H. My CPU Board will
                ;; automatically map 2000H to address 0H on startup so
                ;; that the RST1 is executed. The code in go_rom0 will
                ;; undo the mapping, convert 0000-1FFF into RAM.

                org 2000h                   ; beginning of EPROM
                rst 1

                org 2008h	            ; rst 1 jumps here
                jmp go_rom0        

                include "lib/go-rom.inc"

                endif ; serscel

                org 2100H
            
rom_start:      call SINIT              ; Initialize serial port
                mvi h,hi(titletxt)
                mvi l,lo(titletxt)
                call puts
                jmp init                ; jump to FORTH initialization

;------------------------------------------------------------------------
; next - increment program pointer and jump to next;
;
; 1. CUR holds the current element in our body. Dereference it to get
;    the address of the callee's codeword, and store that in CALLEE.
;
; 2. Dereference the callee's codeword to get the address to jump to
;
; 3. Setup the jump pointer for jmpa_jump
;
; 4. Move CUR to point to the next codeword in our body
;
; 5. Jump
;
;------------------------------------------------------------------------

next:           ;; first setup the indirect jump
                
                mvi h,rstackpage
                mvi l,cur
                mov a,m                 ; A has LSB of cur
                inr l
                mov h,m                 ; H has MSB of cur
                mov l,a                 ; HL now has cur

                m_to_ba                 ; BA = address of codeword        

                mvi h,rstackpage
                mvi l,callee            ; store callee's coreword to CALLEE
                mov m,a
                inr l
                mov m,b

                mov l,a                 ; HL = Address of codeword
                mov h,b
                m_to_ba                 ; Dereference the codeword and put into BA ... Jonesforth *(eax)

                mvi h,rstackpage
                mvi l,jmpa_addr
                mov m,a                 ; LSB of (cur) into jmpaddr_lo
                inr l
                mov m,b                 ; MSB of (cur) into jmpaddr_hi

                mvi l,cur               ; now do the increment part of the LODSL
                double_inc_m jmpa_jump  ; inc CUR +2 and jump to jmpa_jmp

;------------------------------------------------------------------------
; docol - the interpreter
;
; 1. CUR holds the next element in the Caller's body. Push it to the
;    return stack.
;
; 2. Load our codeword address from CALLEE
;
; 3. Increment by 2 to skip the codeword and point to our body
;
; 4. Store it back to CUR. CUR now points to the first element in the
;    callee's body.
;
; 5. Call NEXT
;
;------------------------------------------------------------------------

docol:          mvi h,rstackpage
                mvi l,cur
                mov c,m                 ; C has LSB of cur
                inr l
                mov b,m                 ; B has MSB of cur

                ;; decrement rstackptr by 2 and store cur

                rstackptr_get
                dcr l
                mov m,b                 ; MSB into rstackptr-1
                dcr l
                mov m,C                 ; LSB into rstackptr-2
                rstackptr_put

                mvi l,callee
                mov a,m
                inr l
                mov b,m

                mvi l,cur               ; get ready to store to cur. H is still rstackpage

                adi 1H
                jz docolwrap1
                adi 1H
                jz docolwrap2
                mov m,a                 ; store the double-incremented cur-LSB
                inr l
                mov m,b                 ; and store the MSB too, as it may have changed
                jmp next

docolwrap1:     adi 1H                  ; we still need to do our second increment of cur-LSB
docolwrap2:     mov m,a                 ; store the double-incremented cur-LSB
                inr l                   ; point to cur-MSB
                inr b                   ; increment cur-MSB
                mov m,b
                jmp next


;------------------------------------------------------------------------
; init
;------------------------------------------------------------------------

init:           mvi h,rstackpage
                mvi l,jmpa_instr
                mvi a,044H
                mov m,a                 ; store jump instruction into jmpa_instr

                mvi l,state             ; initialize 'state'
                mvi m,0
                inr l
                mvi m,0

                mvi l,here              ; initialize 'here'
                mvi m,0
                inr l
                mvi m,codepage

                mvi l,latest            ; initialize 'latest'
                mvi m,lo(LASTWORD)
                inr l
                mvi m,hi(LASTWORD)

                mvi l,base              ; initialize 'base'
                mvi m,0AH
                inr l
                mvi m,0

                mvi l,cur               ; initialize cur to point to cold_start
                mvi m,lo(cold_start)
                inr l
                mvi m,hi(cold_start)

                mvi l,s0                ; set datastack top pointer
                mvi m,dstack_top
                inr l
                mvi m,dstackpage

                mvi d,dstack_top        ; set initial data stack pointer
                mvi e,rstack_top        ; set initial return stack pointer

                call buf_init           ; initialize input buffer

                jmp next                ; start
cold_start:     db lo(cw_QUIT), hi(cw_QUIT)

fault_token:    mvi h,hi(token_fault_txt)
                mvi l,lo(token_fault_txt)
                call puts
                mvi h,rstackpage
                mvi l,wordbuf           ; we null terminated it
                call puts
                call write_crlf
                jmp fault

fault:          call buf_reset          ; discard remaining input
                jmp code_INTERPRET      ; restart the interpreter

multfunc:       mul16_8
                ret

;------------------------------------------------------------------------
; constants
;------------------------------------------------------------------------

name_VERSION:   db 0, 0
                db 7,'V','E','R','S','I','O','N'
cw_VERSION:     db lo(code_VERSION), hi(code_VERSION)
code_VERSION:   pushconst forthversion
                jmp next


name_RZ:        db lo(name_VERSION), hi(name_VERSION)
                db 2,'R','0'
cw_RZ:          db lo(code_RZ), hi(code_RZ)
code_RZ:        pushconst rstack_top_addr
                jmp next


name_DOCOL:     db lo(name_RZ), hi(name_RZ)                 ; Note: this puts the constant for the address of DOCOL
                db 5,'D','O','C','O','L'                    ; onto the stack. it does not run docol.
cw_DOCOL:       db lo(code_DOCOL), hi(code_DOCOL)
code_DOCOL:     pushconst DOCOL
                jmp next
                ;db lo(DOCOL), hi(DOCOL)
                ;cw EXIT


name_F_IMMED:   db lo(name_DOCOL), hi(name_DOCOL)
                db 7,'F','_','I','M','M','E','D'
cw_F_IMMED:     db lo(code_F_IMMED), hi(code_F_IMMED)
code_F_IMMED:   pushconst F_IMMED
                jmp next


name_F_HIDDEN:  db lo(name_F_IMMED), hi(name_F_IMMED)
                db 8,'F','_','H','I','D','D','E','N'
cw_F_HIDDEN:    db lo(code_F_HIDDEN), hi(code_F_HIDDEN)
code_F_HIDDEN:  pushconst F_HIDDEN
                jmp next


name_F_LENMASK: db lo(name_F_HIDDEN), hi(name_F_HIDDEN)
                db 9,'F','_','L','E','N','M','A','S','K'
cw_F_LENMASK:   db lo(code_F_LENMASK), hi(code_F_LENMASK)
code_F_LENMASK: pushconst F_LENMASK
                jmp next

;------------------------------------------------------------------------
; return stack
;------------------------------------------------------------------------

name_TOR:       db lo(name_F_LENMASK), hi(name_F_LENMASK)
                db 2,'>','R'
cw_TOR:         db lo(code_TOR), hi(code_TOR)
code_TOR:       popab
                rspushab
                jmp next


name_FROMR:     db lo(name_TOR), hi(name_TOR)
                db 2,'R','>'
cw_FROMR:       db lo(code_FROMR), hi(code_FROMR)
code_FROMR:     rspopab
                pushab
                jmp next


name_RSPFETCH:  db lo(name_FROMR), hi(name_FROMR)
                db 4,'R','S','P','@'
cw_RSPFETCH:    db lo(code_RSPFETCH), hi(code_RSPFETCH)
code_RSPFETCH:  mvi b,rstackpage
                mov a,e
                pushab
                jmp next


name_RSPSTORE:  db lo(name_RSPFETCH), hi(name_RSPFETCH)
                db 4,'R','S','P','!'
cw_RSPSTORE:    db lo(code_RSPSTORE), hi(code_RSPSTORE)
code_RSPSTORE:  popab
                mov e,a
                jmp next

;------------------------------------------------------------------------
; data stack
;------------------------------------------------------------------------

name_DSPFETCH:  db lo(name_RSPSTORE), hi(name_RSPSTORE)
                db 4,'D','S','P','@'
cw_DSPFETCH:    db lo(code_DSPFETCH), hi(code_DSPFETCH)
code_DSPFETCH:  mvi b,dstackpage
                mov a,d
                pushab
                jmp next


name_DSPSTORE:  db lo(name_DSPFETCH), hi(name_DSPFETCH)
                db 4,'D','S','P','!'
cw_DSPSTORE:    db lo(code_DSPSTORE), hi(code_DSPSTORE)
code_DSPSTORE:  popab
                mov d,a
                jmp next

;------------------------------------------------------------------------
; IO
;------------------------------------------------------------------------

name_KEY:       db lo(name_DSPSTORE),hi(name_DSPSTORE)
                db 3,'K','E','Y'
cw_KEY:         db lo(code_KEY),hi(code_KEY)
code_KEY:       call _KEY
                mvi b,0
                pushab
                jmp next
_KEY:           jmp buf_read


name_NUMBER:    db lo(name_KEY),hi(name_KEY)
                db 6,'N','U','M','B','E','R'
cw_NUMBER:      db lo(code_NUMBER),hi(code_NUMBER)
code_NUMBER:    popab
                mov c,a
                popab
                call _NUMBER
                mvi h,rstackpage        ; get value from rstackpage:value
                mvi l,value
                mov a,m
                inr l
                mov b,m
                pushab                  ; push the value
                mvi a,0
                mvi b,0
                pushab                  ; number of unparsed characters, 0 = noerror
                jmp next

                ;; on entry BA is address of buffer, C is count
                ;; returns value in BA

_NUMBER:        de_save

                mov e,a
                mov d,b                 ; word buffer will be in DE

                mvi h,rstackpage        ; start value at 0
                mvi l,value
                mvi a,0
                mov m,a
                inr l
                mov m,a

                neg_clear               ; clear negative flag

                mov a,c                 ; length into a
                ora a
                jz _NUMBER_5            ; zero-length string?

                consume_de              ; consume one character
                cpi '-'                 ; is it '-' ?
                jnz _NUMBER_2
                neg_set
                dcr c                   ; more characters ?
                jnz _NUMBER_1
                jmp _NUMBER_5           ; must be '-' with no digits

_NUMBER_1:      e_save2
                c_save
                variable_ab value
                variable_c base
                call multfunc
                ab_variable value
                c_restore
                e_restore2
                consume_de

_NUMBER_2:      sui '0'
                jc _NUMBER_4            ; less than zero
                cpi 0AH
                jc _NUMBER_3            ; between '0' and '9'
                sui 11H
                jc _NUMBER_4            ; between '9' and 'A'
                adi 0AH                 ; add 10, since it must have been >= 'A'
                cpi 2AH                 ; in the uppercase range?
                jm _NUMBER_NOTLOW       ; Nope.
                sui 20H                 ; uppercase letter?
_NUMBER_NOTLOW:

_NUMBER_3:      mvi h,rstackpage
                mvi l,base
                cmp m
                jnc _NUMBER_4

                value_add_a

                dcr c
                jnz _NUMBER_1

_NUMBER_4:      xra a
                ora c
                a_variable numerr
                neg_check
                jz _NUMBER_5
                value_complement

_NUMBER_5:      de_restore
                ret

name_FIND:      db lo(name_NUMBER),hi(name_NUMBER)
                db 4,'F','I','N','D'
cw_FIND:        db lo(code_FIND),hi(code_FIND)
code_FIND:      popab
                mov c,a
                popab
                call _FIND
                mov a,l
                mov b,h
                pushab
                jmp next

_FIND:          de_save
                c_save
                mvi h,rstackpage        ; store match buffer in rstackpage:matchptr
                mvi l,matchptr
                mov m,a
                inr l
                mov m,b

                mvi l,latest            ; store latest in rstackpage:testptr
                mov a,m
                inr l
                mov b,m
                mvi l,testptr
                mov m,a
                inr l
                mov m,b

_FIND1:         c_restore               ; C = counter
                mvi h,rstackpage
                mvi l,matchptr
                mov e,m
                inr l
                mov d,m                 ; DE = matchptr

                mvi h,rstackpage
                mvi l,testptr
                mov a,m
                inr l
                mov h,m
                mov l,a                 ; HL = testptr
                ora h
                jz _FIND4               ; H=L=0, we're at the end of the road

                consume_hl              ; skip the link
                consume_hl              ; ... it's two bytes long
                consume_hl              ; get the length byte
                ani (F_HIDDEN|F_LENMASK)
                cmp c
                jnz _FIND2              ; length does not match

_FIND_CLOOP:    consume_de_safe
                mov b,a
                consume_hl              ; XXX could use a cmp m here XXX optimize me XXX
                cmp b
                jz _FIND_CASEMATCH      ; we matched with case sensitive
                xri 20H                 ; flip the case bit
                cmp b
                jnz _FIND2              ; failed to match even after inverting case
_FIND_CASEMATCH:
                dcr c
                jnz _FIND_CLOOP

                mvi h,rstackpage
                mvi l,testptr
                m_to_ba                 ; BA = testptr
                de_restore
                mov h,b
                mov l,a                 ; HL = testptr
                ret

_FIND2:         mvi h,rstackpage
                mvi l,testptr

                m_to_hl                 ; HL = (m)
                m_to_ba                 ; BA = ((m))
                ab_variable testptr     ; store next word in testptr
                jmp _FIND1

_FIND4:         de_restore
                mvi h,0                 ; we failed
                mvi l,0                 ; these should already be 0, but just to be sure...
                ret


name_TCFA:      db lo(name_FIND),hi(name_FIND)
                db 4,'>','C','F','A'
cw_TCFA:        db lo(code_TCFA),hi(code_TCFA)
code_TCFA:      popab
                call _TCFA
                pushab
                jmp next

_TCFA:          mov l,a
                mov h,b                 ; HL = word pointer

                double_inc_hl           ; skip over the link
_TCFA_ATLEN:                            ; expects flag byte at HL
                mov a,m                 ; get the flag/length byte
                ani F_LENMASK           ; we only want the length
                adi 1                   ; add 1 to include the length
                add l                   ; A = L + length + 1
                jnc _TCFA_NOWRAP1
                mov b,h                 ; B = MSB
                inr b                   ; we wrapped
                ret
_TCFA_NOWRAP1:                          ; BA = pointer to codeword
                mov b,h                 ; B = MSB
                ret


name_EMIT:      db lo(name_TCFA),hi(name_TCFA)
                db 4,'E','M','I','T'
cw_EMIT:        db lo(code_EMIT),hi(code_EMIT)
code_EMIT:      popab
                mvi b,0
                call _EMIT
                jmp next
_EMIT:          cpi LF
                jnz CPRINT
                mvi a,CR
                call CPRINT
                mvi a,LF
                jmp CPRINT


name_WORD:      db lo(name_EMIT),hi(name_EMIT)
                db 4,'W','O','R','D'
cw_WORD:        db lo(code_WORD),hi(code_WORD)
code_WORD:      call _WORD              ; wordbuf=content, c=length
                pushconst wordbuf_addr
                mov a,c                 ; get length from C to A
                mvi b,0
                pushab
                jmp next
_WORD:                                  ; Returns data in wordbuf, count in c
_WORD1:         call _KEY
                cpi '\\'
                jz _WORD3
                cpi ' '
                jz _WORD                ; it's a space
                jc _WORD                ; It's below space? (x86 instruction was JBE)
                mvi h,rstackpage
                mvi l,wordbuf
                mvi c,0                 ; length counter
_WORD2:         mov m,a
                inr l                   ; increment dest pointer
                inr c                   ; increment length counter
                call _KEY
                cpi ' '
                jc  _WORD2B             ; this sequence of 3 jumps as a JA _WORD2
                jz  _WORD2B
                jmp _WORD2
_WORD2B:        mvi m,0H                ; null terminate the wordbuf, makes life easier for everyone
                ret
_WORD3:         call _KEY               ; For comments, eat everything until CR or LF
                cpi CR
                jz _WORD1
                cpi LF
                jz _WORD1
                jmp _WORD3

                
;------------------------------------------------------------------------
; misc words
;------------------------------------------------------------------------

name_DROP:      db lo(name_WORD),hi(name_WORD)
                db 4,'D','R','O','P'
cw_DROP:        db lo(code_DROP),hi(code_DROP)
code_DROP:      popab
                jmp next


name_SWAP:      db lo(name_DROP),hi(name_DROP)
                db 4,'S','W','A','P'
cw_SWAP:        db lo(code_SWAP),hi(code_SWAP)
code_SWAP:      popab
                ab_variable temp1
                popab
                ab_variable temp2
                variable_ab temp1
                pushab
                variable_ab temp2
                pushab
                jmp next


name_DUP:       db lo(name_SWAP),hi(name_SWAP)
                db 3,'D','U','P'
cw_DUP:         db lo(code_DUP),hi(code_DUP)
code_DUP:       popab                   ; XXX optimize
                pushab
                pushab
                jmp next


name_OVER:      db lo(name_DUP),hi(name_DUP)
                db 4,'O','V','E','R'
cw_OVER:        db lo(code_OVER),hi(code_OVER)
code_OVER:      mvi h,dstackpage
                dstackptr_get
                inr l                   ; skip the top element
                inr l                   ; ... MSB
                mov a,m                 ; get the next element LSB
                inr l                   ; ... MSB
                mov b,m
                pushab
                jmp next


name_ROT:       db lo(name_OVER),hi(name_OVER)
                db  3,'R','O','T'
cw_ROT:         db lo(code_ROT),hi(code_ROT)
code_ROT:       popab
                ab_variable temp1
                popab
                ab_variable temp2
                popab
                ab_variable temp3
                variable_ab temp2
                pushab
                variable_ab temp1
                pushab
                variable_ab temp3
                pushab
                jmp next


name_NROT       db lo(name_ROT),hi(name_ROT)
                db  4,'-','R','O','T'
cw_NROT:        db lo(code_NROT),hi(code_NROT)
code_NROT       popab
                ab_variable temp1
                popab
                ab_variable temp2
                popab
                ab_variable temp3
                variable_ab temp1
                pushab
                variable_ab temp3
                pushab
                variable_ab temp2
                pushab
                jmp next


name_2DROP:     db lo(name_NROT),hi(name_NROT)
                db 5,'2','D','R','O','P'
cw_2DROP:       db lo(code_2DROP),hi(code_2DROP)
code_2DROP:     inr d
                inr d
                inr d
                inr d
                jmp next


name_2DUP:      db lo(name_2DROP),hi(name_2DROP)
                db 4,'2','D','U','P'
cw_2DUP:        db lo(code_2DUP),hi(code_2DUP)
code_2DUP:      de_save
                popab
                ab_variable temp1
                popab
                ab_variable temp2
                de_restore
                variable_ab temp2
                pushab
                variable_ab temp1
                pushab
                jmp next


name_2SWAP:     db lo(name_2DUP),hi(name_2DUP)
                db 5,'2','S','W','A','P'
cw_2SWAP:       db lo(code_2SWAP),hi(code_2SWAP)
code_2SWAP:     popab
                ab_variable temp1
                popab
                ab_variable temp2
                popab
                ab_variable temp3
                popab
                ab_variable temp4
                variable_ab temp2
                pushab
                variable_ab temp1
                pushab
                variable_ab temp4
                pushab
                variable_ab temp3
                pushab
                jmp next


name_QDUP:      db lo(name_2SWAP),hi(name_2SWAP)
                db 4,'?','D','U','P'
cw_QDUP:        db lo(code_QDUP),hi(code_QDUP)
code_QDUP:      peekab
                mov c,a
                ora b
                jz _QDUP1
                mov a,c
                pushab
_QDUP1:         jmp next


name_INCR:      db lo(name_QDUP),hi(name_QDUP)
                db 2,'1','+'
cw_INCR:        db lo(code_INCR),hi(code_INCR)
code_INCR:      popab
                adi 1
                mov c,a
                mov a,b
                aci 0
                mov b,a
                mov a,c
                pushab
                jmp next


name_DECR:      db lo(name_INCR),hi(name_INCR)
                db 2,'1','-'
cw_DECR:        db lo(code_DECR),hi(code_DECR)
code_DECR:      popab
                sui 1
                mov c,a
                mov a,b
                sbi 0
                mov b,a
                mov a,c
                pushab
                jmp next


name_INCR2:     db lo(name_DECR),hi(name_DECR)
                db 2,'2','+'
cw_INCR2:       db lo(code_INCR2),hi(code_INCR2)
code_INCR2:     popab
                adi 2
                mov c,a
                mov a,b
                aci 0
                mov b,a
                mov a,c
                pushab
                jmp next


name_DECR2:     db lo(name_INCR2),hi(name_INCR2)
                db 2,'2','-'
cw_DECR2:       db lo(code_DECR2),hi(code_DECR2)
code_DECR2:     popab
                sui 2
                mov c,a
                mov a,b
                sbi 0
                mov b,a
                mov a,c
                pushab
                jmp next

;------------------------------------------------------------------------
; math
;------------------------------------------------------------------------


name_ADD:       db lo(name_DECR2),hi(name_DECR2)
                db 1,'+'
cw_ADD:         db lo(code_ADD),hi(code_ADD)
code_ADD:       popab                   ; BA = first operand
                mov c,a                 ; BC = first operand
                dstackptr_get           ; we will modify top-of-stack in place
                mov a,m
                add c
                mov m,a                 ; (dstack) = (dstack) + op1.LSB
                inr l                   ; point to MSB
                mov a,m
                adc b
                mov m,a                 ; (dstack+1) = (dstack+1) + op1.MSB + carry
                jmp next

name_SUB:       db lo(name_ADD),hi(name_ADD)
                db 1,'-'
cw_SUB:         db lo(code_SUB),hi(code_SUB)
code_SUB:       popab                   ; BA = first operand
                mov c,a
                mov a,m
                sub c
                mov m,a                 ; (dstack) = (dstack) - op1.LSB
                inr l                   ; point to MSB
                mov a,m
                sbb b                   ; (dstack) = (dstack) - op1.MSB - carry
                mov m,a
                jmp next

name_MULT:      db lo(name_SUB),hi(name_SUB)
                db 1,'*'
cw_MULT:        db lo(code_MULT),hi(code_MULT)
code_MULT:      e_save
                popab
                mov c,a
                popab
                call multfunc
                pushab
                e_restore
                jmp next

name_DIVMOD:    db lo(name_MULT),hi(name_MULT)
                db 4,'/','M','O','D'
cw_DIVMOD:      db lo(code_DIVMOD),hi(code_DIVMOD)
code_DIVMOD:    popab                   ; pop divisor
                mov c,a
                popab
                div16_8
                push_0c
                pushab
                jmp next


name_EQUAL:     linklast DIVMOD
                db 1,'='
cw_EQUAL:       db lo(code_EQUAL),hi(code_EQUAL)
code_EQUAL:     e_save
                popab
                mov c,b
                mov e,a
                popab
                cmp e
                jnz _EQUAL_NOT
                mov a,b
                cmp c
                jnz _EQUAL_NOT
                e_restore
                pushconst 1
                jmp next
_EQUAL_NOT:     e_restore
                pushconst 0
                jmp next


name_NEQUAL:    linklast EQUAL
                db 2,'<','>'
cw_NEQUAL:      db lo(code_NEQUAL),hi(code_NEQUAL)
code_NEQUAL:    e_save
                popab
                mov c,b
                mov e,a
                popab
                cmp e
                jnz _NEQUAL_YES
                mov a,b
                cmp c
                jnz _NEQUAL_YES
                e_restore
                pushconst 0
                jmp next
_NEQUAL_YES:    e_restore
                pushconst 1
                jmp next


name_LT:        linklast NEQUAL
                db 1,'<'
cw_LT:          db lo(code_LT),hi(code_LT)
code_LT:        e_save
                popab
                mov c,b
                mov e,a         ; CE = first arg
                popab           ; BA = second arg

                sub e           ; A = A - E
                mov a,b
                sbb c           ; A = B - C
                jm _LT_YES      ; first arg > second arg
                jmp _LT_NOT

_LT_YES:        e_restore
                pushconst 1
                jmp next
_LT_NOT:        e_restore
                pushconst 0
                jmp next


name_LTE:       linklast LT
                db 2,'<','='
cw_LTE:         db lo(code_LTE),hi(code_LTE)
code_LTE:       e_save
                popab
                mov c,b
                mov e,a         ; CE = first arg
                popab           ; BA = second arg

                mov l,a

                sub e           ; A = A - E
                mov a,b
                sbb c           ; A = B - C
                jm _LTE_YES     ; first arg > second arg

                mov a,l
                cmp e
                jnz _LTE_NOT    ; not equal
                mov a,b
                cmp c
                jnz _LTE_NOT    ; not equal

_LTE_YES:       e_restore
                pushconst 1
                jmp next
_LTE_NOT:       e_restore
                pushconst 0
                jmp next


name_GT:        linklast LTE
                db 1,'>'
cw_GT:          db lo(code_GT),hi(code_GT)
code_GT:        e_save
                popab
                mov c,b
                mov e,a         ; CE = first arg
                popab           ; BA = second arg

                ;; same code as LT, but we swap the args
                ;; before comparing.

                mov h,a         ; swap A and E
                mov a,e
                mov e,h

                mov h,b         ; swap B and C
                mov b,c
                mov c,h

                sub e           ; A = A - E
                mov a,b
                sbb c

                jm _GT_YES
                jmp _GT_NOT
_GT_YES:       
                e_restore
                pushconst 1
                jmp next
_GT_NOT:        e_restore
                pushconst 0
                jmp next


name_GTE:       linklast GT
                db 2,'>','='
cw_GTE:         db lo(code_GTE),hi(code_GTE)
code_GTE:       e_save
                popab
                mov c,b
                mov e,a         ; CE = first arg
                popab           ; BA = second arg

                ;; same code as LTE, but we swap the args
                ;; before comparing.

                mov h,a         ; swap A and E
                mov a,e
                mov e,h

                mov h,b         ; swap B and C
                mov b,c
                mov c,h

                mov l,a         ; save A into L for later

                sub e           ; A = A - E
                mov a,b
                sbb c           ; A = B - C
                jm _GTE_YES      ; first arg > second arg

                mov a,l
                cmp e
                jnz _GTE_NOT    ; not equal
                mov a,b
                cmp c
                jnz _GTE_NOT    ; not equal

_GTE_YES:       e_restore
                pushconst 1
                jmp next
_GTE_NOT:       e_restore
                pushconst 0
                jmp next                


name_ZEQUAL:    linklast GTE
                db 2,'0','='
cw_ZEQUAL:      db lo(code_ZEQUAL),hi(code_ZEQUAL)
code_ZEQUAL:    popab
                ora b
                jnz _ZEQUAL_NOT
                pushconst 1
                jmp next
_ZEQUAL_NOT:    pushconst 0
                jmp next


name_ZGT:       linklast ZEQUAL
                db 2,'0','>'
cw_ZGT:         db lo(code_ZGT),hi(code_ZGT)
code_ZGT:       popab
                mov c,a
                mov a,b
                ora a
                jm _ZGT_NOT             ; minus flag
                ora c                   ; C = LSB or MSB
                jz _ZGT_NOT             ; zero flag
                pushconst 1
                jmp next
_ZGT_NOT:       pushconst 0
                jmp next


name_ZLT:       linklast ZGT
                db 2,'0','<'
cw_ZLT:         db lo(code_ZLT),hi(code_ZLT)
code_ZLT:       popab
                mov c,a
                mov a,b
                ora a
                jp _ZLT_NOT             ; plus flag
                ora c                   ; C = LSB or MSB
                jz _ZLT_NOT             ; zero flag
                pushconst 1
                jmp next
_ZLT_NOT:       pushconst 0
                jmp next


name_AND:       linklast ZLT
                db 3,'A','N','D'
cw_AND:         db lo(code_AND),hi(code_AND)
code_AND:       popab                   ; BA = first operand
                mov c,a                 ; BC = first operand
                dstackptr_get           ; we will modify top-of-stack in place
                mov a,m
                ana c
                mov m,a                 ; (dstack) = (dstack) & op1.LSB
                inr l                   ; point to MSB
                mov a,m
                ana b
                mov m,a                 ; (dstack+1) = (dstack+1) & op1.MSB
                jmp next


name_OR:        linklast AND
                db 2,'O','R'
cw_OR:          db lo(code_OR),hi(code_OR)
code_OR:        popab                   ; BA = first operand
                mov c,a                 ; BC = first operand
                dstackptr_get           ; we will modify top-of-stack in place
                mov a,m
                ora c
                mov m,a                 ; (dstack) = (dstack) & op1.LSB
                inr l                   ; point to MSB
                mov a,m
                ora b
                mov m,a                 ; (dstack+1) = (dstack+1) & op1.MSB
                jmp next


name_XOR:       linklast OR
                db 3,'X','O','R'
cw_XOR:         db lo(code_XOR),hi(code_XOR)
code_XOR:       popab                   ; BA = first operand
                mov c,a                 ; BC = first operand
                dstackptr_get           ; we will modify top-of-stack in place
                mov a,m
                xra c
                mov m,a                 ; (dstack) = (dstack) & op1.LSB
                inr l                   ; point to MSB
                mov a,m
                xra b
                mov m,a                 ; (dstack+1) = (dstack+1) & op1.MSB
                jmp next


name_INVERT:    linklast XOR
                db 6,'I','N','V','E','R','T'
cw_INVERT:      db lo(code_INVERT),hi(code_INVERT)
code_INVERT:    popab                   ; BA = first operand
                xri 0FFH
                mov c,a
                mov a,b
                xri 0FFH
                mov b,a
                mov a,c
                pushab
                jmp next


;------------------------------------------------------------------------
; more words
;------------------------------------------------------------------------


name_EXIT:      linklast INVERT
                db 4,'E','X','I','T'
cw_EXIT:        db lo(code_EXIT),hi(code_EXIT)
code_EXIT:      mvi h,rstackpage
                rstackptr_get
                mov c,m                 ; C has LSB of return addr
                inr l
                mov b,m                 ; B has MSB of return addr
                inr l
                rstackptr_put

                mvi l,cur               ; store return addr into cur
                mov m,c
                inr l
                mov m,b

                jmp next

EXIT:           equ cw_EXIT
     

name_LIT:       db lo(name_EXIT),hi(name_EXIT)
                db 3,'L','I','T'
cw_LIT:         db lo(code_LIT),hi(code_LIT)
code_LIT:
                mvi h,rstackpage
                mvi l,cur
                m_to_hl                 ; HL=(cur)
                m_to_ba                 ; AB=((cur))
                pushab                  ; push to data stack
                double_inc_cur next     ; cur+=2 and jump to next


;------------------------------------------------------------------------
; memory
;------------------------------------------------------------------------

name_STORE:     db lo(name_LIT),hi(name_LIT)
                db 1,'!'
cw_STORE:       db lo(code_STORE),hi(code_STORE)
code_STORE:     e_save
                popab
                mov c,b                 ; CE = address
                mov e,a
                popab
                mov h,c
                mov l,e
                ba_to_m
                e_restore
                jmp next


name_FETCH:     db lo(name_STORE),hi(name_STORE)
                db 1,'@'
cw_FETCH:       db lo(code_FETCH),hi(code_FETCH)
code_FETCH:     popab
                mov h,b                 ; Set HL to BA
                mov l,a
                m_to_ba
                pushab                  ; push it
                jmp next


name_ADDSTORE:  linklast FETCH
                db 2,'+','!'
cw_ADDSTORE:    db lo(code_ADDSTORE),hi(code_ADDSTORE)
code_ADDSTORE:  e_save
                popab
                mov c,b                 ; CE = address
                mov e,a
                popab                   ; BA = amount
                mov h,c
                mov l,e

                add m                   ; A = A + M            (LSB)
                mov m,a                 ; M = A
                inr_hl
                mov a,b                 ; A = B                (MSB)
                adc m                   ; A = A + M + CARRY
                mov m,a                 ; M = B
                e_restore
                jmp next


name_STOREBYTE: linklast ADDSTORE
                db 2,'C','!'
cw_STOREBYTE:   db lo(code_STOREBYTE),hi(code_STOREBYTE)
code_STOREBYTE: e_save
                popab
                mov c,b                 ; CE = address
                mov e,a
                popab
                mov h,c
                mov l,e
                mov m,a                 ; store A in CE
                e_restore
                jmp next

name_FETCHBYTE: linklast STOREBYTE
                db 2,'C','G'
cw_FETCHBYTE:   db lo(code_FETCHBYTE),hi(code_FETCHBYTE)
code_FETCHBYTE: popab
                mov h,b
                mov l,a
                mov a,m
                mvi b,0
                pushab
                jmp next

;------------------------------------------------------------------------
; variables
;------------------------------------------------------------------------

name_STATE:     linklast FETCHBYTE
                db 5,'S','T','A','T','E'
cw_STATE:       db lo(code_STATE),hi(code_STATE)
code_STATE:     pushconst state_addr
                jmp next

name_HERE:      db lo(name_STATE),hi(name_STATE)
                db 4,'H','E','R','E'
cw_HERE:        db lo(code_HERE),hi(code_HERE)
code_HERE:      pushconst here_addr
                jmp next

name_LATEST:    db lo(name_HERE),hi(name_HERE)
                db 6,'L','A','T','E','S','T'
cw_LATEST:      db lo(code_LATEST),hi(code_LATEST)
code_LATEST:    pushconst latest_addr
                jmp next

name_S0:        db lo(name_LATEST),hi(name_LATEST)
                db 2,'S','0'
cw_S0:          db lo(code_S0),hi(code_S0)
code_S0:        pushconst s0_addr
                jmp next

name_BASE:      db lo(name_S0),hi(name_S0)
                db 4,'B','A','S','E'
cw_BASE:        db lo(code_BASE),hi(code_BASE)
code_BASE:      pushconst base_addr
                jmp next

;------------------------------------------------------------------------
; more words
;------------------------------------------------------------------------


name_QUIT:      db lo(name_BASE),hi(name_BASE)
                db 4,'Q','U','I','T'
cw_QUIT:        codeword_DOCOL
                cw RZ
                cw RSPSTORE
                cw INTERPRET
                cw BRANCH
                dw -4


name_CREATE:    db lo(name_QUIT),hi(name_QUIT)
                db 6,'C','R','E','A','T','E'
cw_CREATE:      db lo(code_CREATE),hi(code_CREATE)
code_CREATE:    de_save
                popab
                mov c,a                 ; C = length
                popab                   ; AB = address of name
                ab_variable temp1       ; save AB to temp1

                variable_de here        ; get HERE into DE

                variable_ab latest      ; get LATEST into AB
                uni_dehl
                mov m,a                 ; store LATEST LSB
                inr_hl
                mov m,b                 ; ... and MSB
                inr_hl
                mov m,c                 ; store length
                inr_hl
                uni_hlde                ; DE = here+3

                variable_ab temp1       ; address of name
                mov h,b
                mov l,a                 ; HL = source address

_CREATE_LOOP:   mov a,m
                inr_hl
                swap_dehl
                mov m,a
                inr_hl
                swap_dehl
                dcr c
                jnz _CREATE_LOOP

                variable_ab here        ; AB = HERE
                ab_variable latest      ; LATEST = HERE
                de_variable here

                de_restore
                inr d                   ; we saved D before adjusted it for POPs
                inr d
                inr d
                inr d
                jmp next


name_COMMA:     db lo(name_CREATE),hi(name_CREATE)
                db 1,','
cw_COMMA:       db lo(code_COMMA),hi(code_COMMA)
code_COMMA:     popab
                call _COMMA
                jmp next

                ;; on entry, BA = value

_COMMA:         mvi h,rstackpage        ; get the value stored in here
                mvi l,here
                mov c,m
                inr l
                mov h,m                 
                mov l,c                 ; HL now points to (here)
                ba_to_m                 ; Store BA in (here)

                mvi h,rstackpage
                mvi l,here
                double_inc_m _COMMA_DONE
_COMMA_DONE:
                ret


name_LBRAC:     db lo(name_COMMA),hi(name_COMMA)
                db 1|F_IMMED,'['
cw_LBRAC:       db lo(code_LBRAC),hi(code_LBRAC)
code_LBRAC:     state_clear
                jmp next


name_RBRAC:     db lo(name_LBRAC),hi(name_LBRAC)
                db 1,']'
cw_RBRAC:       db lo(code_RBRAC),hi(code_RBRAC)
code_RBRAC:     state_set
                jmp next


name_COLON:     db lo(name_RBRAC),hi(name_RBRAC)
                db 1,':'
cw_COLON:       db lo(DOCOL),hi(DOCOL)
                db lo(cw_WORD),hi(cw_WORD)
                db lo(cw_CREATE),hi(cw_CREATE)
                db lo(cw_LIT),hi(cw_LIT)
                db lo(DOCOL),hi(DOCOL)
                db lo(cw_COMMA),hi(cw_COMMA)
                db lo(cw_LATEST),hi(cw_LATEST)
                db lo(cw_FETCH),hi(cw_FETCH)
                db lo(cw_HIDDEN),hi(cw_HIDDEN)
                db lo(cw_RBRAC),hi(cw_RBRAC)
                db lo(EXIT),hi(EXIT)


name_SEMICOLON: db lo(name_COLON),hi(name_COLON)
                db 1|F_IMMED,';'
cw_SEMICOLON:   db lo(DOCOL),hi(DOCOL)
                db lo(cw_LIT),hi(cw_LIT)
                db lo(EXIT),hi(EXIT)
                db lo(cw_COMMA),hi(cw_COMMA)
                db lo(cw_LATEST),hi(cw_LATEST)
                db lo(cw_FETCH),hi(cw_FETCH)
                db lo(cw_HIDDEN),hi(cw_HIDDEN)
                db lo(cw_LBRAC),hi(cw_LBRAC)
                db lo(EXIT),hi(EXIT)


name_IMMEDIATE: db lo(name_SEMICOLON),hi(name_SEMICOLON)
                db 9,'I','M','M','E','D','I','A','T','E'
cw_IMMEDIATE:   db lo(code_IMMEDIATE),hi(code_IMMEDIATE)
code_IMMEDIATE: variable_ab latest
                mov h,b
                mov l,a
                double_inc_hl
                mov a,m
                xri F_IMMED
                mov m,a
                jmp next


name_HIDDEN:    db lo(name_IMMEDIATE),hi(name_IMMEDIATE)
                db 6,'H','I','D','D','E','N'
cw_HIDDEN:      db lo(code_HIDDEN),hi(code_HIDDEN)
code_HIDDEN:    pophl                   ; get address of word
                double_inc_hl           ; skip the link and point to the name/flags
                mov a,m
                xri F_HIDDEN
                mov m,a
                jmp next


name_HIDE:      db lo(name_HIDDEN),hi(name_HIDDEN)
                db 4,'H','I','D','E'
cw_HIDE:        db lo(DOCOL),hi(DOCOL)
                db lo(cw_WORD),hi(cw_WORD)
                db lo(cw_FIND),hi(cw_FIND)
                db lo(cw_HIDDEN),hi(cw_HIDDEN)
                db lo(EXIT),hi(EXIT)


name_TICK:      db lo(name_HIDE),hi(name_HIDE)
                db 1,"'"
cw_TICK:        db lo(code_TICK),hi(code_TICK)
code_TICK:      mvi h,rstackpage        ; NOTE: identical to the code for LIT
                mvi l,cur
                m_to_hl                 ; HL=(cur)
                m_to_ba                 ; AB=((cur))
                pushab                  ; push to data stack
                double_inc_cur next     ; cur+=2 and jump to next


name_BRANCH:    db lo(name_TICK),hi(name_TICK)
                db 6,'B','R','A','N','C','H'
cw_BRANCH:      db lo(code_BRANCH),hi(code_BRANCH)
code_BRANCH:    mvi h,rstackpage
                mvi l,cur
                mov a,m
                inr l
                mov h,m
                mov l,a         ; HL = cur

                mov c,m
                inr_hl
                mov b,m         ; BC = (cur)
                dcr_hl

                mov a,l
                add c
                mov c,a         ; C = L + C

                mov a,h
                adc b
                mov b,a         ; B = H + B + carry

                mvi h,rstackpage
                mvi l,cur
                mov m,c         ; store cur LSB
                inr l
                mov m,b         ; store cur MSB
                jmp next


name_ZBRANCH:   db lo(name_BRANCH),hi(name_BRANCH)
                db 7,'0','B','R','A','N','C','H'
cw_ZBRANCH:     db lo(code_ZBRANCH),hi(code_ZBRANCH)
code_ZBRANCH:   popab
                ora b
                jz code_BRANCH
                double_inc_cur next     ; cur+=2 and jump to next


name_LITSTRING: db lo(name_ZBRANCH),hi(name_ZBRANCH)
                db 9,'L','I','T','S','T','R','I','N','G'
cw_LITSTRING:   db lo(code_LITSTRING), hi(code_LITSTRING)
code_LITSTRING: variable_ab cur         ; AB = address of length of string
                mov h,b
                mov l,a                 ; HL = address of length of string
                mov c,m                 ; C = length of string
                double_inc_cur _LIT1    ; skip over string length
_LIT1:          variable_ab cur         ; AB = address of string
                pushab                  ; push address of string
                mvi h,rstackpage
                mvi l,cur
                mov a,m                 ; get LSB of cur
                add c                   ; add string length
                mov m,a                 ; store LSB
                inr l
                mov a,m                 ; get MSB of cur
                aci 0                   ; add in carry
                mov m,a                 ; store MSB
                push_0c                 ; push length of string
                jmp next


name_TELL:      db lo(name_LITSTRING),hi(name_LITSTRING)
                db 4,'T','E','L','L'
cw_TELL:        db lo(code_TELL), hi(code_TELL)
code_TELL:      popab
                mov c,a
                popab
                mov l,a
                mov h,b
                mov a,c                 ; test the length byte
                ora a
                jz next                 ; empty string
_TELL_LOOP:     mov a,m
                inr_hl
                call _EMIT
                dcr c
                jnz _TELL_LOOP
                jmp next


name_BYE:       db lo(name_TELL),hi(name_TELL)
                db 3,'B','Y','E'
cw_BYE:         db lo(code_BYE),hi(code_BYE)
code_BYE:       hlt
                ;; there is no next 

name_DO:        linklast BYE
                db 2,'D','O'
cw_DO:          db lo(code_DO),hi(code_DO)
code_DO:        popab                   ; initial
                rspushab                ; push onto return stack
                mvi a,1
                mvi b,0
                rspushab                ; increment
                popab                   ; limit
                rspushab                ; push onto return stack
                variable_ab cur
                rspushab                ; push onto return stack
                jmp next

name_LOOP:      linklast DO
                db 4,'L','O','O','P'
cw_LOOP:        db lo(code_LOOP),hi(code_LOOP)
                ; rstack has the address of the next statement
                ; rstack-1 has the limit
                ; rstack-2 has the increment
                ; rstack-3 has the index
code_LOOP:      de_save
                mvi h,rstackpage
                rstackptr_get           ; starts pointing at loop address
                inr l
                inr l                   ; now pointing at limit
                inr l                   
                inr l                   ; now pointing at increment
                mov c,m                 ; increment LSB
                inr l
                mov b,m                 ; increment MSB
                inr l                   ; now pointing at index
                mov a,m                 ; get index lsb
                add c
                mov m,a                 ; store index lsb
                mov e,a                 ; save a copy of index lsb in e
                inr l
                mov a,m                 ; get index msb
                adc b                   ; add carry
                mov m,a                 ; store index msb
                mov d,a                 ; DE has index

                dcr l                   ; pointing to index LSB
                dcr l                   ; pointing at increment msb
                dcr l                   ; pointing at increment lsb
                dcr l                   ; pointing to limit MSB
                mov b,m                 ; B = limit MSB
                dcr l                   ; pointing to limit LSB
                mov c,m                 ; BC has limit

                ;; check to see if we have hit the limit
                ;; note -- this does not check to see if we've EXCEEDED the limit (bug)

                ; DE = index
                ; BC = limit

                mvi l,e_temp
                mov l,m                 ; get saved rstackptr into l
                inr l
                inr l                   ; now pointing at limit
                inr l                   
                inr l                   ; now pointing at increment
                inr l
                mov a,m                 ; increment MSB
                rlc
                jc LOOP_neg

                mov a,e
                sub c
                mov a,d
                sbb b
                jm LOOP_CONT
                jmp LOOP_STOP

LOOP_neg:       mov a,e
                sub c
                mov a,d
                sbb b
                jp LOOP_CONT
                jmp LOOP_STOP

;                mov a,e
;                cmp c
;                jnz LOOP_CONT
;                mov a,d
;                cmp b
;                jnz LOOP_CONT
                
LOOP_STOP:      de_restore
                rstackptr_get           ; we're done. Clean all our crap off the return stack
                inr l
                inr l
                inr l
                inr l
                inr l
                inr l
                inr l
                inr l
                rstackptr_put
                jmp next

LOOP_CONT:      de_restore
                rstackptr_get           ; set cur to the address that we saved in DO
                mov a,m
                inr l
                mov b,m
                ab_variable cur
                jmp next

name_PLUSLOOP:  linklast LOOP
                db 5,'+','L','O','O','P'
cw_PLUSLOOP:    db lo(code_PLUSLOOP),hi(code_PLUSLOOP)
code_PLUSLOOP:  popab
                mvi h,rstackpage
                rstackptr_get           ; starts pointing at loop address
                inr l
                inr l                   ; now pointing at limit
                inr l                   
                inr l                   ; now pointing at increment
                mov m,a
                inr l
                mov m,b
                jmp code_LOOP

name_I:         linklast PLUSLOOP
                db 1,'I'
cw_I:           db lo(code_I),hi(code_I)
code_I:         rstackptr_get           ; should be pointing at loop address
                inr l
                inr l                   ; now pointing at limit
                inr l                   
                inr l                   ; now pointing at increment
                inr l                   
                inr l                   ; now pointing at index
                mov a,m
                inr l
                mov b,m
                pushab
                jmp next


name_INTERPRET: linklast I
                db 9,'I','N','T','E','R','P','R','E','T'
cw_INTERPRET:   db lo(code_INTERPRET),hi(code_INTERPRET)

code_INTERPRET: call _WORD              ; returns word in wordbuf, length in C
                islit_clear             ; clear islit
                mvi b,rstackpage        ; _FIND expects address in BA
                mvi a,wordbuf
                call _FIND              ; returns address in HL if found
                mov a,h
                ora l
                jz _INTERP1             ; not in the dictionary

                double_inc_hl           ; skip the codeword

                mov c,m                 ; get the flags into C
                call _TCFA_ATLEN
                ab_variable callee      ; save it to temp1 for good safe keeping
                mov a,c                 ; restore flags to A
                ani F_IMMED
                jnz _INTERP4            ; immediate - go to execute
                jmp _INTERP2            ; check state

_INTERP1:       islit_set
                mvi b,rstackpage        ; _NUMBER expects buffer in BA
                mvi a,wordbuf
                call _NUMBER
                ;; _NUMBER will return with value stored in rstackpage:value
                ;; TODO: error check on _NUMBER
                variable_check numerr
                jnz _INTERP6
                symbol_variable cw_LIT,callee

_INTERP2:       state_check
                jz _INTERP4             ; executing

                variable_ab callee
                call _COMMA
                islit_check
                jz _INTERP3             ; not literal
                value_ab
                call _COMMA
_INTERP3:       jmp next

_INTERP4:       islit_check
                jnz _INTERP5            ; yes literal

                variable_hl callee      ; get the address of the codeword back into HL
                jmp_hl_indir

_INTERP5:       value_ab
                pushab
                jmp next

_INTERP6:       jmp fault_token;
                ;mvi h,hi(interperrtxt)
                ;mvi l,lo(interperrtxt)
                ;call puts
                ;jmp next

name_CHAR:      db lo(name_INTERPRET),hi(name_INTERPRET)
                db 4,'C','H','A','R'
cw_CHAR:        db lo(code_CHAR),hi(code_CHAR)
code_CHAR:      call _WORD
                mvi h,rstackpage
                mvi l,wordbuf
                mov a,m
                mvi b,0
                pushab
                jmp next


name_EXECUTE:   db lo(name_CHAR),hi(name_CHAR)
                db 7,'E','X','E','C','U','T','E'
cw_EXECUTE:     db lo(code_EXECUTE),hi(code_EXECUTE)
code_EXECUTE:   popab                   ; get execution token
                mov h,b
                mov l,a
                m_to_ba                 ; deference execution token
                mvi h,rstackpage
                mvi l,jmpa_addr         ; write the address into jmpa
                mov m,a
                inr l
                mov m,b
                jmp jmpa_jump


name_OUT:       linklast EXECUTE
                db 3,"OUT"
cw_OUT:         db lo(code_OUT),hi(code_OUT)
code_OUT:       popab                   ; get the port address into A
                ani 00011111B           ; construct the "OUT" instruction
                rlc
                ori 01000001B
                mvi h,hi(jmpa_addr)
                mvi l,lo(jmpa_addr)
                mov m,a                 ; store the "OUT" instruction at jmp_addr
                inr l
                mvi m,07H               ; store the "RET" instruction at jmp_addr+1
                mov a,b
                popab                   ; get the value in A
                call jmpa_addr
                jmp next


LASTWORD_KERNEL:
name_IN:        linklast OUT
                db 2,"IN"
cw_IN:          db lo(code_IN),hi(code_IN)
code_IN:        popab
                ani 00000111B
                rlc
                ori 01000001B
                mvi h,hi(jmpa_addr)
                mvi l,lo(jmpa_addr)
                mov m,a                   ; store the "IN" instruction at jmp_addr
                inr l
                mvi m,07H                 ; store the "RET" instruction at jmp_addr+1
                call jmpa_addr            ; execute the "IN" instruction
                mvi b,0
                pushab                    ; save to the stack
                jmp next

                include "forth-extra.inc"

LASTWORD        equ LASTWORD_EXTRA

write_crlf:     mvi a,CR
                call _EMIT
                mvi a,LF
                jmp _EMIT

;; write_hex. Prints the hex value in A. Destroys A,B,C.

write_hex:  mov c,a                 ; save the byte in C
            rrc                     ; rotate most significant nibble into lower 4 bits
            rrc
            rrc
            rrc
            call hex2ascii          ; convert the most significand digit to ascii
            call _emit              ; print the most significant digit
            mov a,c                 ; restore
            call hex2ascii
            call _emit
            ret
hex2ascii:  ani 0FH                 ; mask all but the lower nibble
            cpi 0AH
            jc hex2ascii1           ; jump if the nibble is less than 10
            adi 7                   ; add 7 to convert to A-F
hex2ascii1: adi 30H
            ret

;; puts. Prints the string in HL until a \0 is encountered.

puts:       mov a,m
            ana a
            rz                      ; end of string
            call _emit
            inr l                   ; next character
            jnz puts
            inr h
            jmp puts

token_fault_txt: db ">> Unknown Token or Not a Number: ",0

titletxt:   db "\r\n\r\n"
            db "8008-Forth by smbaker\r\n"
            db "https://www.smbaker.com/\r\n\r\n",0

                include "bufio.inc"
                include "lib/serial.inc"



