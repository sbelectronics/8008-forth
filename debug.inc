name_PRSTACK:   db lo(name_TELL),hi(name_TELL)
                db 7,'P','R','S','T','A','C','K'
cw_PRSTACK:     db lo(code_PRSTACK),hi(code_PRSTACK)

code_PRSTACK:   call write_crlf
                mvi h,dstackpage
                dstackptr_get

PRSTACK_LOOP:   mov a,l
                cpi dstack_top
                jz PRSTACK_DONE

                inr l                   ; point at the MSB
                mov a,m
                call write_hex
                dcr l                   ; now back to the LSB
                mov a,m
                call write_hex
                inr l                   ; point to the next cell
                inr l
                mvi a,' '
                call _EMIT
                jmp PRSTACK_LOOP

PRSTACK_DONE:   call write_crlf
                jmp next

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




