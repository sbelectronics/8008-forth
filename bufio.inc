;------------------------------------------------------------------------
; Simple Line Buffer
; Scott Baker, http://www.smbaker.com/
;
; The buffer occupies an entire page (256 bytes) of memory. No effort is
; made to prevent overrun. If you type more than 256 characters before
; hitting <CR> it will start erasing old characters.
;------------------------------------------------------------------------

buf_discard	macro
		mvi l,bufhead
		mov b,m
		mvi l,buftail
		mov a,m			; L = tail pointer
		cmp b			; Prevent the user from moving past the head
		jz empty
		sbi 1			; decrement tail pointer
		mov m,a			; store tail pointer
empty:
		endm

buf_enqueue	macro
		mvi l,buftail		; point to buffer tail pointer
		mov l,m			; L = bufptr
		mvi h,bufpage
		mov m,a			; store A into buffer
		mvi h,rstackpage	; point back to default page
		inr l			; increment buffer tail pointer
		mov b,l			; save bufptr in B
		mvi l,buftail		; point to tail
		mov m,b			; store tail
		endm

buf_dequeue	macro
		mvi l,bufhead		; point to buffer tail pointer
		mov l,m			; L = bufptr
		mvi h,bufpage
		mov a,m			; character to return in A
		mvi h,rstackpage	; point back to default page
		inr l			; decrement buffer head pointer
		mov b,l			; save bufptr in B
		mvi l,bufhead		; point to head
		mov m,b			; store head
		endm

buf_init:	jmp buf_reset

buf_reset:	mvi h,rstackpage
		mvi l,bufhead		; bufhead points at the current slot to remove
		mvi m,0
		mvi l,buftail		; buftail points to the next empty slot to fill
		mvi m,0
		mvi l,bufrdy		; bufrdy is 1 a LINETERM was enqueued, and 0 if it was DEQUEUED
		mvi m,0
		ret

		; Modifies A,B
		; Leaves H set to rstackpage

buf_read:	mvi h,rstackpage
		mov a,l			; save L
		mvi l,l_temp		; .. into l_temp
		mov m,a			
		mvi l,bufrdy		; is there a CR in the buffer?
		mov a,m
		ora a
		jz buf_fill		; No. Fill until we get a CR
buf_retchar:    buf_dequeue		; Get the next character from the buffer
		cpi LINETERM		; Is it CR?
		jnz buf_read_ret        ; Nope.
		mvi l,bufrdy		; Yes. The buffer is no longer in ready state
		mvi m,0
buf_read_ret:	mvi l,l_temp
		mov l,m
		ret			; return character in A


buf_fill:	call CINP		; get a character
		ani 07FH		; strip off that crazy scelbi high bit
		cpi 08H
		jz buf_backspace	; handle backspace
		buf_enqueue		; store character in buffer
		cpi LINETERM
		jnz buf_fill		; not line terminator
		mvi l,bufrdy		; signal ready
		mvi m,1
		mvi a,LF
		call _EMIT		; follow the CR with an LF
		mvi a,CR
		jmp buf_retchar

; alternate implementation that translates LF to CR
;
;notdel:	cpi LF			; If user typed a LF, then turn it into a CR
;		jnz notlf		; This handles sending Linux files from SIMH
;		mvi a,LINETERM
;		jmp notcr
;
;notlf:		cpi CR			; If user typed a CR, then emit an LF
;		jnz notcr
;		mvi a,LF
;		call _EMIT
;		mvi a,CR
;
;notcr:		cpi 08H
;		jz buf_backspace	; handle backspace
;
;		buf_enqueue		; store character in buffer
;		cpi LINETERM
;		jnz buf_fill		; not line terminator
;		mvi l,bufrdy		; signal ready
;		mvi m,1
;		jmp buf_retchar

buf_backspace:  mvi A,SPACE
		call _EMIT
		mvi A,BS
		call _EMIT
		buf_discard		; on backspace, remove a character from the buffer
		jmp buf_fill		; continue filling

