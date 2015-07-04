.MODEL compact ;multiple data, one code segment, stack and near data segment are grouped

;-------------------------------------------------------------------------
;debug mode
debug=0
;macros
setvideomode macro NewMode,OldMode
    ;get current mode, set in al
    mov ah,0Fh
    int 10h
    mov OldMode,al
    ;set new mode
    mov ah,00h
    mov al,NewMode
    int 10h
endm

restorevideomode macro OldMode 
    mov ah,00h
    mov al,OldMode
    int 10h
endm

blankscreenmode13 macro
    local blankscreenmode13_wait1,blankscreenmode13_wait2
    ;store context
    push ax
    push cx
    push dx
    push es
    push di
    ;set target registers for stosb
    mov ax,0A000h
    mov es,ax
    xor di,di
    ;wait for vblank
    mov dx, 03dah ; VGA status port                 
blankscreenmode13_wait1: ;if in vblank, wait until vblank finishes
    in al, dx
    and al, 8
    jnz blankscreenmode13_wait1                 ; busy wait
blankscreenmode13_wait2:                        ;wait until begin of vblank
    in al, dx
    and al, 8
    jz blankscreenmode13_wait2             ; and again
    ;set fill color
    mov al,0
    mov cx,64000 ;size of video ram
    rep stosb ;blit
    ;restore context
    pop di
    pop es
    pop dx
    pop cx
    pop ax
endm

;returns offset in AX
screenbufferoffset macro X,Y
    push DX
    mov AX,screenwidth
    imul Y
    add AX,X
    pop DX
endm

;debugging
print macro character
    mov ah,02h
    mov dl,character
    int 21h
endm

printcrlf macro
    mov ah,02h
    mov dl,0Ah
    int 21h
    mov ah,02h
    mov dl,0Dh
    int 21h
endm
;-------------------------------------------------------------------------

;-------------------------------------------------------------------------
;const defines
rectwidth = 10
rectheight = 10
rectcolor =50
rectcolor2 =0;100
koekjecolor = 150
keybbufsize=256
screenwidth=320
screenheight=200

vakjeshorizontaal = 20
vakjesvertikaal = 32

;scancodes for movement keys
leftscancode=09eh ;a
rightscancode=0a0h ;d
topscancode=091h ;w
bottomscancode=09fh ;s
exitscancode=0adh ;x

;amount of movement
movementatkeypresshor=10
movementatkeypressver=10
;-------------------------------------------------------------------------

;-------------------------------------------------------------------------
; stack and data segments
.STACK 1024 ;stack, 1024 bytes

.FARDATA ;for storing off-line buffer
    videobuffer db 64000 dup (0) ;init backbuffer to zero

.DATA ;data segment, static variables
;old keyboard interrupt handler
    oldhandler dd ?
;old video mode
    oldvideomode db ?
;rectangle graphic 
    rectangle db rectwidth dup (rectcolor),(rectheight-2) dup (rectcolor,(rectwidth-2) dup (0),rectcolor),rectwidth dup (rectcolor)
;koekje graphic 
    koekje db rectwidth dup (koekjecolor),(rectheight-2) dup (koekjecolor,(rectwidth-2) dup (0),koekjecolor),rectwidth dup (koekjecolor)
;origin of rectangle
    xpos dw 40 ;current position head
    ypos dw 40
    xposprev dw 30 ;previous position tail
    yposprev dw 20 
;keyboard scan code circular buffer
    keybbuf db 256 dup (?) ;256 to get free circular behaviour
    keybbuffront db (0);front index
    keybbufback db (0);back index
;keyboard flags
    KbdFlags db 0
    KbdFlags2 db 0
    KbdFlags3 db 0
    KbdFlags4 db 0
;snake
	snakebody dw 40,40,40,30,30,30,30,20,0,0,0,0 ;db 10 dup (?)
	snakebodyaantal dw 4 
	snakebodyarrayaantal dw 8 ;    6          8 
	snakebodyarrayaantal2 dw 16 ;  8 10     12 14
	xkoekje dw 0
	ykoekje dw 0
	score dw 0
	lives dw 3
    keypressed dw 0
    richting dw 0 ; 0=omhoog, 1 = rechts, 2 = omlaag, 3 = links
    keyboardcounter dw 5
   	randomnumber dw 0
   	koekjeopgegeten dw 1 ; 1 = true, 0 = false
    
    
.CODE ;code segment
.STARTUP ; start main, sets ds to beginning of data segment, updates ss to ds and adjusts sp acordingly
    ;init
    setvideomode 13h,oldvideomode
    call installkeyboardhandler    
    ;main loop
main_loop:
    ;process keyboard
    ;reserve room for return value of process_keyboard on the stack (0 if exit, non-zero otherwise)
    xor ax,ax
    push ax
    call processkeyboard
    pop ax ;return value in ax
    test ax,0FFFFh ;if zero exit main loop
    jz end_main_loop
    ;update backbuffer
    call updatestate
    ;update screen
    call drawscreen
    call WACHTEN
    mov keypressed, 1
    jmp main_loop
end_main_loop:
    ;cleanup
    mov ax,seg oldvideomode
    mov ds,ax
    ;restore old videomode
    restorevideomode oldvideomode
    ;restore keyboardhanlder
    call restorekeyboardhandler
.EXIT ; end main/exit to dos
 ;procedures


; SetCmd-   Sends the command byte in the AL register to the 8042
;       keyboard microcontroller chip (command register at
;       port 64h).

SetCmd proc near
        push    cx
        push    ax      ;Save command value.
        cli         ;Critical region, no ints now.

; Wait until the 8042 is done processing the current command.

        xor cx, cx      ;Allow 65,536 times thru loop.
Wait4Empty: in  al, 64h     ;Read keyboard status register.
        test    al, 10b     ;Input buffer full?
        loopnz  Wait4Empty  ;If so, wait until empty.
; Okay, send the command :
        pop ax      ;Retrieve command.
        out 64h, al
        sti         ;Okay, ints can happen again.
        pop cx
        ret
SetCmd      endp



; SendCmd-  The following routine sends a command or data byte to the
; keyboard data port (port 60h).

SendCmd     proc    near
        push    ds
        push    bx
        push    cx
        mov bx, ax      ;Save data byte

        mov bh, 3       ;Retry cnt.
RetryLp:    cli         ;Disable ints while accessing HW.

; Clear the Error, Acknowledge received, and resend received flags
; in KbdFlags4

        and KbdFlags4, 4fh

; Wait until the 8042 is done processing the current command.

        xor cx, cx          ;Allow 65,536 times thru loop.
Wait4Empty: in  al, 64h         ;Read keyboard status register.
        test    al, 10b         ;Input buffer full?
        loopnz  Wait4Empty      ;If so, wait until empty.

; Okay, send the data to port 60h

        mov al, bl
        out 60h, al
        sti             ;Allow interrupts now.

; Wait for the arrival of an acknowledgement from the keyboard ISR:

        xor cx, cx          ;Wait a long time, if need be.
Wait4Ack:   
        test KbdFlags4, 10h  ;Acknowledge received bit.
        jnz GotAck
        loop Wait4Ack
        dec bh          ;Do a retry on this guy.
        jne RetryLp

; If the operation failed after 3 retries, set the error bit and quit.

        or  KbdFlags4, 80h  ;Set error bit.
GotAck:     
        pop cx
        pop bx
        pop ds
        ret
SendCmd endp


keybinterrupthandler proc far
        push ds
        push ax
        push bx
        push cx
        mov ax,@data
        mov ds,ax
        
        mov al, 0ADh        ;Disable keyboard
        call SetCmd
        cli                 ;Disable interrupts. interrupts reenabled when flags are popped
        xor cx, cx
Wait4Data:  in  al, 64h     ;Read kbd status port.
        test    al, 10b     ;Data in buffer?
        loopz   Wait4Data   ;Wait until data available.
        in  al, 60h         ;Get keyboard data.
        
        if debug
        xor ah,ah
        push ax
        call printint
        printcrlf
        endif
        
        cmp al, 0EEh        ;Echo response?
        je  QuitInt9
        cmp al, 0FAh        ;Acknowledge?
        jne NotAck
        or  KbdFlags4, 10h  ;Set ack bit.
        jmp QuitInt9

NotAck: cmp al, 0FEh        ;Resend command?
        jne NotResend       
        or  KbdFlags4, 20h  ;Set resend bit.
        jmp QuitInt9

NotResend: 
        ;if buffer not full, write scan code in dl to buffer 
        mov cl,keybbufback
        inc cl
        cmp cl,keybbuffront
        jz QuitInt9
        ;ok buffer not full, insert al and increment keybbufback
        mov bx,offset keybbuf
        mov cl,keybbufback
        xor ch,ch
        add bx,cx
        mov byte ptr [bx],al
        inc keybbufback
bufferfull:                 ;Put in type ahead buffer.
QuitInt9:   
        mov al, 0AEh        ;Reenable the keyboard
        call SetCmd

        mov al, 20h         ;Send EOI (end of interrupt)
        out 20h, al         ; to the 8259A PIC.
        pop cx
        pop bx
        pop ax
        pop ds
        iret
keybinterrupthandler endp

;procedure to install the new keyboard interrupt handler 
installkeyboardhandler proc near
    ;update stack frame
    push bp
    mov bp,sp
    ;save context
    push ax
    push bx
    push dx
    push es
    push ds
    ;get old handler in ES:BX, using DOS int 21h, function 35h
    mov ah,35h ;dos function 35h
    mov al,09h ;interrupt source 9 (keyboard)
    int 21h
    mov word ptr oldhandler,bx
    mov word ptr oldhandler+2,es
    ;set new handler (far ptr in ds:dx)
    mov dx,seg keybinterrupthandler
    mov ds,dx
    mov dx,keybinterrupthandler
    mov ah,25h ;dos function 25h
    mov al,09h ;interrupt source 9 (keyboard)
    int 21h
    ;restore context
    pop ds
    pop es
    pop dx
    pop bx
    pop ax
    ;restore stackframe
    pop bp
    ret
installkeyboardhandler endp

;procedure to restore the old keyboard interrupt handler
restorekeyboardhandler proc near
    ;update stack frame
    push bp
    mov bp,sp
    ;save context
    push ax
    push dx
    push ds
    
    
    sub keyboardcounter,1
    cmp keyboardcounter,0 
    jne keyboardhandlergedaan
    mov keyboardcounter, 5
    
    
    ;reset old handler
    mov dx,word ptr oldhandler
    mov ax,word ptr oldhandler+2
    mov ds,ax
    mov ah,25h ;dos function 25h
    mov al,09h ;interrupt source 9 (keyboard)
    int 21h
keyboardhandlergedaan:
    ;restore context
    pop ds
    pop dx
    pop ax
    ;restore stackframe
    pop bp
    ret
restorekeyboardhandler endp

;translate contents of the keyboard buffer into events
;return value on tos. return value 0 = exit
processkeyboard proc near
    push bp
    mov bp,sp
    push ax
    push bx
    push dx
    push di
    push si
    ;set default return value to one
    mov word ptr [bp+4],1
    ;process all keys from keybbuffront to keybbufback
    mov bx,offset keybbuf
    processkeyboard_nextcode:    
    mov dl,keybbuffront ;if keybbuf empty exit
    cmp dl,keybbufback
    jnz continueprocessing
    jmp processkeyboard_done
continueprocessing:    
    ;process key (switch statement)
    mov al,keybbuffront
    xor ah,ah
    add bx,ax
    mov al,byte ptr [bx]
    ;if left
    cmp al,leftscancode
    jnz top
    mov dx,xpos
    cmp dx,movementatkeypresshor
    ;bounds checking, if dx<movementatkeypresshor ignore
    jl invalid_left_move
    sub dx,movementatkeypresshor ;calculate new pos
    ;mov xpos,dx ;current pos = new pos
    mov richting, 3; links
invalid_left_move: 
    jmp nextkey    
top:
    mov keypressed, 1
    ;if top
    cmp al,topscancode
    jnz right
    mov dx,ypos
    cmp dx,movementatkeypressver
    ;bounds checking, if dx<movementatkeypressver ignore
    jl invalid_top_move
    sub dx,movementatkeypressver ;calculate new pos
    ;mov ypos,dx ;current pos = new pos
    mov richting, 0 ;omhoog
invalid_top_move:   
    jmp nextkey    
right:
    ;if right
    cmp al,rightscancode
    jnz bottom
    mov dx,xpos
    cmp dx,screenwidth-movementatkeypresshor-rectwidth
    ;bounds checking, if dx>=screenwidth-movementatkeypresshor ignore
    jge invalid_right_move
    add dx,movementatkeypresshor ;calculate new pos
    ;mov xpos,dx ;current pos = new pos
    mov richting, 1; rechts
invalid_right_move:   
    jmp nextkey    
bottom:
    ;if bottom
    cmp al,bottomscancode
    jnz exit
    mov dx,ypos
    cmp dx,screenheight-movementatkeypressver-rectheight
    ;bounds checking, if dx>=screenwidth-movementatkeypresshor ignore
    jge invalid_bottom_move
    add dx,movementatkeypressver ;calculate new pos
    ;mov ypos,dx ;current pos = new pos
    mov richting, 2 ; onder
invalid_bottom_move:
    jmp nextkey    
exit:
    ;if exit
    cmp al,exitscancode
    jnz nextkey
    ;return 0    
    
    mov word ptr [bp+4],0
    jmp processkeyboard_done
nextkey:
    inc keybbuffront
    jmp processkeyboard_nextcode    
processkeyboard_done:    
    pop si
    pop di
    pop dx
    pop bx
    pop ax
    pop bp
    ret
processkeyboard endp



;generate a random number using Oakenfull congruential generator
;x(n+1)=(a*x(n)+b) mod m, a=16333, b=25887, m=2^15, range between 0 and
;32767

random2 proc near
a=16333
b=25887
    push bp
    mov bp,sp
    push dx
    mov ax,randomnumber
    ;times a
    mov dx,a
    mul dx
    ;add b
    add ax,b ;no need for adc, bits will be lost anyway
    ;mask 15 first bits to perform mod 2^15
    and ax,7FFFh
    mov randomnumber,ax
    ;done
    pop dx
    pop bp
  	ret
random2 endp

generateKoekje proc near
  	;save dynamic link
  	push bp
  	;update base pointer
  	mov bp,sp
  	;save context
  	push ax
  	push bx
  	push dx

	call random2 ; call naar random2, resultaat in randomnumber
	mov ax,randomnumber
checkX:
	;x mag maximum 320 zijn 
	cmp ax,320
	jbe maaky
	mov bx,10
    cwd ;sign extend to DX:AX (32-bit)
    idiv word ptr bx  ;divide DX:AX by 10, result in AX, remainder in DX
    jmp checkX
maaky:
	mov ykoekje, ax ; sla resultaat op
	call random2 ; call naar random2, resultaat in randomnumber
	mov ax,randomnumber
checkY:
	;y mag maximum 200 zijn 
	cmp ax,200
	jbe generateKoekjeDone
	mov bx,10
    cwd ;sign extend to DX:AX (32-bit)
    idiv word ptr bx  ;divide DX:AX by 10, result in AX, remainder in DX
    jmp checkX			
generateKoekjeDone:
	mov ykoekje,ax;sla resultaat op
	
	;xkoekje afronden tot op tiental
	;   xkoekje delen door tien
	;   remainder van xkoekje aftrekken		
	mov ax, xkoekje
	mov bx,10
	cwd ;sign extend to DX:AX (32-bit)
    idiv word ptr bx  ;divide DX:AX 10, result in AX, remainder in DX
    sub xkoekje,dx  
    ;ykoekje afronden tot op tiental
	;   ykoekje delen door tien
	;   remainder van ykoekje aftrekken	
	mov ax, ykoekje
	mov bx,10
	cwd ;sign extend to DX:AX (32-bit)
    idiv word ptr bx  ;divide DX:AX 10, result in AX, remainder in DX
    sub ykoekje,dx    
	
	;restore context
  	pop dx
  	pop bx
  	pop ax
  	;restore bp
  	pop bp
  	;return, removing parameter
	ret
generateKoekje endp






checkVakje proc far
  	;save dynamic link
  	push bp
  	;update base pointer
  	mov bp,sp
  	;save context
  	push ax
  	push bx
  	push cx
  	push dx
  	push si
  	push di
  	push es
  	
  	mov ax,snakebodyarrayaantal2
  	;mov cx,offset snakebody;
  	;mov si, [snakebody] 
  	mov si,offset snakebody
controleerXCoordLichaam:
	sub ax, 4
	sub si, 4
	cmp ax, 0
	je  controleerXCoordkoekje ;--> de head van snake is niet tegen een ander lichaamsdeel gebotst, nu nog checken of er koekje is
	;sub ax, 4
	;sub si, 4
	mov bx,[si]
	cmp bx,xpos ;xpos is x-coord van kop van snake
	jne controleerXCoordLichaam;indien dit gelijk is, moet de y-coordinaat nog worden gecheckt.
	;hierna wordt y-coord gecheckt
	add ax,2
	add si,2
	mov bx,[si]
	cmp bx,ypos	
	;////////////////////////////////////////////////
	;je DEAD; Indien Y ook overeenkomt, is snake dood.  DEAD MOET NOG GEMAAKT WORDEN
	;////////////////////////////////////////////////
	sub ax,2; ax weer klaarmaken voor volgende check, als snake niet dood was.
	sub si,2
	jmp controleerXCoordLichaam

controleerXCoordkoekje:
	mov ax, xpos
	mov bx, xkoekje
	cmp ax,bx
	jne moveGedaan
	;x-coord van kop komt overeen met x-coord koekje, checken of ook y-coord overeen komen
	mov ax, ypos
	mov bx, ykoekje
	cmp ax,bx
	je eat; er staat een koekje, koekje opeten dus!
	jmp moveGedaan; er staat geen koekje, en ook geen lichaamsdeel. Er gebeurt dus niets speciaals  		

eat:
	;score verhogen
	add score,1
  	;lichaamlengte verlengen
  	add snakebodyaantal,1
  	add snakebodyarrayaantal,2 
  	add snakebodyarrayaantal2,4 
  
  	;extra lichaamsdeel toevoegen
  	
  	;mov si,offset snakebody
  	;mov ax, snakebodyarrayaantal2
  	;sub ax, 2
  	;add si, ax ;---> vakj voor nieuwe y
  	;mov si, 
  	
 	;mov si, [snakebody] 
	;mov ax, snakebodyarrayaantal
	;sub ax, 1 ; 
	;mov bx, ypos
	;mov [snakebody+ax], bx ; dit is de y-coordinaat van het nieuwe vakje		
	;sub ax, 1 ; 
	;mov bx, xpos
	;mov [snakebody+ax], bx ; dit is de x-coordinaat van het nieuwe vakje


genereerNieuwKoekje:	
	;////////////////////////////////////////////////
	;BEREKEN 2 NIEUWE RANDOM GETALLEN VOOR VOLGEND "KOEKJE"
	;   resultaat komt in xkoekje en ykoekje
    ; 	deze getallen liggen tussen 0 en 320 (X) en 0 en 200 (Y)	
	;////////////////////////////////////////////////
	call generateKoekje
	mov koekjeopgegeten, 0

moveGedaan:	
	cmp koekjeopgegeten, 1
	je genereerNieuwKoekje
  	;restore context
  	pop es
  	pop di
  	pop si
  	pop dx
  	pop cx
  	pop bx
  	pop ax
  	;restore bp
  	pop bp
  	;return, removing parameter
  	ret
checkVakje endp







updatestate proc near
  ;save dynamic link
  push bp
  ;update base pointer
  mov bp,sp
  ;save context
  push ax
  push bx
  push cx
  push dx
  push si
  push di
  push es
  
cmp keypressed, 0 
je updatestategedaan
mov keypressed, 0

  	cmp richting, 0 
  	jne checkrechts
  	sub ypos, 10
  	jmp richtingGeupdate  
checkrechts:
	cmp richting, 1 
  	jne checkonder
  	add xpos, 10
  	jmp richtingGeupdate  
checkonder:
	cmp richting, 2 
  	jne checklinks
  	add ypos, 10
  	jmp richtingGeupdate 
checklinks:
  	add ypos, 10
richtingGeupdate:
	call checkVakje
	 
    
; array ziet er intern als volgt uit:
; ---------------------------------------------------------
; | v1 x | v1 y  || v2 x | v2 y || v3 x | v3 y || .....
; ---------------------------------------------------------
; v= vakje
; v1 = vakje 1 = hoofd van snake
; v2 = vakje 2 = lichaamsdeel direct na het hoofd
; x= x-coördinaat van het vakje
; y= y-coördinaat van het vakje
updatearray:
		
		
		;mov cx, [snakebody];mov cx,offset snakebody;snakebodyaantal snakebody
		mov si,offset snakebody
		;mov bx, snakebodyaantal
		mov ax, snakebodyarrayaantal2
		sub ax, 2
		
; update de vorigex en y 
; vorigex/y = x en y van de staart van snake, dit is het deel dat moet weggetekend worden
updatevorigexy:
		;laatste vakje uit array = y coordinaat van staart
		mov si,offset snakebody;
		add si, ax
   		mov bx, [si] 
		mov yposprev, bx
		sub si,2
		sub ax,2
		;voorlaatste vakje uit array = y coordinaat van staart
   		mov bx, [si]
   	 	mov xposprev, bx

    	sub ax,2
    	;sub si,2
    	add ax,4
;verplaats alle elementen van de array
; alle vakjes schuiven 1 plaats naar achter
verplaatselementenarray:

		cmp ax, 2		
		je updatehead
		; y-coordinaat naar achter schuiven
		sub si,2
   		mov bx,[si]
   		add si,4
   		mov [si], bx
   		
   		; x-coordinaat naar achter schuiven
   		sub si,6
   		mov bx, [si]
   		add si,4
   		mov [si], bx
   		
   		sub ax,4
   		sub si,4 
		jmp verplaatselementenarray
updatehead: 
		;nieuwe waardes invullen in x en y van het hoofd van de slang
		add si,2	
    	mov bx, ypos
    	mov [si], bx
    	
    	mov bx, xpos
    	sub si, 2
    	mov [si], bx
    			
;////////////////////////////////////////////////////////////  		  
  
  ;erase old rectangle from backbuffer if xposprev != xpos || yposprev != ypos
;  mov ax,xposprev
;  cmp ax,xpos
;  je checkypos
;  jmp eraseit
;checkypos:
;  mov ax,yposprev
;  cmp ax,ypos
;  je drawit;//je updatearray;//je drawit
eraseit:
  ;set destination for rep stosb
  mov di,seg videobuffer
  mov es,di
  mov di,offset videobuffer
  ;adjust di for xposprev,yposprev
  ;find offset (put in ax)
  screenbufferoffset xposprev,yposprev		
  add di,ax
  mov al,rectcolor2 ;set color to erase
  mov dx,rectheight ;set heightcount
erase_vertical:
  mov cx,rectwidth
  rep stosb
  ;adjust di to point to next line
  add di,screenwidth-rectwidth
  dec dx
  jnz erase_vertical
  ;update xposprev,yposprev



drawit:  
  ;ok draw new rect
  ;rep movsb source in ds:si, destination in es:di
  mov si,offset rectangle
  mov di,seg videobuffer
  mov es,di
  mov di,offset videobuffer
  ;adjust di to account for new origin
  screenbufferoffset xpos,ypos
  add di,ax
  ;init ax for loop (height of rectangle)
  mov dx,rectheight
updatestate_vertical:
  mov cx,rectwidth
  rep movsb
  ;update di to point to next line
  add di,screenwidth-rectwidth
  dec dx
  jnz updatestate_vertical
 
;;;;;
tekenkoekje:
  cmp koekjeopgegeten,1
  je updatestategedaan
  ;ok draw new rect
  ;rep movsb source in ds:si, destination in es:di
  mov si,offset koekje
  mov di,seg videobuffer
  mov es,di
  mov di,offset videobuffer
  ;adjust di to account for new origin
  screenbufferoffset xkoekje,ykoekje
  add di,ax
  ;init ax for loop (height of rectangle)
  mov dx,rectheight
updatestate_vertical_koekje:
  mov cx,rectwidth
  rep movsb
  ;update di to point to next line
  add di,screenwidth-rectwidth
  dec dx
  jnz updatestate_vertical_koekje
    
updatestategedaan:
  ;restore context
  pop es
  pop di
  pop si
  pop dx
  pop cx
  pop bx
  pop ax
  ;restore bp
  pop bp
  ;return, removing parameter
  ret
updatestate endp

drawscreen proc near
  ;stack frame
  push bp
  mov bp,sp
  ;save context
  push ax
  push cx
  push dx
  push si
  push di
  push ds
  push es
  ;change registers for rep movsb to video ram
  mov dx,seg videobuffer
  mov ds,dx
  mov si,offset videobuffer
  mov dx,0A000h
  mov es,dx
  xor di,di
  ;wait for vblank to write to video ram
  mov dx, 03dah ; VGA status port             
drawscreen_wait1: ;if in vblank, wait until vblank finishes
  in al, dx
  and al, 8
  jnz drawscreen_wait1                 ; busy wait
drawscreen_wait2:                      ;wait until begin of vblank
  in al, dx
  and al, 8
  jz drawscreen_wait2                  ; and again
  mov cx,64000                         ;screen buffer is 64000 bytes long
  rep movsb ;blit
  ;restore context
  pop es
  pop ds
  pop di
  pop si
  pop dx
  pop cx
  pop ax
  ;restore stack frame
  pop bp
  ret
drawscreen endp

WACHTEN	PROC FAR				
		PUSH BX				
		PUSH CX	
		MOV CX,0FFFFH			
		MOV BL,50;80H	
M2:		mov CX,0FFFFH 			
M1:		DEC CX				
		JNZ M1				
		DEC BL				
		JNZ M2				
		POP CX				
		POP BX	
		
		RET	; Sprong terug naar het	
			
WACHTEN	ENDP		; hoofdprogramma		
			


end
