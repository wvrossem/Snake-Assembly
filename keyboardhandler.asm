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