;generate a random number using Oakenfull congruential generator
;x(n+1)=(a*x(n)+b) mod m, a=16333, b=25887, m=2^15, range between 0 and 32767

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

generate_koekje proc near
generate_koekje_x
	call random2 ; call naar random2, resultaat in randomnumber
	mov ax,randomnumber
	testen_x
	cmp ax,320 ;het huidige randomnumber (in ax) vergelijken met de maximale x-waarde = 320
	jbe generate_koekje_y ;jumb if below or equal, als dit zo is dan hebben we een x
	idiv 10 ;delen door 10, geheel getal in AL met rest in AH
	mov ax,al ;geheel getal terug in ax zodat we weer kunnen testen of het getal nu niet te groot is
	jmp testen
generate_koekje_y
	mov xkoekje,ax
	call random2
	mov ax,randomnumber
	testen_y
	cmp ax,200
	jbe generate_koekje_done
	idiv 10
	mov ax,al
	jmp testen_y
generate_koekje_done
	mov ykoekje,ax
	ret
generate_koekje endp
	
	