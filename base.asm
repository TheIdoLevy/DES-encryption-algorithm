IDEAL
MODEL small
STACK 100h
DATASEG

; -------------  First key permutation vars
pc1_q1 db 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15 ; pc1 split into 4 blocks of 8 bits
pc1_q2 db 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15
pc1_q3 db 15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0
pc1_q4 db 15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0

key_q1 dw 1111111111111111b ; key split into blocks of 8 bits
key_q2 dw 1111111111111111b
key_q3 dw 1111111100000000b
key_q4 dw 1000000011111111b

new_key_q1 dw 0 ; new key split into blocks of 8 bits
new_key_q2 dw 0
new_key_q3 dw 0
new_key_q4 dw 0

mask1 dw 000000001b

current_pc1_quarter_adr dw 0 ; address of current pc_1 quarter
current_key_quarter_adr dw 0
current_new_key_quarter_adr dw 0
; -------------

; -------------- Shl table to create 16 CnDc pairs

num_shifts db 1,1,2,2,2,2,2,2,1,2,2,2,2,2,2,1
CnDn_pairs db 102 dup (?)

margin dw 34 dup (0)
carry_set_shl_2 db 0 ; This var tracks the need to call next proc for or-ing with 1
carry_set_shl_3 db 0
carry_set_shl_4 db 0
;---------------


;-------------- Get data and first permutation
margin3 dw 3 dup(?)
data_q1 dw 1111111111111111b
data_q2 dw 0000000011111111b
data_q3 dw 1111111100000000b
data_q4 dw 1010101010101010b
margin4 db 5 dup(?)
new_data_q1 dw 0
new_data_q2 dw 0
new_data_q3 dw 0
new_data_q4 dw 0
;---------------

;-------------- Expand ln and rn to 48 bits
expansion_selection_table db 15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0
current_data_half dw 0
current_exp_half_adr dw 0
;--------------


;-------------- Round function vars
s1_b1_mask dw 1000010000000000b ; get the value of x0000x... in order to decide which row of s1 to access
s1_b1_col_mask dw 0111100000000000b ; get the value of 0xxxx0... in order to decide which column to access

s1_q1 db 14,0,2,13,9,6,8,7,15,4,9,3,1,11,12,10,5 ; S1 table to rows
s1_q2 db 14,0,2,13,9,6,8,7,15,4,9,3,1,11,12,10,5
s1_q3 db 14,0,2,13,9,6,8,7,15,4,9,3,1,11,12,10,5
s1_q4 db 14,0,2,13,9,6,8,7,15,4,9,3,1,11,12,10,5

margin5 dd 0

newRn db 8 dup (0) ; This array is s1(B1)s1(B2)...s1(B8)
newRn_as_num_h1 dw 0 ; s1(b1)...s1(b8) array turned into number
newRn_as_num_h2 dw 0

Ln_h1_tmp dw 0 ; This var will be used to hold Ln-1
Ln_h2_tmp dw 0

Ln_h1 dw 0 
Ln_h2 dw 0

Rn_h1_tmp dw 0 ; This var will be used to hold Rn-1
Rn_h2_tmp dw 0
rn_expansion dw 0 ; Only expand right half

Rn_h1 dw 0
Rn_h2 dw 0

CODESEG

proc permute_key_pc1
mov cx, 16
mov di,0
looper:
push cx
mov cl,15
mov ch, [byte ptr bx+di] ; bit number from pc1
sub cl,ch
shl [mask1], cl ; shl to bit number 15 - [ch] in order to get the bit
mov ax, [mask1]
mov si, [current_key_quarter_adr]
and ax, [si] ; get the bit from index in pc1
mov [mask1],ax
shr [mask1],cl ; return bit to position 0
mov cx,15
sub cx,di ; get the index to put the new_bit in new_key
shl [mask1], cl
mov ax, [mask1]
mov si, [current_new_key_quarter_adr]
or [si], ax ; put the bit in place
mov [mask1], 1
inc di
pop cx
dec cx
cmp cx,0
jne looper
ret
endp permute_key_pc1

proc create_Cn_Dn_pairs_shifts
;-------------------- Pemute keys to get CnDn pairs
mov cx, 16
mov bx, offset num_shifts
mov si, 0
mov di, 0
looper2:
push cx
mov cl, [byte ptr bx + di]

mov ax, [new_key_q4]
and ax, 0FF00h
shl ax, cl
mov [new_key_q4], ax
jc add_1_to_tail_1 ; if carry out from shl of rightermost byte, or with 1 to last bit of next byte
jmp shift_no_or_1

add_1_to_tail_1:
mov ax, [new_key_q3]
shl ax,cl ; shift key quarter left by 1
jc set_carry_out_var_1 ; if carry, go to label and set variable that deides if to go to the label that
jmp continue1 ; adds a one to the end of the next byte
set_carry_out_var_1:
mov [carry_set_shl_2], 1
continue1:
or ax, 1 ; or last bit because there was a carry on the last shift
mov [new_key_q3], ax
cmp [carry_set_shl_2], 1 ; if carry on last shift, go to label that or's last bit of next byte
je add_1_to_tail_2
jmp shift_no_or_2

add_1_to_tail_2:
mov ax, [new_key_q2]
shl ax,cl
jc set_carry_out_var_2
jmp continue2
set_carry_out_var_2:
mov [carry_set_shl_3], 1
continue2:
or ax, 1
mov [new_key_q2], ax
cmp [carry_set_shl_3], 1
je add_1_to_tail_3
jmp shift_no_or_3

add_1_to_tail_3:
mov ax, [new_key_q1]
shl ax, cl
jc set_carry_out_var_3
jmp continue3
set_carry_out_var_3:
mov [carry_set_shl_4],1
continue3:
or ax, 1
mov [new_key_q1], ax
cmp [carry_set_shl_4], 1
je or_rightmost_bit ; if shift of last byte gave an overflow, turn on last bit of 7th byte
jmp end_key_shifts


avoid_far_jump_1:
jmp looper2

or_rightmost_bit:
or [new_key_q4], 100000000b
jmp end_key_shifts

shift_no_or_1:
shl [new_key_q3], cl
jc set_carry_out_var_3
jmp shift_no_or_2

shift_no_or_2:
shl [new_key_q2], cl
jc add_1_to_tail_1
jmp shift_no_or_3

shift_no_or_3:
shl [new_key_q1], cl
jc or_rightmost_bit

end_key_shifts:
push bx
mov bx, offset CnDn_pairs
mov al, [byte ptr new_key_q1]
mov [byte ptr bx + si], al
inc si
mov al, [byte ptr new_key_q1+1]
mov [byte ptr bx + si], al
inc si
mov al, [byte ptr new_key_q2]
mov [byte ptr bx + si], al
inc si
mov al, [byte ptr new_key_q2+1]
mov [byte ptr bx + si], al
inc si
mov al, [byte ptr new_key_q3]
mov [byte ptr bx + si], al
inc si
mov al, [byte ptr new_key_q3+1]
mov [byte ptr bx + si], al
inc si
mov al, [byte ptr new_key_q4]
mov [byte ptr bx + si], al
inc si
inc di
pop bx
pop cx
dec cx
cmp cx, 0
jne avoid_far_jump_1
ret
endp create_Cn_Dn_pairs_shifts


proc expand_ln_rn
mov cx, 16
mov si, 0
mov bx, offset expansion_selection_table
looper4:
push cx
mov cl,15
mov ch, [byte ptr bx+si]
sub cl, ch
shl [mask1], cl
mov ax, [current_data_half]
and [mask1], ax ; take bit value from index of table from dataq2 or data q4
shr [mask1], cl
mov cx,15
sub cx,si
push bx
mov bx, [current_exp_half_adr]
shl [mask1], cl
mov ax, [word ptr bx]
or ax, [mask1] ; change bit in index si according to table
mov [word ptr bx], ax
mov [mask1], 1
pop bx
inc si
pop cx
dec cx
cmp cx,0
jne looper4
ret
endp expand_ln_rn


proc s1b1_s1b8_calculator
mov ax, [s1_b1_mask]  ; The mask is 100001... checks wich row to go to in s1 table
and ax, [Rn_h1_tmp]

cmp ax, 0
je s1_table_row0
cmp ax, 0000010000000000b
je s1_table_row1
cmp ax, 1000000000000000b
je s1_table_row2
cmp ax, 1000010000000000b
je s1_table_row3

s1_table_row0:
mov ax, [Rn_h1_tmp]
and ax, [s1_b1_col_mask]
shr ax, 11d
mov di, ax
mov bx, offset s1_q1
mov al, [byte ptr bx+di]
 
s1_table_row1:
mov ax, [Rn_h1_tmp]
and ax, [s1_b1_col_mask]
shr ax, 11d
mov di, ax
mov bx, offset s1_q2
mov al, [byte ptr bx+di]

s1_table_row2:
mov ax, [Rn_h1_tmp]
and ax, [s1_b1_col_mask]
shr ax, 11d
mov di, ax
mov bx, offset s1_q3
mov al, [byte ptr bx+di]

s1_table_row3:
mov ax, [Rn_h1_tmp]
and ax, [s1_b1_col_mask]
shr ax, 11d
mov di, ax
mov bx, offset s1_q4
mov al, [byte ptr bx+di]
ret
endp s1b1_s1b8_calculator

proc calc_s1b1_s1b8_for_rn
mov cx, 7
looper9:
push cx
mov ax, [rn_expansion]
and ax, 1111110000000000b
shl [rn_expansion], 6
mov bx, ax
shr bx, 10d
mov ax, [Rn_h2_tmp]
and ax, 1111110000000000b
shr ax, 10d
shl [Rn_h2_tmp], 6
or [Rn_h2_tmp], bx
shl [Rn_h1_tmp], 6
jmp continue4

avoid_far_jump_3:
jmp looper9

continue4:
or [Rn_h1_tmp], ax
call s1b1_s1b8_calculator
pop cx
;mov ah,0
;shl ax, cl
mov bx, offset newRn
mov di, 8
sub di, cx
mov [byte ptr bx+di], al
;or [newRn], ax
dec cx
cmp cx, 0
jne avoid_far_jump_3
ret
endp calc_s1b1_s1b8_for_rn



proc main_function

ret
endp main_function



start:
mov ax, @data
mov ds, ax


;------ Permute initial key using pc1 table
mov bx, offset key_q1
mov [current_key_quarter_adr], bx
mov bx, offset new_key_q1
mov [current_new_key_quarter_adr], bx
mov bx, offset pc1_q1
call permute_key_pc1

mov bx, offset key_q2
mov [current_key_quarter_adr], bx
mov bx, offset new_key_q2
mov [current_new_key_quarter_adr], bx
mov bx, offset pc1_q2
call permute_key_pc1

mov bx, offset key_q3
mov [current_key_quarter_adr], bx
mov bx, offset new_key_q3
mov [current_new_key_quarter_adr], bx
mov bx, offset pc1_q3
call permute_key_pc1

mov bx, offset key_q4
mov [current_key_quarter_adr], bx
mov bx, offset new_key_q4
mov [current_new_key_quarter_adr], bx
mov bx, offset pc1_q4
call permute_key_pc1
;--------------------


call create_Cn_Dn_pairs_shifts


;-------------------- Initial permutaion of data
mov bx, offset data_q1
mov [current_key_quarter_adr], bx
mov bx, offset new_data_q1
mov [current_new_key_quarter_adr], bx
mov bx, offset pc1_q1
call permute_key_pc1

mov bx, offset data_q2
mov [current_key_quarter_adr], bx
mov bx, offset new_data_q2
mov [current_new_key_quarter_adr], bx
mov bx, offset pc1_q2
call permute_key_pc1

mov bx, offset data_q3
mov [current_key_quarter_adr], bx
mov bx, offset new_data_q3
mov [current_new_key_quarter_adr], bx
mov bx, offset pc1_q3
call permute_key_pc1

mov bx, offset data_q4
mov [current_key_quarter_adr], bx
mov bx, offset new_data_q4
mov [current_new_key_quarter_adr], bx
mov bx, offset pc1_q4
call permute_key_pc1
;-----------------------


;----------------- Save L0 and R0 
mov ax, [new_data_q1]
mov [Ln_h1], ax

mov ax, [new_data_q2]
mov [Ln_h2], ax

mov ax, [new_data_q3]
mov [Rn_h1], ax

mov ax, [new_data_q4]
mov [Rn_h2], ax
;-----------------


mov cx, 16
main_loop:
	;---------------- The 16 round function
	mov ax, [Ln_h1]
	mov [Ln_h1_tmp], ax

	mov ax, [Ln_h2]
	mov [Ln_h2_tmp], ax

	mov ax, [Rn_h1]
	mov [Rn_h1_tmp], ax

	mov ax, [Rn_h2_tmp]
	mov [Rn_h2_tmp], ax

	;---------------- Ln = Rn-1
	mov ax, [Rn_h1_tmp]
	mov [Ln_h1], ax

	mov ax, [Rn_h2_tmp]
	mov [Ln_h2], ax
	;----------------

	;----------------------- Expand Rn-1 to 48 bits
	mov bx, [Rn_h2_tmp]
	mov [current_data_half], bx
	mov bx, offset rn_expansion ; expand right half
	mov [current_exp_half_adr], bx
	call expand_ln_rn
	;------------------------

	mov bx, offset CnDn_pairs  ;xor data right half with key no. (round no.)
	mov ax, [word ptr bx]
	xor [Rn_h1_tmp], ax
	mov ax, [word ptr bx+2]
	xor [Rn_h2_tmp], ax
	mov ax, [word ptr bx+4]
	xor [rn_expansion], ax

	call s1b1_s1b8_calculator ; Calculate the s1(B1)
	mov bx, offset newRn
	mov [byte ptr bx], al

	call calc_s1b1_s1b8_for_rn

	mov bx, offset newRn
	mov cx, 8
	mov di, 0
	mov si, 3
	turn_newRn_arr_to_num_h1:  ; S1(b1)...S1(b8) is an array - turn into a number
	push cx
		mov bp, [word ptr bx+di]
		and bp, 0FFh
		mov ax, 4
		mul si				  ; Shl si*ax = si*4 times
		mov cx, ax
		shl bp, cl
		pop cx
		or [newRn_as_num_h1], bp
		inc di
		dec si
		dec cx
		cmp cx, 4
	jne turn_newRn_arr_to_num_h1

	mov si, 3
	turn_newRn_arr_to_num_h2: ; Need two loops because newRn is 32 bits in size
		push cx
		mov bp, [word ptr bx+di]
		and bp, 0FFh
		mov ax, 4
		mul si				; Shl si*ax = si*4 times
		mov cx, ax
		shl bp, cl
		pop cx
		or [newRn_as_num_h2], bp
		inc di
		dec si
		dec cx
		cmp cx, 0
		jne turn_newRn_arr_to_num_h2
		jmp continue_main
	;----------------
	avoid_far_jump_main:
		jmp main_loop
	
	continue_main:
	;---------------- Rn = Ln-1 + f(Rn-1, xn)
	mov ax, [Ln_h1_tmp]
	mov [Rn_h1], ax

	mov ax, [Ln_h2_tmp]
	mov [Rn_h2], ax

	mov ax, [newRn_as_num_h1]
	xor [Rn_h1], ax

	mov ax, [newRn_as_num_h2]
	xor [Rn_h2], ax
	;----------------
	pop cx
	dec cx
	cmp cx, 0
	jne avoid_far_jump_main
		
exit:
mov ax,4c00h
int 21h
END start