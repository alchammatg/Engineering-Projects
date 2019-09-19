; ISR_example.asm: a) Increments/decrements a BCD variable every half second using
; an ISR for timer 2; b) Generates a 2kHz square wave at pin P3.7 using
; an ISR for timer 0; and c) in the 'main' loop it displays the variable
; incremented/decremented using the ISR for timer 0 on the LCD.  Also resets it to 
; zero if the 'BOOT' pushbutton connected to P4.5 is pressed.
$NOLIST
$MODLP52
$LIST

CLK           EQU 22118400 ; Microcontroller system crystal frequency in Hz
BAUD 		  equ 115200
TIMER0_RATE   EQU 1000     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD EQU ((65536-(CLK/TIMER0_RATE)))
TIMER2_RATE   EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/TIMER2_RATE)))
T2LOAD 		  equ (0x10000-(CLK/(16*BAUD)))


CE_ADC    		EQU P2.0 
MY_MOSI   		EQU P2.1  
MY_MISO   		EQU P2.2 
MY_SCLK   		EQU P2.3 
SET_4_BUTTON 	equ p2.7
HOUR_BUTTON 	equ p2.3
AMPM_BUTTON 	equ p2.1
EXIT_BUTTON 	equ p0.0
SET_1_BUTTON 	equ p2.4
SET_2_BUTTON 	equ p2.5
SET_3_BUTTON 	equ p2.6
BOOT_BUTTON     equ P4.5
SOUND_OUT       equ P3.7
UPDOWN          equ P0.0
START_BUTTON    equ P2.4
STOP_BUTTON		equ P2.4 
ALARM_KILL		equ P2.5
OVEN			equ P0.4

; Reset vector
org 0000H
    ljmp main

; External interrupt 0 vector (not used in this code)
org 0003H
	reti

; Timer/Counter 0 overflow interrupt vector
org 000BH
	ljmp Timer0_ISR

; External interrupt 1 vector (not used in this code)
org 0013H
	reti

; Timer/Counter 1 overflow interrupt vector (not used in this code)
org 001BH
	reti

; Serial port receive/transmit interrupt vector (not used in this code)
org 0023H 
	reti
	
; Timer/Counter 2 overflow interrupt vector
org 002BH
	ljmp Timer2_ISR

dseg at 30h
Count1ms:     ds 2 
BCD_counter:   ds 1 
h: 			   ds 1
m: 			   ds 1
s: 			   ds 1
ampm: 		   ds 1

set_sec: 	   ds 1
set_min: 	   ds 1
set_hour: 	   ds 1

alarm_sec: 	   ds 1
alarm_min: 	   ds 1
alarm_hour:    ds 1

current_sec:   ds 1
current_min:   ds 1
current_hour:  ds 1

soak_temp: 	   ds 2
soak_time: 	   ds 1
ref_temp: 	   ds 2
ref_time:	   ds 1

sec: 		   ds 1
state: 		   ds 1
pwm: 		   ds 1
temp: 		   ds 2

var: 		   ds 1

PWMlooper60:   ds 4
PWMlooper45:   ds 4
onesec60:      ds 4
foursec60:	   ds 4
onesec45:	   ds 4
foursec45:	   ds 4

Result: ds 2
Result_2: ds 2
total: ds 4

bseg
seconds_flag: dbit 1 ; Set to one in the ISR every time 500 ms had passed
am_pm: dbit 1
sound_flag: dbit 1
cseg
; These 'equ' must match the wiring between the microcontroller and the LCD!
LCD_RS equ P1.4
LCD_RW equ P1.5
LCD_E  equ P1.6
LCD_D4 equ P3.2
LCD_D5 equ P3.3
LCD_D6 equ P3.4
LCD_D7 equ P3.5
$NOLIST
$include(LCD_4bit.inc) ; A library of LCD related functions and utility macros
$include(math32.inc)
$LIST

;                     	  1234567890123456    <- This helps determine the position of the counter
str_soak_temp: 		  db 'Soak temp', 0
str_soak_time: 		  db 'Soak time', 0
str_ref_time: 		  db 'Refl time', 0
str_ref_temp: 		  db 'Refl temp', 0
ready: 				  db 'Ready!              '
test: 				  db 'test                ', 0
blank: 				  db '                       ', 0
colon:				  db ':',0
state_machine_1:	  db 'STATEx SOAK TEMP',0
state_machine_2_c:	  db '0xxxC/0xxxC xxx%',0
state_machine_2_s:	  db '0xxs/0xxs       ',0
state_machine_2_co:	  db 'C/060C      ',0
letter_E:			  db 'E',0
cooldown:			  db 'Cooldown ',0
spaces:				  db '        ',0
Done:				  db '     Done!      ',0
Space:				  db ' ',0
One_hundred:		  db '100',0
   					  db  '\r', '\n', '.', 0
C_: 				  db 'C', 0
F_: 				  db 'F', 0
K_: 				  db 'K', 0
R_: 				  db 'R', 0
;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 0                     ;
;---------------------------------;
Timer0_Init:
	mov a, TMOD
	anl a, #0xf0 ; Clear the bits for timer 0
	orl a, #0x01 ; Configure timer 0 as 16-timer
	mov TMOD, a
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	; Enable the timer and interrupts
    setb ET0  ; Enable timer 0 interrupt
    setb TR0  ; Start timer 0
    setb EA   ; Enable Global interrupts
	ret

;---------------------------------;
; ISR for timer 0.  Set to execute;
; every 1/4096Hz to generate a    ;
; 2048 Hz square wave at pin P3.7 ;
;---------------------------------;
Timer0_ISR:
	; Define a latency correction for the timer reload
	CORRECTION EQU (4+4+2+2+4+4) ; lcall+ljmp+clr+mov+mov+setb
	; In mode 1 we need to reload the timer.
	clr TR0
	mov TH0, #high(TIMER0_RELOAD+CORRECTION)
	mov TL0, #low(TIMER0_RELOAD+CORRECTION)
	setb TR0
	
	jnb sound_flag, out_tr0
	cpl SOUND_OUT ; Connect speaker to P3.7!
	
out_tr0:
	push acc
	push psw
	
	; Increment the 16-bit counter
	inc Count1ms+0    ; Increment the low 8-bits first
	mov a, Count1ms+0 ; move current low 8 bits into accumulator
	jnz Inc_Done ; check if overflow (low 8-bits =00000000), if not overflow jump Inc_done
	inc Count1ms+1 ; increment high 8-bits

Inc_Done:
	; Check if second has passed
	mov a, Count1ms+0 ; move low 8-bits into accumulator
	cjne a, #low(1000), Timer2_ISR_done ;compare acuumulator and #low(value), if not = then jump to Timer2_ISR_done (1000 for 1 sec)
	mov a, Count1ms+1 ;mov high 8-bits into accumulator
	cjne a, #high(1000), Timer2_ISR_done ; if a not #low(value) jump to Timer2_ISR_done (1000 for 1 sec)
	
	; 1000 milliseconds have passed.  Set a flag so the main program knows
	setb seconds_flag ; Let the main program know one second had passed
	cpl TR1 ; This line makes a beep-silence-beep-silence sound
	; Reset the milli-seconds counter, it is a 16-bit variable
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Increment the BCD counter
	mov a, BCD_counter
	jnb UPDOWN, Timer2_ISR_decrement
	add a, #0x01
	sjmp Timer2_ISR_da
	
	
Timer2_ISR_decrement:
	add a, #0x99
Timer2_ISR_da:
Timer2_s:	
	da a
	mov BCD_counter, a

	mov a, s
	add a, #0x01
	da a
	mov s, a
	cjne a, #0x60, Timer2_ISR_done
	mov s, #0x00
	Timer2_m:
	mov a, m
	add a, #0x01
	da a
	mov m, a
	cjne a, #0x60, Timer2_ISR_done	
	mov m, #0x00
	Timer2_h:	
	mov a, h
	add a, #0x01
	da a
	mov h, a	
	cjne a, #0x12, Timer2_ISR_done	
	jb am_pm, loop8
	jnb am_pm, loop9
	loop8:
		clr am_pm
		ljmp Timer2_ISR_done
	loop9:
		setb am_pm

Timer2_ISR_done:
	mov a, h
	cjne a, #0x13, Timer2_ISR_done2
	mov h, #0x01
Timer2_ISR_done2:
	pop psw
	pop acc	
	reti

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 2                     ;
;---------------------------------;
Timer2_Init:
	mov T2CON, #0 ; Stop timer.  Autoreload mode.
	; One millisecond interrupt
	mov RCAP2H, #high(TIMER2_RELOAD)
	mov RCAP2L, #low(TIMER2_RELOAD)
	; Set the 16-bit variable Count1ms to zero
	clr a
	mov s, #0
	mov m, #0
	mov h, #6
	mov alarm_sec, #0x05
	mov alarm_min, #0x00
	mov alarm_hour, #0x01
	mov ampm, #0
	
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Enable the timer and interrupts
    setb ET2  ; Enable timer 2 interrupt
    setb TR2  ; Enable timer 2
    setb EA   ; Enable Global interrupts
	ret
;---------------------------------;
; ISR for timer 2                 ;
;---------------------------------;
Timer2_ISR:
	clr TF2  ; Timer 2 doesn't clear TF2 automatically in ISR
	reti
;---------------------------------;
; Main program. Includes hardware ;
; initialization and 'forever'    ;
; loop.                           ;
;---------------------------------;
main:
	; Initialization
    mov SP, #7FH
    mov PMOD, #0 ; Configure all ports in bidirectional mode
    lcall Timer0_Init
    lcall Timer2_Init
    lcall LCD_4BIT
    lcall INIT_SPI
	lcall InitSerialPort
    mov a, #0x00
    mov soak_temp+1, a
    mov a, #0x90
    mov soak_temp, a
    mov a, #0x00
    mov ref_temp+1, a
    mov a, #0x90
    mov ref_temp, a
    mov state, #0
    mov temp, #0
    mov temp+1,#0
    
    
	clr OVEN
	clr sound_flag

	Set_Cursor(1, 1)
    Send_Constant_String(#Ready)
    Set_Cursor(2, 1)
    Send_Constant_String(#Blank)
	mov BCD_counter, #0x00
	
loop:
	Set_Cursor(1, 1)
    Send_Constant_String(#Ready)
    Set_Cursor(2, 3)
    Send_Constant_String(#colon)
    Set_Cursor(2, 6)
    Send_Constant_String(#colon)
    Set_Cursor(2, 9)
    Send_Constant_String(#spaces)
	lcall display_time
	Wait_Milli_seconds(#250)
set_soak:
	lcall check_exit
	jb BOOT_BUTTON, set_ref  		; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)			; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb BOOT_BUTTON, set_ref  		; if the 'BOOT' button is not pressed skip
	jnb BOOT_BUTTON, $				; wait for release
	lcall display_time
	ljmp set_soak_temp
set_ref:
	jb SET_3_BUTTON, check_start  	; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)			; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb SET_3_BUTTON, check_start  	; if the 'BOOT' button is not pressed skip
	jnb SET_3_BUTTON, $				; wait for release
	ljmp set_ref_temp	
check_start:
	lcall state_machine	
kill_speaker:
	jb ALARM_KILL, loop_to_loop  	; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)			; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb ALARM_KILL,loop_to_loop   	; if the 'BOOT' button is not pressed skip
	jnb ALARM_KILL, $				; wait for release
	clr sound_flag
		
loop_to_loop:
	ljmp loop
	
set_soak_temp:
		lcall check_exit
		lcall bcd_soak_temp
		Set_cursor(1, 1)
		Send_Constant_String(#str_soak_temp)
		set_cursor(1,11)
		Display_BCD(bcd+1)
		set_cursor(1,13)
		Display_BCD(bcd)
		jb SET_1_BUTTON, set_soak_temp_2
		Wait_Milli_Seconds(#50)
		jb SET_1_BUTTON, set_soak_temp_2
		jnb SET_1_BUTTON, $
		set_cursor(1,11)
		mov a, soak_temp	
		add a, #0x01
		mov soak_temp, a
	set_soak_temp_2:
		jb SET_2_BUTTON, set_soak_time
		Wait_Milli_Seconds(#50)
		jb SET_2_BUTTON, set_soak_time
		jnb SET_2_BUTTON, $
		set_cursor(1,11)
		mov a, soak_temp
		add a, #0xA
		mov soak_temp, a
		ljmp set_soak_temp


set_soak_time:
		lcall check_exit
		set_cursor(2, 1)
		Send_Constant_String(#str_soak_time)
		set_cursor(2, 13)
		mov a, soak_time
		Display_BCD(a)
		jb SET_3_BUTTON, set_soak_time_2
		Wait_Milli_Seconds(#50)
		jb SET_3_BUTTON, set_soak_time_2
		jnb SET_3_BUTTON, $
		set_cursor(2, 13)
		mov a, soak_time
		add a, #0x01
		da a
		mov soak_time, a
	set_soak_time_2:
		jb SET_4_BUTTON, set_soak_temp_jump
		Wait_Milli_Seconds(#50)
		jb SET_4_BUTTON, set_soak_temp_jump
		jnb SET_4_BUTTON, $
		set_cursor(2, 11)
		mov a, soak_time
		add a, #0x10
		da a
		mov soak_time, a
		cjne a, #0x99, set_soak_temp_jump
		ljmp set_soak_time
set_soak_temp_jump:
	ljmp set_soak_temp
		
set_ref_temp:
		lcall check_exit
		lcall bcd_ref_temp
		Set_cursor(1, 1)
		Send_Constant_String(#str_ref_temp)
		set_cursor(1,11)
		Display_BCD(bcd+1)
		set_cursor(1,13)
		Display_BCD(bcd)
		jb SET_1_BUTTON, set_ref_temp_2
		Wait_Milli_Seconds(#50)
		jb SET_1_BUTTON, set_ref_temp_2
		jnb SET_1_BUTTON, $
		set_cursor(1,11)
		mov a, ref_temp	
		add a, #0x01
		mov ref_temp, a

	set_ref_temp_2:	
		jb SET_2_BUTTON, set_ref_time
		Wait_Milli_Seconds(#50)
		jb SET_2_BUTTON, set_ref_time
		jnb SET_2_BUTTON, $
		set_cursor(1,11)
		mov a, ref_temp
		add a, #0xA
		mov ref_temp, a
		ljmp set_ref_temp

set_ref_time:
		lcall check_exit
		set_cursor(2, 1)
		Send_Constant_String(#str_ref_time)
		set_cursor(2, 13)
		mov a, ref_time
		Display_BCD(a)
		jb SET_3_BUTTON, set_ref_time_2
		Wait_Milli_Seconds(#50)
		jb SET_3_BUTTON, set_ref_time_2
		jnb SET_3_BUTTON, $
		set_cursor(2, 13)
		mov a, ref_time
		add a, #0x01
		da a
		Display_BCD(a)
		mov ref_time, a
	set_ref_time_2:
		jb SET_4_BUTTON, set_ref_temp_jump
		Wait_Milli_Seconds(#50)
		jb SET_4_BUTTON, set_ref_temp_jump
		jnb SET_4_BUTTON, $
		set_cursor(2, 13)
		mov a, ref_time
		add a, #0x10
		da a
		Display_BCD(a)
		mov ref_time, a
		cjne a, #0x99, set_ref_temp_jump
		;mov soak_time, a
		ljmp set_ref_time		
set_ref_temp_jump:
	ljmp set_ref_temp
	
bcd_soak_temp:
	mov x+0, soak_temp+0	;stores lower byte of Result into 1st byte of x
    mov x+1, soak_temp+1	;stores upper byte of Result into 2nd byte of x
    mov x+2, #0			;stores 0 into 3rd byte of x
    mov x+3, #0			;stores 0 into 4th byte of x
    lcall hex2bcd
    ret
bcd_ref_temp:
	mov x+0, ref_temp+0
	mov x+1, ref_temp+1
	mov x+2, #0
	mov x+3, #0
	lcall hex2bcd
	ret
bcd_temp:
	mov x+0, temp+0
	mov x+1, temp+1
	mov x+2, #0
	mov x+3, #0
	lcall hex2bcd
	ret	
hex_temp:
	mov x+0, temp+0
	mov x+1, temp+1
	mov x+2, #0
	mov x+3, #0	
	lcall bcd2hex
	ret
display_time:
    clr seconds_flag 	; We clear this flag in the main loop, but it is set in the ISR for timer 0
	Set_Cursor(2, 7)    ; the place in the LCD where we want the BCD counter value
	Display_BCD(s) 		; This macro is also in 'LCD_4bit.inc'
	Set_Cursor(2, 4)    ; the place in the LCD where we want the BCD counter value
	Display_BCD(m) 		; This macro is also in 'LCD_4bit.inc'
	Set_Cursor(2, 1)    ; the place in the LCD where we want the BCD counter value
	Display_BCD(h) 		; This macro is also in 'LCD_4bit.inc'
	

	
	ret
	
	
; subroutines==================================================================================
check_exit:
	jb EXIT_BUTTON, return  ; if the 'EXIT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb EXIT_BUTTON, return  ; if the 'EXIT' button is not pressed skip
	jnb EXIT_BUTTON, $		; wait for release
	;if pressed continue
	ljmp loop
return:
	ret

; state machine================================================================================
state_machine:
state0:
	mov a, state
	cjne a, #0, jump_to_state_1
;	clr OVEN
;	lcall get_voltage
;	mov temp, bcd
;	mov temp+1, bcd+1
	
;	lcall bcd_soak_temp
;	clr c
;	mov a, bcd+1
;	subb a, temp+1
;	jc	go_tooo_state5
;;	sjmp dont_skip_10
;go_tooo_state5:	
;	ljmp state5
;dont_skip_10:		
;	clr c
;	lcall bcd_soak_temp
;	mov a, bcd
;	subb a, temp
;	jnc jump_to_state_1
;	ljmp state5
	
	

	sjmp dont_skip_2

jump_to_state_1:
	ljmp state1
	
dont_skip_2:		
	; CHECK FOR START_BUTTON
	jb START_BUTTON, return_from_sm
	Wait_Milli_Seconds(#50) 			; debounce time
	jb START_BUTTON, return_from_sm
	jnb START_BUTTON, $ 				; Wait for key release
	
	mov state, #1
	; Display state machine general message 
	Set_Cursor(1, 1)
	Send_Constant_String(#state_machine_1)
	; Display current state
	Set_Cursor(1,5)
	Display_BCD(state)
	; Overwrite 0 with E
	Set_Cursor(1, 5)
	Send_Constant_String(#Letter_E)
	; display units for state
	Set_Cursor(2, 1)
	Send_Constant_String(#state_machine_2_c)
	
	sjmp state1 
return_from_sm:
	ret

state1:
	mov a, state
	cjne a, #1, go_to_state_2
	
	sjmp dont_skip_55
	
go_to_state_2:	
	ljmp state2
	
dont_skip_55:	
	; CHECK FOR STOP_BUTTON
	jb STOP_BUTTON, continue_s1	
	Wait_Milli_Seconds(#50) 			; debounce time
	jb STOP_BUTTON, continue_s1	
	jnb STOP_BUTTON, $ 					; Wait for key release

	mov pwm, #0
	mov state, #0
	ret  		; LEAVE STATE MACHINE --------- RETURN ALSO TO STATE 0 ----------
	
continue_s1:
	setb OVEN
	lcall get_voltage
	mov temp, bcd
	mov temp+1, bcd+1
	
	; display current temp
	Set_Cursor(2, 1)    		
	Display_BCD(temp+1)
	; display lower 2 digits for current temp 
	Set_Cursor(2, 3)    	
	Display_BCD(temp)
	 
	lcall bcd_soak_temp
	; display target temp
	Set_Cursor(2, 7)   		    
	Display_BCD(bcd+1) 
	; display lower 2 digits for soak_temp 
	Set_Cursor(2, 9)   		    
	Display_BCD(bcd)
	 
	; Calculate percentage
	lcall hex_temp
	Load_y(100)
	lcall mul32
	
	mov y, soak_temp
	mov y+1,soak_temp+1
	mov y+2,#0
	mov y+3,#0
	
	lcall div32
	lcall hex2bcd
	; Display percentage
	Set_Cursor(2, 12)    		
	Display_BCD(bcd+1)
	; display lower 2 digits for current temp 
	Set_Cursor(2, 14)    	
	Display_BCD(bcd)
	; Overwrite 0 in percentage
	Set_Cursor(2, 12)    		
	Send_Constant_String(#Space)
	
	; adjust power and sec variable	
	setb OVEN
	mov sec, #0
	
	; check temp condition
	lcall bcd_soak_temp
	clr c
	mov a, bcd+1
	subb a , temp+1

	jc prepare_for_s2
	cjne a, #0, jump_to_state1
	
	clr c
	lcall bcd_soak_temp
	mov a, bcd
	subb a, temp

	jnc jump_to_state1
prepare_for_s2:	
	clr OVEN
	Set_Cursor(2, 13)    		
	Send_Constant_String(#One_hundred)

	; set state = 2
	mov state, #2
	setb sound_flag  ; Enable timer 0 interrupt
	sjmp state2
jump_to_state1:
	ljmp state1	
	
state2:
	mov a, state
	cjne a, #2, hello

	sjmp skipjump_1
	
hello:
		ljmp state3	
skipjump_1:	
	; CHECK FOR STOP_BUTTON
	jb STOP_BUTTON, pre_s2	
	Wait_Milli_Seconds(#50) ; debounce time
	jb STOP_BUTTON, pre_s2	
	jnb STOP_BUTTON, $ ; Wait for key release

	mov pwm, #0
	mov state, #0
	ret  		; LEAVE STATE MACHINE --------- RETURN ALSO TO STATE 0 ----------

pre_s2:

	mov a, soak_time
	cjne a, sec, continue_s2
	ljmp prepare_for_s3
	 
continue_s2:
	; Display State
	Set_Cursor(1,5)
	Display_BCD(state)
	; Overwrite 0 in State number
	Set_Cursor(1, 5)
	Send_Constant_String(#Letter_E)
	; display state type
	Set_Cursor(1, 8)    ; the place in the LCD where we want the BCD counter value
	Send_Constant_String(#str_soak_time)		
	; display correct units
	Set_Cursor(2, 1)
	Send_Constant_String(#state_machine_2_s)
	; display current time
	Set_Cursor(2,2)
	Display_BCD(sec)
	; display target time
	Set_Cursor(2,7)
	Display_BCD(soak_time)	
	
	; start time counter & power modulation
	setb OVEN
	Wait_Milli_Seconds(#200)
	clr OVEN	
	Wait_Milli_Seconds(#200)
	Wait_Milli_Seconds(#200)	
	Wait_Milli_Seconds(#200)
	Wait_Milli_Seconds(#200)
	;increment second counter
	mov a, sec
	add a, #0x01
	da a
	mov sec, a
	clr sound_flag  ; clear timer 0 interrupt
	ljmp state2
prepare_for_s3:	
	mov state, #3
	setb sound_flag  ; Enable timer 0 interrupt
	
state3:
	mov a, state
	cjne a, #3, go_to_state4

	sjmp dont_skip_4

go_to_state4:
	ljmp state4

dont_skip_4:
		; CHECK FOR STOP_BUTTON
	jb STOP_BUTTON, continue_s3	
	Wait_Milli_Seconds(#50) ; debounce time
	jb STOP_BUTTON, continue_s3	
	jnb STOP_BUTTON, $ ; Wait for key release

	mov pwm, #0
	mov state, #0
	ret  		; LEAVE STATE MACHINE --------- RETURN ALSO TO STATE 0 ----------
	
continue_s3:

	; Display current state
	Set_Cursor(1,5)
	Display_BCD(state)
	; Overwrite 0 with E
	Set_Cursor(1, 5)
	Send_Constant_String(#Letter_E)
	; display state type
	Set_Cursor(1, 8)    ; the place in the LCD where we want the BCD counter value
	Send_Constant_String(#str_ref_temp)
	; display units for state
	Set_Cursor(2, 5)
	display_char(#'C')
	Set_Cursor(2, 6)
	display_char(#'/')	
	Set_Cursor(2, 11)
	display_char(#'C')
	
	Wait_Milli_Seconds(#250)
	clr sound_flag
	
	lcall get_voltage
	mov temp, bcd
	mov temp+1, bcd+1
	
	; display current temp
	Set_Cursor(2, 1)    		
	Display_BCD(temp+1)
	; display lower 2 digits for current temp 
	Set_Cursor(2, 3)    	
	Display_BCD(temp)
	 
	lcall bcd_ref_temp
	; display target temp
	Set_Cursor(2, 7)   		    
	Display_BCD(bcd+1) 
	; display lower 2 digits for soak_temp 
	Set_Cursor(2, 9)   		    
	Display_BCD(bcd)
	 
	; Calculate percentage
	lcall hex_temp
	Load_y(100)
	lcall mul32
	
	mov y, ref_temp
	mov y+1,ref_temp+1
	mov y+2,#0
	mov y+3,#0
	
	lcall div32
	lcall hex2bcd	
	; Display percentage
	Set_Cursor(2, 12)    		
	Display_BCD(bcd+1)
	; display lower 2 digits for current temp 
	Set_Cursor(2, 14)    	
	Display_BCD(bcd)
	; Overwrite 0 in percentage
	Set_Cursor(2, 12)    		
	Send_Constant_String(#Space)
	
	; adjust power and sec variable	
	setb OVEN
	mov sec, #0
	; check temp condition
	lcall bcd_soak_temp
	clr c
	mov a, bcd+1
	subb a , temp+1

	jc prepare_for_s4
	cjne a, #0, jump_to_state3
	
	clr c
	lcall bcd_ref_temp
	mov a, bcd
	subb a, temp

	jnc jump_to_state3
	
prepare_for_s4:	

	clr OVEN
	Set_Cursor(2, 13)    		
	Send_Constant_String(#One_hundred)

	; set state = 4
	mov state, #4
	setb sound_flag  ; Enable timer 0 interrupt
	sjmp state4
jump_to_state3:
	ljmp state3
	
	
state4:
	mov a, state
	cjne a, #4, hello1

	sjmp skipjump_2
	
hello1:
		ljmp state5
skipjump_2:	
	; CHECK FOR STOP_BUTTON
	jb STOP_BUTTON, pre_s4	
	Wait_Milli_Seconds(#50) ; debounce time
	jb STOP_BUTTON, pre_s4	
	jnb STOP_BUTTON, $ ; Wait for key release

	mov pwm, #0
	mov state, #0
	ret  		; LEAVE STATE MACHINE --------- RETURN ALSO TO STATE 0 ----------

pre_s4:

	mov a, ref_time
	cjne a, sec, continue_s4
	ljmp prepare_for_s5
	 
continue_s4:
	; Display State
	Set_Cursor(1,5)
	Display_BCD(state)
	; Overwrite 0 in State number
	Set_Cursor(1, 5)
	Send_Constant_String(#Letter_E)
	; display state type
	Set_Cursor(1, 8)    ; the place in the LCD where we want the BCD counter value
	Send_Constant_String(#str_ref_time)		
	; display correct units
	Set_Cursor(2, 1)
	Send_Constant_String(#state_machine_2_s)
	; display current time
	Set_Cursor(2,2)
	Display_BCD(sec)
	; display target time
	Set_Cursor(2,7)
	Display_BCD(ref_time)	
	
	; start time counter & power modulation
	setb OVEN
	Wait_Milli_Seconds(#200)
	clr OVEN	
	Wait_Milli_Seconds(#200)
	Wait_Milli_Seconds(#200)	
	Wait_Milli_Seconds(#200)
	Wait_Milli_Seconds(#200)
	;increment second counter
	mov a, sec
	add a, #0x01
	da a
	mov sec, a
	clr sound_flag  ; clear timer 0 interrupt
	ljmp state4
prepare_for_s5:	
	mov state, #5
	setb sound_flag  ; Enable timer 0 interrupt
	
state5:
	mov a, state
	cjne a, #5, jumpto0

	sjmp dontjump
	
	jumpto0:
	ljmp state0
	
	dontjump:
		; CHECK FOR STOP_BUTTON
	jb STOP_BUTTON, continue_s5	
	Wait_Milli_Seconds(#50) ; debounce time
	jb STOP_BUTTON, continue_s5
	jnb STOP_BUTTON, $ ; Wait for key release

	mov pwm, #0
	mov state, #0
	ret  		; LEAVE STATE MACHINE --------- RETURN ALSO TO STATE 0 ----------
	
continue_s5:
	; display current state
	Set_Cursor(1,5)
	Display_BCD(state)
	; overwrite 0 in state with Letter E
	Set_Cursor(1, 5)
	Send_Constant_String(#Letter_E)
	; display state mode
	Set_Cursor(1, 8)    
	Send_Constant_String(#cooldown)		
	; display correct units
	Set_Cursor(2, 5)
	Send_Constant_String(#state_machine_2_co)
	
	; get current temperature
	lcall get_voltage
	mov temp, bcd
	mov temp+1, bcd+1
	
	; display current temp
	Set_Cursor(2, 1)    		
	Display_BCD(bcd+1)
	; display lower 2 digits for current temp 
	Set_Cursor(2, 3)    	
	Display_BCD(bcd)
	
	; Adjust power
	clr OVEN
	;check temp condition
;	mov a,temp
;	clr c
;	subb a,#60
	lcall hex_temp 		; bcd2hex temp!
	mov a, x+1
	cjne a,#0,go_to_state_5
	clr c
	mov a,x
	subb a,#27			; cooldown temp
	
	clr sound_flag  ; clear timer 0 interrupt	
	jnc go_to_state_5
	mov state,#0
	setb sound_flag
	ret
go_to_state_5:
	ljmp state5

InitSerialPort:
	clr TR2 ; Disable timer 2
	mov T2CON, #30H ; RCLK=1, TCLK=1 
	mov RCAP2H, #high(T2LOAD)  
	mov RCAP2L, #low(T2LOAD)
	setb TR2 ; Enable timer 2
	mov SCON, #52H
	ret
putchar:
    JNB TI, putchar
    CLR TI
    MOV SBUF, a
    RET
; Send a constant-zero-terminated string through the serial port
SendResult:
    mov x+0, Result+0	;stores lower byte of Result into 1st byte of x
    mov x+1, Result+1	;stores upper byte of Result into 2nd byte of x
    mov x+2, #0			;stores 0 into 3rd byte of x
    mov x+3, #0			;stores 0 into 4th byte of x
    Load_Y (3149)
	lcall mul32
	Load_Y (10000)
	lcall div32
	;mov total, x
	mov total+0, x+0	;stores lower byte of Result into 1st byte of x
    mov total+1, x+1	;stores upper byte of Result into 2nd byte of x
    mov total+2, #0			;stores 0 into 3rd byte of x
    mov total+3, #0			;stores 0 into 4th byte of x
	mov x+0, Result_2+0	;stores lower byte of Result into 1st byte of x
    mov x+1, Result_2+1	;stores upper byte of Result into 2nd byte of x
    mov x+2, #0			;stores 0 into 3rd byte of x
    mov x+3, #0			;stores 0 into 4th byte of x
   ;adjust the ADC output to a readable voltage Vout
   	load_y(500)			;make y 500 (5.0Vx100)
   	lcall mul32			;multiply by y
   	load_y(1023)		;load y by 1023 (2^10-1)
   	lcall div32			;divide by y
   	load_y(273)
 	lcall sub32
    
    mov y+0, total+0
    mov y+1, total+1
    mov y+2, #0
    mov y+3, #0
    
    ;load_y(total)
    ;lcall add32

   
	;mov y+0, Result_2+0	;stores lower byte of Result into 1st byte of x
    ;mov y+1, Result_2+1	;stores upper byte of Result into 2nd byte of x
    ;mov y+2, #0			;stores 0 into 3rd byte of x
    ;mov y+3, #0			;stores 0 into 4th byte of x
	lcall add32
   
    lcall hex2bcd	;convert x from 32 bit hex to 10 digit BCD
 	
    mov a, bcd+1
    swap a
    anl a, #0x0f
    orl a, #0x30
    
    lcall putchar

    mov a, bcd+1
    anl a, #0x0f
    orl a, #0x30
    lcall putchar
	    
    mov a, bcd+0
    swap a			;swaps the upper and lower nibbles of a, 	
    anl a, #0x0f	;ands a with 0x0fH (0x00001111B), makes a binary coded hex
    orl a, #0x30	; ors a with 30... ie adds 30 to value of a, makes it ASCII

    lcall putchar 	; sends the 4bits of binary coded ASCII to putty

    mov a, bcd+0
    anl a, #0x0f
    orl a, #0x30
    lcall putchar

    ; Send new line / carriage return
    mov a, #'\r'
    lcall putchar
    mov a, #'\n'
    lcall putchar
    Wait_Milli_Seconds(#1)
    ret	
SendString:
    CLR A
    MOVC A, @A+DPTR
    JZ SendStringDone
    LCALL putchar
    INC DPTR
    SJMP SendString
SendStringDone:
    ret
INIT_SPI: 
	setb MY_MISO    ; Make MISO an input pin
	clr MY_SCLK     ; For mode (0,0) SCLK is zero
	ret	
DO_SPI_G: 
	push acc 
	mov R1, #0      ; Received byte stored in R1
	mov R2, #8			; Loop counter (8-bits)
DO_SPI_G_LOOP: 
	mov a, R0       ; Byte to write is in R0
	rlc a           ; Carry flag has bit to write
	mov R0, a 
	mov MY_MOSI, c 
	setb MY_SCLK    ; Transmit
	mov c, MY_MISO  ; Read received bit
	mov a, R1       ; Save received bit in R1
	rlc a 
	mov R1, a 
	clr MY_SCLK 
	djnz R2, DO_SPI_G_LOOP 
	pop acc
	ret 
get_voltage:
	clr CE_ADC
	mov R0, #00000001B	;Start bit: 1	
	lcall Do_SPI_G
	
	mov R0, #10000000B	;Single ended, read channel 0
	lcall DO_SPI_G
	mov a, R1			;R1 contains bits 8 and 9
	anl a, #00000011B	;We need only the two least significant bits
	mov Result+1, a
	
	mov R0, #55H		;It doesn't matter what we transmit...
	lcall DO_SPI_G
	mov Result, R1		;R1 contains bits 0 to 7. Save result low.
	setb CE_ADC
	;wait .1 second
	Wait_Milli_Seconds(#250)

	lcall get_lm335
	lcall SendResult

	ret
get_lm335:
	clr CE_ADC
	mov R0, #00000001B	;Start bit: 1	
	lcall Do_SPI_G
	
	mov R0, #10010000B	;Single ended, read channel 0
	lcall DO_SPI_G
	mov a, R1			;R1 contains bits 8 and 9
	anl a, #00000011B	;We need only the two least significant bits
	mov Result_2+1, a
	
	mov R0, #55H		;It doesn't matter what we transmit...
	lcall DO_SPI_G
	mov Result_2, R1		;R1 contains bits 0 to 7. Save result low.
	setb CE_ADC
	;wait .1 second
	Wait_Milli_Seconds(#250)

	ret	
	
end_of_time:
    SJMP end_of_time				
END
