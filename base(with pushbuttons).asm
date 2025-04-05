; ISR_example.asm: a) Increments/decrements a BCD variable every half second using
; an ISR for timer 2; b) Generates a 2kHz square wave at pin P1.7 using
; an ISR for timer 0; and c) in the 'main' loop it displays the variable
; incremented/decremented using the ISR for timer 2 on the LCD.  Also resets it to 
; zero if the 'CLEAR' push button connected to P1.5 is pressed.
$NOLIST
$MODN76E003
$LIST

;  N76E003 pinout:
;                               -------
;       PWM2/IC6/T0/AIN4/P0.5 -|1    20|- P0.4/AIN5/STADC/PWM3/IC3
;               TXD/AIN3/P0.6 -|2    19|- P0.3/PWM5/IC5/AIN6
;               RXD/AIN2/P0.7 -|3    18|- P0.2/ICPCK/OCDCK/RXD_1/[SCL]
;                    RST/P2.0 -|4    17|- P0.1/PWM4/IC4/MISO
;        INT0/OSCIN/AIN1/P3.0 -|5    16|- P0.0/PWM3/IC3/MOSI/T1
;              INT1/AIN0/P1.7 -|6    15|- P1.0/PWM2/IC2/SPCLK
;                         GND -|7    14|- P1.1/PWM1/IC1/AIN7/CLO
;[SDA]/TXD_1/ICPDA/OCDDA/P1.6 -|8    13|- P1.2/PWM0/IC0
;                         VDD -|9    12|- P1.3/SCL/[STADC]
;            PWM5/IC7/SS/P1.5 -|10   11|- P1.4/SDA/FB/PWM1
;                               -------
;

CLK           EQU 16600000 ; Microcontroller system frequency in Hz
TIMER0_RATE   EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD EQU ((65536-(CLK/TIMER0_RATE)))
TIMER2_RATE   EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/TIMER2_RATE)))
BAUD              EQU 115200 ; Baud rate of UART in bps
TIMER1_RELOAD     EQU (0x100-(CLK/(16*BAUD)))
TIMER0_RELOAD_1MS EQU (0x10000-(CLK/1000))

START_BUTTON  equ P1.5
SOUND_OUT     equ P1.7

; Reset vector
org 0x0000
    ljmp main

; External interrupt 0 vector (not used in this code)
org 0x0003
	reti

; Timer/Counter 0 overflow interrupt vector
org 0x000B
	ljmp Timer0_ISR

; External interrupt 1 vector (not used in this code)
org 0x0013
	reti

; Timer/Counter 1 overflow interrupt vector (not used in this code)
org 0x001B
	reti

; Serial port receive/transmit interrupt vector (not used in this code)
org 0x0023 
	reti
	
; Timer/Counter 2 overflow interrupt vector
org 0x002B
	ljmp Timer2_ISR

; In the 8051 we can define direct access variables starting at location 0x30 up to location 0x7F
dseg at 0x30
Count1ms:     ds 2 ; Used to determine when half second has passed
BCD_counter:  ds 1 ; The BCD counter incrememted in the ISR and displayed in the main loop
sec_threshold: ds 1 ;
state: ds 1
sec : ds 1
sec1: ds 1
current_temp : ds 1
Temp_soak: ds 1
Time_soak: ds 1
Temp_refl: ds 1
Time_refl: ds 1
 
; In the 8051 we have variables that are 1-bit in size.  We can use the setb, clr, jb, and jnb
; instructions with these variables.  This is how you define a 1-bit variable:
bseg
half_seconds_flag: dbit 1 ; Set to one in the ISR every time 500 ms had passed

PB0: dbit 1  ; Pushbutton 0 
PB1: dbit 1  ; Pushbutton 1 
PB2: dbit 1  ; Pushbutton 2 
PB3: dbit 1  ; Pushbutton 3 
PB4: dbit 1  ; Pushbutton 4

cseg
; These 'equ' must match the hardware wiring
LCD_RS equ P1.3
;LCD_RW equ PX.X ; Not used in this code, connect the pin to GND
LCD_E  equ P1.4
LCD_D4 equ P0.0
LCD_D5 equ P0.1
LCD_D6 equ P0.2
LCD_D7 equ P0.3


$NOLIST
$include(LCD_4bit.inc) ; A library of LCD related functions and utility macros
$LIST

;                     1234567890123456    <- This helps determine the location of the counter
Initial_Message:  db 'BCD_counter: xx ', 0
Initial_State:  db 'State:       xx ', 0
Soak_Display:    db 'S:xx T:xx R:xx T:xx', 0

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 0                     ;
;---------------------------------;
Timer0_Init:
	orl CKCON, #0b00001000 ; Input for timer 0 is sysclk/1
	mov a, TMOD
	anl a, #0xf0 ; 11110000 Clear the bits for timer 0
	orl a, #0x01 ; 00000001 Configure timer 0 as 16-timer
	mov TMOD, a
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	; Enable the timer and interrupts
    setb ET0  ; Enable timer 0 interrupt
    setb TR0  ; Start timer 0
	ret

;---------------------------------;
; ISR for timer 0.  Set to execute;
; every 1/4096Hz to generate a    ;
; 2048 Hz wave at pin SOUND_OUT   ;
;---------------------------------;
Timer0_ISR:
	;clr TF0  ; According to the data sheet this is done for us already.
	; Timer 0 doesn't have 16-bit auto-reload, so
	clr TR0
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	setb TR0
	cpl SOUND_OUT ; Connect speaker the pin assigned to 'SOUND_OUT'!
	reti

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 2                     ;
;---------------------------------;
Timer2_Init:
	mov T2CON, #0 ; Stop timer/counter.  Autoreload mode.
	mov TH2, #high(TIMER2_RELOAD)
	mov TL2, #low(TIMER2_RELOAD)
	; Set the reload value
	orl T2MOD, #0x80 ; Enable timer 2 autoreload
	mov RCMP2H, #high(TIMER2_RELOAD)
	mov RCMP2L, #low(TIMER2_RELOAD)
	; Init One millisecond interrupt counter.  It is a 16-bit variable made with two 8-bit parts
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Enable the timer and interrupts
	orl EIE, #0x80 ; Enable timer 2 interrupt ET2=1
    setb TR2  ; Enable timer 2
	ret

;---------------------------------;
; ISR for timer 2                 ;
;---------------------------------;
Timer2_ISR:
	clr TF2  ; Timer 2 doesn't clear TF2 automatically. Do it in the ISR.  It is bit addressable.
	cpl P0.4 ; To check the interrupt rate with oscilloscope. It must be precisely a 1 ms pulse.
	
	; The two registers used in the ISR must be saved in the stack
	push acc
	push psw
	
	; Increment the 16-bit one mili second counter
	inc Count1ms+0    ; Increment the low 8-bits first
	mov a, Count1ms+0 ; If the low 8-bits overflow, then increment high 8-bits
	jnz Inc_Done
	inc Count1ms+1

Inc_Done:
	; Check if half second has passed
	mov a, Count1ms+0
	cjne a, #low(300), Timer2_ISR_done ; Warning: this instruction changes the carry flag!
	mov a, Count1ms+1
	cjne a, #high(300), Timer2_ISR_done
	
	; 500 milliseconds have passed.  Set a flag so the main program knows
	setb half_seconds_flag ; Let the main program know half second had passed
	
	cpl TR0 ; Enable/disable timer/counter 0. This line creates a beep-silence-beep-silence sound.
	; Reset to zero the milli-seconds counter, it is a 16-bit variable
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Increment the BCD counter
	mov a, BCD_counter
	add a, #0x01
	sjmp Timer2_ISR_da
Timer2_ISR_da:
	da a ; Decimal adjust instruction.  Check datasheet for more details!
	mov BCD_counter, a

	lcall update_LCD
	
Timer2_ISR_done:
	pop psw
	pop acc
	reti

wait_1ms:
	clr	TR0 ; Stop timer 0
	clr	TF0 ; Clear overflow flag
	mov	TH0, #high(TIMER0_RELOAD_1MS)
	mov	TL0,#low(TIMER0_RELOAD_1MS)
	setb TR0
	jnb	TF0, $ ; Wait for overflow
	ret

waitms:
	lcall wait_1ms
	djnz R2, waitms
	ret

LCD_PB:
	; Set variables to 1: 'no push button pressed'
	setb PB0
	setb PB1
	setb PB2
	setb PB3
	setb PB4
	; The input pin used to check set to '1'
	setb P1.5
	
	; Check if any push button is pressed
	clr P0.0
	clr P0.1
	clr P0.2
	clr P0.3
	clr P1.3
	jb P1.5, LCD_PB_Done

	; Debounce
	mov R2, #50
	lcall waitms
	jb P1.5, LCD_PB_Done

	; Set the LCD data pins to logic 1
	setb P0.0
	setb P0.1
	setb P0.2
	setb P0.3
	setb P1.3
	
	; Check the push buttons one by one
	clr P1.3
	mov c, P1.5
	mov PB4, c
	setb P1.3

	clr P0.0
	mov c, P1.5
	mov PB3, c
	setb P0.0
	
	clr P0.1
	mov c, P1.5
	mov PB2, c
	setb P0.1
	
	clr P0.2
	mov c, P1.5
	mov PB1, c
	setb P0.2
	
	clr P0.3
	mov c, P1.5
	mov PB0, c
	setb P0.3

LCD_PB_Done:		
	ret

;---------------------------------;
; Main program. Includes hardware ;
; initialization and 'forever'    ;
; loop.                           ;
;---------------------------------;
main:
	; Initialization
    mov SP, #0x7F
    mov P0M1, #0x00
    mov P0M2, #0x00
    mov P1M1, #0x00
    mov P1M2, #0x00
    mov P3M2, #0x00
    mov P3M2, #0x00
          
    lcall Timer0_Init
    lcall Timer2_Init
    setb EA   ; Enable Global interrupts
    lcall LCD_4BIT
    ; For convenience a few handy macros are included in 'LCD_4bit.inc':
	Set_Cursor(1, 1)
    Send_Constant_String(#Initial_Message)
	Set_Cursor(2, 1)
    Send_Constant_String(#Initial_State)
    setb half_seconds_flag
	mov BCD_counter, #0x00
	mov state, #0x00
	mov Temp_soak, #0x00
	mov Time_soak, #0x00
	mov Temp_refl, #0x00
	mov Time_refl, #0x00
	
	; After initialization the program stays in this 'forever' loop
loop:
	jb START_BUTTON, loop_a  ; if the 'CLEAR' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb START_BUTTON, loop_a  ; if the 'CLEAR' button is not pressed skip
	jnb START_BUTTON, $		; Wait for button release.  The '$' means: jump to same instruction.
	; A valid press of the 'CLEAR' button has been detected, reset the BCD counter.
	; But first stop timer 2 and reset the milli-seconds counter, to resync everything.
	clr TR2                 ; Stop timer 2
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Now clear the BCD counter
	mov BCD_counter, a
	setb TR2                ; Start timer 2
	lcall update_LCD             ; Display the new value
	
	; enter FSM loops
	sjmp state0
loop_a:
	jnb half_seconds_flag, loop
update_LCD:
    clr half_seconds_flag ; We clear this flag in the main loop, but it is set in the ISR for timer 2
	Set_Cursor(1, 14)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(BCD_counter) ; This macro is also in 'LCD_4bit.inc'
	Set_Cursor(2, 14)
	Display_BCD(state)
    ret
    
; our own code:
state0: ; Start button dependent
    ;mov pwm, #0
    
    mov state, #0x00
    mov BCD_counter, #0x00
	clr TR2                 ; Stop timer 2
	clr a
	lcall update_LCD
	
	;display the state
	Set_Cursor(1, 1)
    Send_Constant_String(#Initial_State)
	;display the data
    Set_Cursor(2, 1)
    Send_Constant_String(#Soak_Display)

check_button1:
    lcall LCD_PB; get the pushbuttons value

	jb PB1, check_button2
	wait_milli_seconds(#50)  ; debounce
	jb PB1, check_button2
	jnb PB1, $

	mov a, Temp_soak
	add a, #1
	mov Temp_soak, a
	lcall update_LCD

check_button2:
    lcall LCD_PB; get the pushbuttons value

	jb PB2, check_button3
	wait_milli_seconds(#50)  ; debounce
	jb PB2, check_button3
	jnb PB2, $

	mov a, Time_soak
	add a, #1
	mov Time_soak, a
	lcall update_LCD

check_button3:
    lcall LCD_PB; get the pushbuttons value

	jb PB3, check_button4
	wait_milli_seconds(#50)  ; debounce
	jb PB3, check_button4
	jnb PB3, $

	mov a, Temp_refl
	add a, #1
	mov Temp_refl, a
	lcall update_LCD

check_button4:
    lcall LCD_PB; get the pushbuttons value

	jb PB4, state0_done
	wait_milli_seconds(#50)  ; debounce
	jb PB4, state0_done
	jnb PB4, $

	mov a, Time_refl
	add a, #1
	mov Time_refl, a
	lcall update_LCD

state0_done:
    jb start_button, skip1
    ljmp check_button1
    
skip1:
    wait_milli_seconds(#50)  ; debounce
    jb start_button, skip2
    ljmp check_button1
skip2:
    jnb start_button, $
    
    sjmp state1

state1: ; Temperature dependent
       
    mov state, #0x01
    mov sec_threshold, #0x05
    mov BCD_counter, #0
    lcall update_LCD    ; update display for state
	setb TR2                 ; Stop timer 2    
state1_loop:
    clr c
    ;lcall get_temperature   ; bcd = current temperature
	mov a, BCD_counter
    subb a, sec_threshold     ; check if temperature > 150 then jump
    jnc state2             ; if c is 0 (result is positive) go to state 2
    ;lcall delay_50ms        ; GPT said this is good to add, we still have to write this routine
    sjmp state1_loop

state2: ; Time dependent
    ;mov pwm, #20
    mov state, #0x02
    mov sec_threshold, #0x10
    mov BCD_counter, #0x00
    lcall update_LCD    ; update display for state    
state2_loop:
	clr c
	mov a, BCD_counter
    subb a, sec_threshold
    jnc state3
    sjmp state2_loop

state3: ; Time dependent
    ;mov pwm, #20
    mov state, #0x03
    mov sec_threshold, #0x15
    mov BCD_counter, #0x00
    lcall update_LCD
state3_loop:
	clr c
	mov a, BCD_counter
    subb a, sec_threshold
    jnc skip3
    sjmp state3_loop
    
skip3:
    ljmp state0
END
