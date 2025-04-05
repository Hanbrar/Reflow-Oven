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
PWM_OUT    EQU P1.0 ; Logic 1=oven on


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
BCD_counter:  ds 2 ; The BCD counter incrememted in the ISR and displayed in the main loop
sec_threshold: ds 1 ;
state: ds 1
sec : ds 1
sec1: ds 1
Avg_Sum: ds 4 ; accumulator for averaging

current_temp : ds 4
LM_temp: ds 4
; for Math32:
x:   ds 4
y:   ds 4
bcd: ds 5
Oven_temp: ds 4
TH_temp:      ds 4  ; Thermocouple temperature (BCD)

beep_time:  ds 2      ; new varibles to set how long the beep turning on

Temp_soak: ds 2
Time_soak: ds 2
Temp_refl: ds 2
Time_refl: ds 2

pwm_counter:  ds 1 ; Free running counter 0, 1, 2, ..., 100, 0
pwm:          ds 1 ; pwm percentage
seconds:      ds 1 ; a seconds counter attached to Timer 2 ISR


 
; In the 8051 we have variables that are 1-bit in size.  We can use the setb, clr, jb, and jnb
; instructions with these variables.  This is how you define a 1-bit variable:
bseg
half_seconds_flag: dbit 1 ; Set to one in the ISR every time 500 ms had passed
s_flag: dbit 1 ; set to 1 every time a second has passed
ison: dbit 1

PB0: dbit 1  ; Pushbutton 0 
PB1: dbit 1  ; Pushbutton 1 
PB2: dbit 1  ; Pushbutton 2 
PB3: dbit 1  ; Pushbutton 3 
PB4: dbit 1  ; Pushbutton 4
mf: dbit 1

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

$NOLIST
$include(math32.inc)
$LIST

Init_All:
	; Configure all the pins for biderectional I/O
	mov	P3M1, #0x00
	mov	P3M2, #0x00
	mov	P1M1, #0x00
	mov	P1M2, #0x00
	mov	P0M1, #0x00
	mov	P0M2, #0x00
	
	orl	CKCON, #0x10 ; CLK is the input for timer 1
	orl	PCON, #0x80 ; Bit SMOD=1, double baud rate
	mov	SCON, #0x52
	anl	T3CON, #0b11011111
	anl	TMOD, #0x0F ; Clear the configuration bits for timer 1
	orl	TMOD, #0x20 ; Timer 1 Mode 2
	mov	TH1, #TIMER1_RELOAD ; TH1=TIMER1_RELOAD;
	setb TR1
	
	; Using timer 0 for delay functions.  Initialize here:
	clr	TR0 ; Stop timer 0
	orl	CKCON,#0x08 ; CLK is the input for timer 0
	anl	TMOD,#0xF0 ; Clear the configuration bits for timer 0
	orl	TMOD,#0x01 ; Timer 0 in Mode 1: 16-bit timer
	
	; Initialize the pin used by the ADC (P1.1) as input.
	orl	P0M1, #0b00010000  ; Set P0.4 as input (analog mode)
	anl	P0M2, #0b11101111  ; Ensure P0.4 is not set as push-pull output
	
	; Initialize and start the ADC:
	; Initialize and start the ADC:
	anl ADCCON0, #0xF0      ; Clear channel selection bits
	orl ADCCON0, #0x05      ; Select channel 5 (P0.4)

	; AINDIDS select if some pins are analog inputs or digital I/O:
	mov AINDIDS, #0x00      ; Disable all analog inputs
	orl AINDIDS, #0b00010000 ; Enable P0.4 as analog input

	orl ADCCON1, #0x01      ; Enable ADC
	ret

;                     1234567890123456    <- This helps determine the location of the counter
Initial_Message:  db 'TIME:xx State: xx  P:xx', 0
Soak_Display:    db 'S   ,   R   ,    ', 0
on_message: db 'on ',0
off_message: db 'of ',0
oven_temperature_message: db 'To:   ,Tr:            ',0


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
    
    jb ison,jump2
    
    clr SOUND_OUT

	reti

    jump2:
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

	inc pwm_counter
	clr c
	mov a, pwm
	subb a, pwm_counter ; If pwm_counter <= pwm then c=1
	cpl c
	mov PWM_OUT, c
	
	mov a, pwm_counter
	cjne a, #100, Timer2_ISR_done
	mov pwm_counter, #0
	inc seconds ; It is super easy to keep a seconds count here
	setb s_flag


Inc_Done:
    ; Check if half second has passed
    mov a, Count1ms+0
    cjne a, #low(1000), Timer2_ISR_done ; Warning: this instruction changes the carry flag!
    mov a, Count1ms+1
    cjne a, #high(1000), Timer2_ISR_done
    
    ; 500 milliseconds have passed.  Set a flag so the main program knows
    setb half_seconds_flag ; Let the main program know half second had passed
    
    ;cpl TR0  Enable/disable timer/counter 0. This line creates a beep-silence-beep-silence sound.
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

;------------------------------------------
; new code to TIMER 2, to turn on/off speaker depend on the time
;------------------------------------------

    
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

putchar:
	jnb TI, putchar
	clr TI
	mov SBUF, a
	ret
	
SendString:
	clr A
	movc A, @A+DPTR
	jz SendStringDone
	lcall putchar
	inc DPTR
	sjmp SendString
	SendStringDone:
	ret


get_TH:
    ; ----- Initialize the 32-bit accumulator to zero -----
    clr  A
    mov  Avg_Sum+0, A
    mov  Avg_Sum+1, A
    mov  Avg_Sum+2, A
    mov  Avg_Sum+3, A

    mov  R3, #10          ; Number of samples to average

avg_loop:
	anl ADCCON0, #0xF0
	orl ADCCON0, #0x05 ; Select channel 5

	clr ADCF
	setb ADCS ;  ADC start trigger signal
    jnb ADCF, $ ; Wait for conversion complete
    
    ; Read the ADC result and store in [R1, R0]
    mov a, ADCRH   
    swap a
    push acc
    anl a, #0x0f
    mov R1, a
    pop acc
    anl a, #0xf0
    orl a, ADCRL
    mov R0, A
    
    ; Convert to voltage
	mov x+0, R0
	mov x+1, R1
	mov x+2, #0
	mov x+3, #0

	Load_y(50300) ; VCC voltage measured
	lcall mul32
	Load_y(4095) ; 2^12-1
	lcall div32          ; Divide by 4095 to get V_out in mV
    load_y(27300)
    lcall sub32
    load_y(100)
    lcall mul32


	;lcall hex2bcd
	;lcall putty_display

	mov LM_temp+0, x+0
	mov LM_temp+1, x+1
	mov LM_temp+2, x+2
	mov LM_temp+3, x+3			

    ; Select ADC channel 7 (AIN7/Pin 14)
    anl ADCCON0, #0xF0
    orl ADCCON0, #0x07 ;select 7
    clr ADCF
    setb ADCS
    jnb ADCF, $ ; Wait for ADC

    ; Read ADC result
    mov a, ADCRH
    swap a
    push acc
    anl a, #0x0f
    mov R1, a
    pop acc
    anl a, #0xf0
    orl a, ADCRL
    mov R0, A

    ; Convert to voltage (thermocouple)
    mov x+0, R0
    mov x+1, R1
    mov x+2, #0
    mov x+3, #0


	Load_y(50300) ; VCC voltage measured
	lcall mul32 

	Load_y(4095)
	lcall div32

	Load_y(244)
	lcall mul32




	;load_y(17100)
	;lcall mul32
	;Load_y(100) ; Correct scaling factor
    ;lcall div32
    ;Load_y(49800) ; VCC voltage measured
	;lcall mul32
    ;Load_y(244) ; Correct scaling factor
    ;lcall mul32
    ;Load_y(4095)
    ;lcall div32

    ; Add LM_temp (LM335) and TH_temp (thermocouple)
    mov y+0, LM_temp+0
    mov y+1, LM_temp+1
    mov y+2, LM_temp+2
    mov y+3, LM_temp+3
    lcall add32
 ; --- Accumulate this sample into Avg_Sum ---
    ; (Add the 32-bit result in x to the accumulator Avg_Sum)
    mov  y+0, Avg_Sum+0
    mov  y+1, Avg_Sum+1
    mov  y+2, Avg_Sum+2
    mov  y+3, Avg_Sum+3
    lcall add32     ; x = Avg_Sum + (current sample)
    ; Store the result back into Avg_Sum:
    mov  Avg_Sum+0, x+0
    mov  Avg_Sum+1, x+1
    mov  Avg_Sum+2, x+2
    mov  Avg_Sum+3, x+3

    djnz R3, jump   ; Loop for all 8 samples
	

    ; --- Divide the accumulated sum by 8 to get the average ---
    mov  x+0, Avg_Sum+0
    mov  x+1, Avg_Sum+1
    mov  x+2, Avg_Sum+2
    mov  x+3, Avg_Sum+3
    Load_y(10)
    lcall div32   ; Now x holds the average sample

    ; --- Convert the average value to BCD ---
    ; lcall hex2bcd    (Assuming hex2bcd converts the 32-bit value in x into BCD and stores it in your "bcd" array)

	

    ;Display T_H
    ;lcall hex2bcd
    ;lcall putty_display

	; Wait 500 ms between conversions
	mov R2, #250
	lcall waitms
	mov R2, #250
	lcall waitms
	
    ;Display T_H
    ;lcall hex2bcd
    ;lcall putty_display
	;lcall Display_temp
    lcall hex2bcd
    lcall putty_display

    ret	

jump:
ljmp avg_loop
ret


putty_display:

	; th
    mov A, bcd+3
    swap A
    anl A, #0x0F
    orl A, #0x30
    lcall putchar
	
	; hun
    mov A, bcd+3
    anl A, #0x0F
    orl A, #0x30
    lcall putchar
	
	; ten
    mov A, bcd+2
    swap A
    anl A, #0x0F
    orl A, #0x30
    lcall putchar

	; one
    mov A, bcd+2
    anl A, #0x0F
    orl A, #0x30
    lcall putchar

    ;mov A, #'.'
    ;lcall putchar

    mov A, bcd+1
    swap A
    anl A, #0x0F
    orl A, #0x30
    lcall putchar

    mov A, bcd+1
    anl A, #0x0F
    orl A, #0x30
    lcall putchar

    mov A, #0x0D  ; \r
    lcall putchar
    mov A, #0x0A  ; \n
    lcall putchar

	ret

;Display_temp:
;	
 ;   Set_Cursor(1, 1)
;	Display_BCD(bcd+3)
 ;   Display_BCD(bcd+2)
;	Display_char(#'.')
 ;   Display_BCD(bcd+1)
  ;  Display_BCD(bcd+0)
;
	;ret


;Display_LM_temp:
	
 ;   Set_Cursor(2, 1)
;	Display_BCD(LM_temp+3)
 ;   Display_BCD(LM_temp+2)
;	Display_char(#'.')
 ;   Display_BCD(LM_temp+1)
  ;  Display_BCD(LM_temp+0)

	;ret

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
	mov R2, #20
	lcall waitms
	jb P1.5, LCD_PB_Done

	; Set the LCD data pins to logic 1
	setb P0.0
	setb P0.1
	setb P0.2
	setb P0.3
	setb P1.3
	
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
	; Check the push buttons one by one
	clr P1.3
	mov c, P1.5
	mov PB1, c
	setb P1.3

	clr P0.0
	mov c, P1.5
	mov PB2, c
	setb P0.0
	
	clr P0.1
	mov c, P1.5
	mov PB3, c
	setb P0.1
	
	clr P0.2
	mov c, P1.5
	mov PB4, c
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

	lcall Init_All ; init call
          
    lcall Timer0_Init
    lcall Timer2_Init
    setb EA   ; Enable Global interrupts
    lcall LCD_4BIT
    ; For convenience a few handy macros are included in 'LCD_4bit.inc':
	Set_Cursor(1, 1)
    Send_Constant_String(#Initial_Message)
	Set_Cursor(2, 1)
    Send_Constant_String(#Soak_Display)
    setb half_seconds_flag
	mov BCD_counter+0, #0x00 
    mov BCD_counter+1, #0x00 
 	mov state, #0x00
	mov bcd+3, #0x00
	mov bcd+2, #0x00
	mov bcd+1, #0x00
	mov LM_temp+3, #0x00
	mov LM_temp+2, #0x00
	mov LM_temp+1, #0x00
	
	; initialize reflow values (rn its random)
	mov Temp_soak+0, #0x50 
    mov Temp_soak+1, #0x01 
    mov Time_soak+0, #0x10 
    mov Time_soak+1, #0x00
    mov Temp_refl+0, #0x20
    mov Temp_refl+1, #0x02
    mov Time_refl+0, #0x45
    mov Time_refl+1, #0x00
	
	
	lcall update_LCD
	lcall update_LCD2
	
	; After initialization the program stays in this 'forever' loop
loop:
	lcall get_TH
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
	ljmp state0
loop_a:
	jnb half_seconds_flag, loop

update_LCD:
    clr half_seconds_flag ; We clear this flag in the main loop, but it is set in the ISR for timer 2
	Set_Cursor(1, 6)     ; the place in the LCD where we want the BCD counter value
	Display_BCD_3Digits(BCD_counter) ; This macro is also in 'LCD_4bit.inc'
	Set_Cursor(1, 15)
	Display_BCD(state) ; This macro is also in 'LCD_4bit.inc'
    ret
    
update_LCD2:
    Set_Cursor(2, 1)
    Send_Constant_String(#Soak_Display)
    
	Set_Cursor(2,2)
	Display_BCD_3Digits(Temp_soak)

	Set_Cursor(2,6)
	Display_BCD_3Digits(Time_soak)

	Set_Cursor(2, 10)
	Display_BCD_3Digits(Temp_refl)

	Set_Cursor(2, 14)
	Display_BCD_3Digits(Time_refl)

	ret
update_LCD3:
    
    clr half_seconds_flag ; We clear this flag in the main loop, but it is set in the ISR for timer 2
	Set_Cursor(1, 6)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(BCD_counter) ; This macro is also in 'LCD_4bit.inc'
	Set_Cursor(1, 15)
	Display_BCD(state) ; This macro is also in 'LCD_4bit.inc'
	Set_Cursor(2, 1)
    Send_Constant_String(#oven_temperature_message)
	Set_Cursor(2, 4)

	;display oven temperature
	Display_BCD(bcd+3)
	Display_BCD(bcd+2)
    ;Display_BCD(bcd+2)
	;Display_char(#'.')
    ;Display_BCD(bcd+1)
    ;Display_BCD(bcd+0)
	
	;Display_BCD_3Digits(bcd)
	

	;display room temperature
	Set_Cursor(2, 11)
	Display_BCD(LM_temp+3)

    Display_BCD(LM_temp+2)
	;Display_char(#'.')
    ;Display_BCD(LM_temp+1)
    ;Display_BCD(LM_temp+0)
	
	;Display_BCD_3Digits(LM_temp)

    ret

; our own code:

state0: ; Start button dependent
    clr ison
    clr TR0
    mov pwm, #0
   
    mov state, #0x00
    mov BCD_counter+0, #0x00
    mov BCD_counter+1, #0x00
	clr TR2                 ; Stop timer 2
	clr a

	lcall update_LCD
	lcall update_LCD2

check_button1:
    lcall LCD_PB; get the pushbuttons value
	jb PB1, check_button2
PB1_check_release:	
	lcall LCD_PB
	jnb PB1, PB1_check_release

	mov a, Temp_soak+0 
    add a, #1             
    da a        
    mov Temp_soak+0, a    
    jnc skip_button1      
    mov a, Temp_soak+1    
    add a, #1 
    da a  
    mov Temp_soak+1, a 
skip_button1:
    lcall update_LCD2

check_button2:
    lcall LCD_PB; get the pushbuttons value
	jb PB2, check_button3
PB2_check_release:	
	lcall LCD_PB
	jnb PB2, PB2_check_release
	
    mov a, Time_soak+0 
    add a, #1             
    da a        
    mov Time_soak+0, a    
    jnc skip_button2      
    mov a, Time_soak+1    
    add a, #1 
    da a  
    mov Time_soak+1, a 
skip_button2:
    lcall update_LCD2

check_button3:
    lcall LCD_PB; get the pushbuttons value
	jb PB3, check_button4
PB3_check_release:	
	lcall LCD_PB
	jnb PB3, PB3_check_release
	

	mov a, Temp_refl+0 
    add a, #1             
    da a        
    mov Temp_refl+0, a    
    jnc skip_button3      
    mov a, Temp_refl+1    
    add a, #1 
    da a  
    mov Temp_refl+1, a 
skip_button3:
    lcall update_LCD2

check_button4:
    lcall LCD_PB; get the pushbuttons value
	jb PB4, state0_done
PB4_check_release:	
	lcall LCD_PB
	jnb PB4, PB4_check_release

	mov a, Time_refl+0 
    add a, #1             
    da a        
    mov Time_refl+0, a    
    jnc skip_button4      
    mov a, Time_refl+1    
    add a, #1 
    da a  
    mov Time_refl+1, a 
skip_button4:
    lcall update_LCD2

state0_done:
	lcall LCD_PB
    jnb PB0 , skip1
    ljmp check_button1
skip1:
    sjmp state1

state1: ; Temperature dependent
    setb ison
    wait_milli_seconds(#250)
    clr ison
    

	lcall get_TH
	mov state, #0x01

    ; set power
    mov pwm, #100
    
    ; set up temperature threshold (rn its still sec threshold need to add temp later)
    mov sec_threshold, #0x05
    
    mov BCD_counter+0, #0x00
    mov BCD_counter+1, #0x00 	; reset timer to 0
    lcall update_LCD3    ; update display for state
	
	setb TR2            ; Stop timer 2    
state1_loop:
    clr c
    lcall get_TH   	; bcd = current temperature

	mov a, bcd+3
    subb a, Temp_soak+1
    jnz check_temp_soak      ; check if temperature > Temp_soak
    mov a, bcd+2
    subb a, Temp_soak+0

check_temp_soak:
    jnc state2             		; if c is 0 (result is positive) go to state 2
    sjmp state1_loop

state2: ; s > Time_soak go state 3
    setb ison
    wait_milli_seconds(#250)
    clr ison
    

	lcall get_TH
    mov pwm, #20
    mov state, #0x02
    mov BCD_counter+0, #0x00
    mov BCD_counter+1, #0x00 
    lcall update_LCD3    ; update display for state    
	
state2_loop:
	lcall get_TH
	clr c
	wait_milli_seconds(#50)
    mov a, BCD_counter+1
    subb a, Time_soak+1
    jnz check_time_soak
    mov a, BCD_counter+0
    subb a, Time_soak+0

check_time_soak:
    jnc state3
    sjmp state2_loop

state3: ; Temp > 220 go state 4

	lcall get_TH
    mov pwm, #100 ;changed from 20 to 100
    mov state, #0x03
    mov BCD_counter+0, #0x00
    mov BCD_counter+1, #0x00 
    lcall update_LCD3
	
state3_loop:
	lcall get_TH
	clr c

	mov a, bcd+3
    subb a, Temp_refl+1
    jnz check_temp_refl      ; check if temperature > Temp_soak
    mov a, bcd+2
    subb a, Temp_refl+0

check_temp_refl:
    jnc state4

    sjmp state3_loop
    
state4: ; s > Time_refl go state 5
	lcall get_TH
    mov pwm, #20
    mov state, #0x04
    mov BCD_counter+0, #0x00
    mov BCD_counter+1, #0x00 
    lcall update_LCD3
	
state4_loop:
	lcall get_TH
	clr c
	mov a, BCD_counter+1
    subb a, Time_refl+1
    jnz check_time_refl
    mov a, BCD_counter+0
    subb a, Time_refl+0

check_time_refl:
    jnc state5
    sjmp state4_loop
    
    
state5: ; Temp < 60 go state 0

	lcall get_TH
    mov pwm, #0
    mov state, #0x05
    mov BCD_counter+0, #0x00
    mov BCD_counter+1, #0x00 
    lcall update_LCD3
state5_loop:
	lcall get_TH
	clr c
	mov a, #0x00
    subb a, bcd+3
    jnz check_state5      ; check if temperature < 60
    mov a, #60
    subb a, bcd+2

check_state5:
    jnc skip5

    sjmp state5_loop

skip5:
    ljmp state0

END