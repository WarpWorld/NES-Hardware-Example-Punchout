#org $AFC1, $1AFD1, 3
	JSR InputFunc

#org $83AB, $163BB, 3
	JMP CountFunc

#org $A008, $1A018, 5
	JMP NMIFunc
	NOP
	NOP

#org $07F0
Scratch:

;Scratch+0 - Dpad Invert
;Scratch+1 - Fast AI
;Scratch+2 - Countdown Trigger
;Scratch+3 - Countdown Mod
;Scratch+4 - Countdown Mod B
;Scratch+5 - Round Update Trigger


;F6,FC - fast 18,0C slow
#org $9FC0, $17FD0
CountFunc:
	PHA
	LDA $05
	;CMP #$02
	BEQ :+
	LDA Scratch+2
	CMP #$AB
	BNE :+

	PLA
	PHA
	CMP #$0B
	BEQ Big
	CMP #$09
	BEQ Small
	CMP #$05
	BNE :+
Small:
	PLA
	CLC
	ADC Scratch+4
	STA $52
	RTS
Big:
	PLA
	CLC
	ADC Scratch+3
	STA $52
	RTS
	:
	PLA
	STA $52
	RTS

#org $D8B0, $1D8C0
InputFunc:
	JSR $B021

	LDA Scratch+0
	CMP #$AB
	BNE :+

	LDA $D0
	AND #$F0
	STA Scratch+$B
	LDA $D0
	AND #$0A
	LSR
	ORA Scratch+$B
	STA Scratch+$B

	LDA $D0
	AND #$05
	ASL
	ORA Scratch+$B
    STA $D0

	:

	LDA Scratch+1
	CMP #$AB
	BNE :+

	LDA $39
	CMP #$10
	BCC :+
	LDA #$10
	STA $39

	:

	RTS
NMIFunc:
	PHA
	TXA
	PHA
	TYA
	PHA

	LDA Scratch+5
	CMP #$AB
	BNE :+

	LDX #$20
	LDY #$93
	JSR $AF2E
	LDA $06
	CLC
	ADC #$BF
	CMP #$BF
	BNE NoZero
	LDA #$FF
NoZero:	
	STA $2007
	JSR $AA48
	LDA #$00
	STA Scratch+5

	:

	JMP $A00D