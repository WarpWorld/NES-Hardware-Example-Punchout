;;;
;;; Register addresses
;;;
#org $40f0
#var FIFO_DATA, 1
#var FIFO_STATUS, 1

;;; SRAM Table for Freezes

;;; Format - FF AA AA VV MM SS WW CA CA CV CS CT XX XX XX XX
;	0	FF - Used flag/fingerprint - AB indicates valid in-use slot
;	1-2	AA - Address
;	3	VV - Value
;	4	MM - Mask
;	5	SS - Size
;	6	WW - Write Type
;	7-8	CA - Compare Address
;	9	CV - Compare Value
;	A	CS - Compare Size
;	B	CT - Compare Type
;	C-F XX - Unused/Expansion

#org $7F00
FreezeTable:
#org $7F80
FreezeScratch:

;Non SRAM value to detect freeze table needing to be initialized
#org $07FF
FreezeInit:

#org $07A1
CommReady:


;;; 
;;; Variables in zero page to make code size small
;;;
#org $7DCC
#var payloadOffset, 1
#var payloadNewValue, 1
#var payloadOldValue, 1
#var payloadSize, 1


;;;
;;; Bottom of stack area, used as scratch area to put code into from usb communications.
;;;
#org $0180; 
codeScratchArea:

#org $07A2
messageReturn:



;;;
;;; The usb write message header. This never changes, so it can just sit in rom and be copied.
;;;
#org $D920, $1D930, $C
sendItemStateChangeMsgHeader:
; n8pro protocol header, '+', '+'^0xff, CMD_USB_WR, CMD_USB_WR^0xff, len in LE BO
#byte '+', $2b ^ $ff, $22, $22 ^ $ff
#byte 4, 0
; our header, 'i' for 'item'.
#byte 'i'
sendItemStateChangeMsgHeaderEnd:

;;;
;;; nmi hook function to check for code dump from usb interface, loads code into ram and jumps to it
;;; it's up to the host to include code to signal that it has successfully handled the data
;;; by writing a completion message to the fifo.
;;;
#org $D8ED, $1D8FD
	JMP usbDataHook

#org $D940, $1D950
usbDataHook:
	;LDA CommReady
	;CMP #$AB
	;BEQ .done
	
	lda FIFO_STATUS
	bmi .done
	cmp #$40 ; extra check for special value mesen returns for unknown registers so we don't break everything
	beq .done
	lda FIFO_DATA
	CMP #$40
	beq .done
	sta payloadSize

	ldx #$0
	.loop:
	cpx payloadSize
	beq .success

	.waitByteLoop:
	
	.doread:
	;lda FIFO_STATUS
	;bmi .waitByteLoop
	lda FIFO_DATA
	sta codeScratchArea,X

	inx
	jmp .loop

	.success:

	:
	LDA FIFO_STATUS
	BMI :+
	LDA FIFO_DATA
	JMP :-
	:

	LDA #$AB
	STA CommReady

	.done:
	LDA $07F5
	JMP $D8F0



;In level hijack
#org $AFBD, $1AFCD
	JSR MainFunc
	NOP

;Main routine hijack instead of NMI
#org $DA00, $1DA10
MainFunc:
	SEI


	LDA $02
	PHA
	LDA $03
	PHA
	LDA $0F
	PHA
	LDA $05
	PHA

	LDA CommReady
	CMP #$AB
	BNE :+

	JSR CommFunc

	LDA #$00
	STA CommReady
	:

	JSR ExecuteFreezes

	PLA
	STA $05
	PLA
	STA $0F
	PLA
	STA $03
	PLA
	STA $02

	CLI
	LDY #$06
	LDX #$00
	RTS

CommFunc:
	
	;checksum verifcation
	LDA payloadSize
	TAX
	DEX
	DEX
	LDA #$00
	STA $07A3
	:
	LDA codeScratchArea,x
	CLC
	ADC $07A3
	STA $07A3
	DEX
	BPL :-

	LDX payloadSize
	DEX
	LDA codeScratchArea,x
	CMP $07A3
	BEQ :+

	;LDA #$72
	;STA FIFO_DATA
	
	RTS
	:

	LDX #$00
	;Read message ID
	LDA codeScratchArea,x
	STA $03
	INX
	;Read action
	LDA codeScratchArea,x
	STA $0F	
	INX

	;system messages
	CMP #$FF
	BNE :+
	JMP NullMsg
	:
	CMP #$FE
	BNE :+
	JMP Version
	:
	;All actions over 6 get sent to return
	CMP #$06
	BCC :+
	LDA #$05
	:


	;;;;;;;;;;;
	;LDA $03
	;STA $0342
	;ORA #$40
	;STA $034A
	;LDA #$80
	;STA $0349
	;RTS

	;jump table for actions
	ASL
    TAY
    LDA Actions,y
    STA $0B
    LDA Actions+1,y
    STA $0C
    JMP ($000B)

Actions:
	#byte <Reads,  >Reads,  <ArrayReads,  >ArrayReads
	#byte <Writes, >Writes, <ArrayWrites, >ArrayWrites
	#byte <Freeze, >Freeze, <CommReturn, >CommReturn

Reads:

	;Read number of addresses
	LDA codeScratchArea,x
	STA $02

	;send N8/USB header stuff
	ldy #$00
	.headerLoop:
	lda sendItemStateChangeMsgHeader, y
	STA FIFO_DATA
	iny
	cpy #4
	bne .headerLoop

	;send message length
	LDA $02
	CLC
	ADC #$03
	STA FIFO_DATA
	LDA #$00
	STA FIFO_DATA

	;Our return header
	LDA #$20  ;static part
	STA FIFO_DATA
	LDA $03   ;message ID
	STA FIFO_DATA
	LDA $0F   ;action (0)
	STA FIFO_DATA



	LDY #$00

	INX

	;loop through addresses sent
	:
	LDA codeScratchArea,x
	STA $0B
	INX
	LDA codeScratchArea,x
	STA $0C
	INX

	;use indirect addressing to read value with 00 as a pointer
	TYA
	PHA
	LDY #$00
	LDA ($0B),Y
	STA FIFO_DATA
	PLA
	TAY

	;loop until reaching saved number of addresses
	INY
	CPY $02
	BNE :-


CommReturn:
	RTS


WriteByte:
	STA FIFO_DATA
	RTS

ArrayReads:

	;Read array length
	LDA codeScratchArea,x
	STA $02

	;send N8/USB header stuff
	ldy #$00
	.headerLoop:
	lda sendItemStateChangeMsgHeader, y
	STA FIFO_DATA
	iny
	cpy #4
	bne .headerLoop

	;send message length
	LDA $02
	CLC
	ADC #$03
	STA FIFO_DATA
	LDA #$00
	STA FIFO_DATA

	;Our return header
	LDA #$20  ;static part
	STA FIFO_DATA
	LDA $03   ;message ID
	STA FIFO_DATA
	LDA $0F   ;action (1)
	STA FIFO_DATA




	INX

	;store base address to pointer

	LDA codeScratchArea,x
	STA $0B
	INX
	LDA codeScratchArea,x
	STA $0C
	INX

	;use indirect addressing to read value with 00 as a pointer
	LDY #$00
	:
	LDA ($0B),Y
	STA FIFO_DATA


	;loop until reaching array length
	INY
	CPY $02
	BNE :-

	RTS


Writes:

	;Read number of addresses
	LDA codeScratchArea,x
	STA $02

	;send N8/USB header stuff
	ldy #$00
	.headerLoop:
	lda sendItemStateChangeMsgHeader, y
	STA FIFO_DATA
	iny
	cpy #4
	bne .headerLoop

	;send message length
	LDA #$03
	STA FIFO_DATA
	LDA #$00
	STA FIFO_DATA

	;Our return header
	LDA #$20  ;static part
	STA FIFO_DATA
	LDA $03   ;message ID
	STA FIFO_DATA
	LDA $0F   ;action (2)
	STA FIFO_DATA


	LDY #$00

	INX

	;loop through addresses sent
	:

	;address
	LDA codeScratchArea,x
	STA $0B
	INX
	LDA codeScratchArea,x
	STA $0C
	INX

	;value
	LDA codeScratchArea,x
	STA $05
	INX



	;use indirect addressing to read value with 00 as a pointer
	TYA
	PHA
	LDY #$00
	LDA $05
	STA ($0B),Y
	PLA
	TAY

	;loop until reaching saved number of addresses
	INY
	CPY $02
	BNE :-

	RTS

ArrayWrites:

	;Read array length
	LDA codeScratchArea,x
	STA $02

	;send N8/USB header stuff
	ldy #$00
	.headerLoop:
	lda sendItemStateChangeMsgHeader, y
	STA FIFO_DATA
	iny
	cpy #4
	bne .headerLoop

	;send message length
	LDA #$03
	STA FIFO_DATA
	LDA #$00
	STA FIFO_DATA

	;Our return header
	LDA #$20  ;static part
	STA FIFO_DATA
	LDA $03   ;message ID
	STA FIFO_DATA
	LDA $0F   ;action (3)
	STA FIFO_DATA


	INX

	;store base address to pointer
	LDA codeScratchArea,x
	STA $0B
	INX
	LDA codeScratchArea,x
	STA $0C
	INX

	;use indirect addressing to read value with 00 as a pointer
	LDY #$00
	:

	;values
	LDA codeScratchArea,x
	INX
	STA ($0B),Y

	;loop until reaching array length
	INY
	CPY $02
	BNE :-

	RTS

Version:
	;send N8/USB header stuff
	ldy #$00
	.headerLoop:
	lda sendItemStateChangeMsgHeader, y
	STA FIFO_DATA
	iny
	cpy #4
	bne .headerLoop

	;send message length
	LDA #$08
	STA FIFO_DATA
	LDA #$00
	STA FIFO_DATA

	;Our return header
	LDA #$20  ;static part
	STA FIFO_DATA
	LDA $03   ;message ID
	STA FIFO_DATA
	LDA $0F   ;action (FE)
	STA FIFO_DATA

	;version number
	LDA #$00
	STA FIFO_DATA
	LDA #$00
	STA FIFO_DATA

	;max read size
	LDA #$10
	STA FIFO_DATA
	;max write size
	LDA #$10
	STA FIFO_DATA
	;max freezes
	LDA #$05
	STA FIFO_DATA
	RTS

NullMsg:
	;send N8/USB header stuff
	ldy #$00
	.headerLoop:
	lda sendItemStateChangeMsgHeader, y
	STA FIFO_DATA
	iny
	cpy #4
	bne .headerLoop

	;send message length
	LDA #$03
	STA FIFO_DATA
	LDA #$00
	STA FIFO_DATA

	;Our return header
	LDA #$20  ;static part
	STA FIFO_DATA
	LDA $03   ;message ID
	STA FIFO_DATA
	LDA $0F   ;action (FF)
	STA FIFO_DATA

	RTS

Freeze:

	;size
	LDA codeScratchArea,x
	STA FreezeScratch+4
	INX

	;compare size
	LDA codeScratchArea,x
	STA FreezeScratch+9
	INX	

	;address
	LDA codeScratchArea,x
	STA $0B
	STA FreezeScratch
	INX
	LDA codeScratchArea,x
	STA $0C
	STA FreezeScratch+1
	INX



	;value
	LDA codeScratchArea,x
	STA FreezeScratch+2
	INX
	
	;mask
	LDA codeScratchArea,x
	STA FreezeScratch+3
	INX


	;write mode
	LDA codeScratchArea,x
	STA FreezeScratch+5
	INX

	;compare address
	LDA codeScratchArea,x
	STA FreezeScratch+6
	INX
	LDA codeScratchArea,x
	STA FreezeScratch+7
	INX

	;compare value
	LDA codeScratchArea,x
	STA FreezeScratch+8
	INX

	;compare type
	LDA codeScratchArea,x
	STA FreezeScratch+10
	INX

	JSR FindFreezeAddr
	BCC .notfound

	;address exists in table
	LDA FreezeScratch+10
	BNE :+

	;Compare is never, erase freeze entry
	LDA #$01
	STA $05
	LDA #$00
	STA FreezeTable,x
	BEQ .finish

	:
	;overwrite existing entry
	JSR SaveFreeze
	LDA #$01
	STA $05
	SEC
	BCS .finish

	.notfound:

	JSR FindFreezeEmpty
	BCS :+

	;No empty slot
	LDA #$00
	STA $05
	BEQ .finish

	:
	JSR SaveFreeze
	LDA #$01
	STA $05

	.finish:

	;send N8/USB header stuff
	ldy #$00
	.headerLoop:
	lda sendItemStateChangeMsgHeader, y
	STA FIFO_DATA
	iny
	cpy #4
	bne .headerLoop

	;send message length
	LDA #$04
	STA FIFO_DATA
	LDA #$00
	STA FIFO_DATA

	;Our return header
	LDA #$20  ;static part
	STA FIFO_DATA
	LDA $03   ;message ID
	STA FIFO_DATA
	LDA $0F   ;action (04)
	STA FIFO_DATA
	LDA $05   ;success
	STA FIFO_DATA

	RTS

;Looks for Address matching $0B,$0C - sets X to that slot and sets carry if found, otherwise clear carry
FindFreezeAddr:
	LDX #$00
	:
	LDA FreezeTable,x
	CMP #$AB
	BNE :+
	LDA FreezeTable+1,x
	CMP $0B
	BNE :+
	LDA FreezeTable+2,x
	CMP $0C
	BNE :+
	SEC
	RTS
	:
	TXA
	CLC
	ADC #$10
	TAX
	CMP #$80
	BNE :--
	CLC
	RTS

;Looks for unused slot - sets X to that slot and sets carry if found, otherwise clear carry
FindFreezeEmpty:
	LDX #$00
	:
	LDA FreezeTable,x
	CMP #$AB
	BEQ :+
	SEC
	RTS
	:
	TXA
	CLC
	ADC #$10
	TAX
	CMP #$80
	BNE :--
	CLC
	RTS

;Copies the incoming values into the current freeze slot pointed to in X
SaveFreeze:

	LDA #$AB
	STA FreezeTable,x
	INX

	LDY #$00
	:
	LDA FreezeScratch,y
	STA FreezeTable,x
	INX
	INY
	CPY #$0B
	BNE :-
	RTS

;loops over freeze table, tests compares and then calls writes
ExecuteFreezes:

	LDA FreezeInit
	CMP #$AB
	BEQ :+

	LDA #$AB
	STA FreezeInit

	LDA #$00
	STA FreezeTable
	STA FreezeTable+$10
	STA FreezeTable+$20
	STA FreezeTable+$30
	STA FreezeTable+$40
	STA FreezeTable+$50
	STA FreezeTable+$60
	STA FreezeTable+$70
	:

	LDX #$00
	:
	LDA FreezeTable,x
	CMP #$AB
	BNE :+

	JSR TestCompare
	BCC :+

	JSR FreezeWrite

	:
	TXA
	CLC
	ADC #$10
	TAX
	CMP #$80
	BNE :--
	CLC


	RTS


;public enum CompareType : byte
;{
;    Never = 0x00,
;    Equal = 0x01,
;    NotEqual = 0x02,//6,A,C,E
;    Always = 0x03,//7,B,D,F
;    GreaterThan = 0x04,
;    GreaterThanOrEqual = 0x05,
;    LessThan = 0x08,
;    LessThanOrEqual = 0x09,
;    NotLessThan = 0x05,
;    NotGreaterThan = 0x09,
;
;    MaskSet = 0x11,
;    MaskUnset = 0x12,
;
;}

;performs the comparison check for the current freeze and sets carry if valid
TestCompare:
	;Get current value
	LDA FreezeTable+7,x
	STA $0B
	LDA FreezeTable+8,x
	STA $0C

	LDY #$00
	LDA ($0B),y
	STA $02

	LDA FreezeTable+$B,x
	CMP #$01
	BNE :+

	;Equal
	LDA $02
	CMP FreezeTable+9,x
	BEQ Success
	BNE .fail

	:
	CMP #$02
	BNE :+

	;NotEqual
	LDA $02
	CMP FreezeTable+9,x
	BNE Success
	BEQ .fail

	:
	CMP #$03
	;Always
	BEQ Success
	CMP #$04
	BNE :+

	;GreaterThan
	LDA $02
	CMP FreezeTable+9,x
	BEQ .fail
	BCS Success
	BCC .fail

	:
	CMP #$05
	BNE :+
	;GreaterThanOrEqual
	LDA $02
	CMP FreezeTable+9,x
	BCC .fail
	BCS Success
	
	:
	CMP #$08
	BNE :+

	;LessThan
	LDA $02
	CMP FreezeTable+9,x
	BCS .fail
	BCC Success
	:
	CMP #$09
	BNE :+

	;LessThanOrEqual
	LDA $02
	CMP FreezeTable+9,x
	BEQ Success
	BCS .fail
	BCC Success
	:

	.fail:
	CLC
	RTS

	Success:
	SEC
	RTS

;public enum WriteType : byte
;{
;    None = 0,
;    WriteValue = 1,
;    IncrementBy = 2,
;    DecrementBy = 3,
;    WriteMasked = 4,
;}

FreezeWrite:

	LDA FreezeTable+1,x
	STA $0B
	LDA FreezeTable+2,x
	STA $0C

	LDA FreezeTable+6,x
	BNE :+
	
	;None
	RTS

	:	
	CMP #$01
	BNE :+

	;WriteValue
	LDY #$00
	LDA FreezeTable+3,x
	STA ($0B),y
	RTS

	:
	CMP #$02
	BNE :+

	;IncrementBy
	LDY #$00
	LDA ($0B),y
	CLC
	ADC FreezeTable+3,x
	STA ($0B),y
	RTS

	:
	CMP #$03
	BNE :+

	;DecrementBy
	LDY #$00
	LDA ($0B),y
	SEC
	SBC FreezeTable+3,x
	STA ($0B),y
	RTS

	:
	CMP #$04
	BNE :+

	;WriteMasked
	LDA FreezeTable+4,x
	STA $03
	AND FreezeTable+3,x
	STA $02

	LDA $03
	EOR #$FF
	STA $03

	LDY #$00
	LDA ($0B),y
	AND $03
	ORA $02
	STA ($0B),y
	

	:

	RTS