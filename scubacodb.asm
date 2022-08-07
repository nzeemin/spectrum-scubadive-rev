;----------------------------------------------------------------------------

	INCLUDE "scubacoda.txt"

;----------------------------------------------------------------------------

	ORG $6000

;----------------------------------------------------------------------------
; Sprites

	INCLUDE "scubasprt.asm"

;----------------------------------------------------------------------------

L9C50	DEFB $00	; $00 = no Octopus, $01 = we have Octopus on the game screen
L9C51	DEFW $0000	; Octopus screen address
L9C53	DEFW $A41B	; ??? LA41B or Octopus sprite address
L9C55	DEFB $04	; Octopus phase

; Draw game screen
; Draws 3x3 blocks = 24x24 tiles, draws Octopus if present, draws static objects on the screen.
; I: HL	Screen position on mini-map
DrawGameScr
	DI	
	LD DE,$4000		; screen pixels start address
	XOR A	
	LD (L9C50),A		; no Octopus by default
	PUSH IX	
	PUSH IY	
	LD IX,$5800		; screen attributes start address
	LD B,$03		; loop counter; game screen 3 blocks high = 24 tiles
	EX DE,HL		; now DE = screen position, HL = address on the screen
L9C69	PUSH BC	
	LD B,$03		; loop counter; game screen 3 blocks wide = 24 tiles
L9C6C	PUSH BC	
	PUSH IX	
	EX DE,HL		; now HL = screen position, DE = address on the screen
	CALL MiniMap_Get	; Calc address in MiniMap and Get block number
	PUSH AF	
	EX DE,HL	
	CP $1C			; place for Octopus?
	JR NZ,L9C89		; no => skip
	LD (L9C51),HL		; save the Octopus screen address
	PUSH HL	
	LD HL,LA41B	
	LD (L9C53),HL	
	POP HL	
	LD A,$01		; flag value
	LD (L9C50),A		; we have the Octopus on the screen
L9C89	INC E	
	LD A,E	
	AND $1F			; 0..31
	LD E,A	
	POP AF	
	PUSH DE	
	PUSH HL	
	PUSH HL	
	LD H,A	
	LD L,$00	
	SRL H	
	RR L	
	SRL H	
	RR L	
	LD DE,LA4DD		; base address for relief blocks, 8x8 tiles each block
	ADD HL,DE		; now HL = address of the block
	PUSH HL	
	POP IY			; now IY = address of the block
	POP HL	
	LD B,$08		; repeat 8 times - block height in tiles
L9CA7	PUSH BC	
	LD B,$08		; repeat 8 times - block width in tiles
L9CAA	PUSH BC	
	LD A,(IY+$00)		; get tile number
	PUSH HL	
	PUSH HL	
	AND $7F			; 0..127
	LD L,A	
	LD H,$00	
	ADD HL,HL	
	ADD HL,HL	
	ADD HL,HL		; *8
	LD BC,L9134		; base address for relief tiles, 8x8 pixels each tile
	ADD HL,BC		; now HL = tile address
	POP DE	
	LD B,$08		; repeat 8 times - tile height
L9CBF	LD A,(HL)		; get tile pixels
	LD (DE),A		; write to the screen
	INC HL			; next tile byte
	INC D			; next pixel row
	DJNZ L9CBF	
	POP HL	
	LD A,(L5B03+1)		; get Screen position on mini-map, row value
	OR A	
	JR NZ,L9CD5	
	LD A,H	
	AND $18			; 0 / 8 / 16 / 24
	JR NZ,L9CD5	
	LD C,$29	
	JR L9CEB	
L9CD5	LD C,$06	
	LD A,(IY+$00)	
	OR A	
	JR Z,L9CEB	
	CP $32	
	JR Z,L9CEB	
	LD C,$02		; red on black (color for relief)
	BIT 7,(IY+$00)	
	JR Z,L9CEB	
	LD C,$06		; yellow on black
L9CEB	LD (IX+$00),C		; set screen attribute
	INC IY	
	INC IX	
	INC L			; next column
	POP BC	
	DJNZ L9CAA		; continue horizontal loop for tiles in the block
	LD BC,$0018		; 24
	ADD IX,BC	
	DEC L	
	LD A,L	
	AND $F8	
	ADD A,$20	
	LD L,A	
	POP BC	
	DJNZ L9CA7		; continue vertical loop for tiles in the block
	POP HL	
	LD BC,$0008	
	ADD HL,BC	
	POP DE	
	POP IX	
	ADD IX,BC	
	POP BC	
	DEC B	
	JP NZ,L9C6C		; continue horizontal loop for blocks on the screen
	LD L,$00	
	LD A,H	
	ADD A,$08	
	LD H,A	
	LD BC,$00E8	
	ADD IX,BC	
	INC D	
	DEC E	
	DEC E	
	DEC E	
	LD A,E	
	AND $1F			; 0..31
	LD E,A	
	POP BC	
	DEC B	
	JP NZ,L9C69		; continue vertical lool for blocks on the screen
	LD A,(L9C50)		; get Octopus flag
	OR A			; do we have the Octopus on the screen?
	JR Z,L9D38		; no => skip
	LD A,(L9C55)		; get Octopus phase
	CALL LB346		; Draw Octopus
L9D38	POP IY	
L9D3A	POP IX	
L9D3C	CALL LB0A9		; Draw static objects on the screen; prepare LB07D table
	RET	

; Calculate address in the MiniMap table
; I: HL	H = row, L = column 0..31
; O: HL = address in the MiniMap table
MiniMap_Addr
	LD A,L	
	LD L,$00	
	SRL H	
	RR L	
	SRL H	
	RR L	
	SRL H	
	RR L			; HL shifted right 3 bits
	OR L	
	LD L,A			; HL := H * 32 + L
	LD DE,MiniMap	
	ADD HL,DE		; HL := MiniMap + H * 32 + L
	RET	

; Calculate address in the MiniMap table and Get block number
; I: H = row, L = column 0..31
; O: A = value
MiniMap_Get
	PUSH HL	
	PUSH DE	
	CALL MiniMap_Addr	; Calculate address in the MiniMap table
	LD A,(HL)		; get value
	POP DE	
	POP HL	
	RET

; Check value in the MiniMap table, if row and column in range 0..31
; I: H = row, L = column
; If column or row is out of range 0..31 - returns flag Z=0;
; else, gets value from MiniMap table;
; if this value is $01, returns flag Z=1, in other case flag Z=0.
MiniMap_Check
	PUSH HL	
	PUSH AF	
	LD A,L	
	AND $E0			; check Column for range 0..31
	JR NZ,L9D73	
	LD A,H	
	AND $E0			; check Row for range 0..31
	JR NZ,L9D73	
	CALL MiniMap_Get	; Calc address in MiniMap and Get block number
	LD L,A	
	POP AF	
	DEC L	
	POP HL	
	RET	
L9D73	POP AF	
	LD L,$01	
	INC L	
	POP HL	
	RET	

; Calculate address in the MiniMap table and Set block number
; I: H = row, L = column 0..31, A = value to set
MiniMap_Set
	PUSH HL	
	PUSH DE	
	PUSH AF	
	CALL MiniMap_Addr	; Calculate address in the MiniMap table
	POP AF	
	LD (HL),A		; set value
	POP DE	
	POP HL	
	RET

; Random
; Calculate next number in pseudo-random sequence
NextRandom
	LD HL,(RANDOM)		; get current Random
	LD D,H	
	LD E,L	
	ADD HL,HL		; x2
	ADD HL,HL		; x4
	ADD HL,HL		; x8
	ADD HL,HL		; x16
	PUSH HL	
	ADD HL,HL		; x32
	EX (SP),HL	
	OR A	
	SBC HL,DE		; HL = x15
	POP BC			; BC = x32
	ADD HL,BC		; x47
	ADD HL,HL		; x94
	ADD HL,HL		; x188
	ADD HL,HL		; x376
	ADD HL,DE		; x377
	ADD HL,HL		; x754
	ADD HL,HL		; x1508
	ADD HL,DE		; x1509
	LD DE,$0029	
	ADD HL,DE	
	LD (RANDOM),HL		; (RANDOM) := (RANDOM) * 1509 + 41
	RET	

L9DA4	DEFB $AF

; Fill block at HL with A
; I: HL = block address, A = value, B = length of the block to fill
FillBlock
	LD (HL),A	
	INC HL	
	DJNZ FillBlock	
	RET

; Prepare the MiniMap table
PrepareMiniMap
	LD HL,MiniMap+3*32+2	; $ACBF = $AC5D + 3 * 32 + 2: row 3 column 2
	LD B,$1C	
	LD A,$16	
	CALL FillBlock		; Fill block at $ACBF with $16
	LD (HL),$06		; ($ACDB) <- $06
	INC HL			; HL = $ACDC
	LD B,$22	
	LD A,$01	
	CALL FillBlock		; Fill block at $ACDC with $01
	INC HL			; HL = $ACFF
	LD A,$1A	
	LD B,$1C	
	CALL FillBlock		; Fill block at $ACFF with $1A
	INC HL	
	INC HL	
	LD (HL),$02	
	LD HL,MiniMap+6*32+31	; $AD3C = $AC5D + 6 * 32 + 31; row 6 column 31
	LD (HL),$06		; ($AD3C) <- $06
	INC HL	
	INC HL	
	INC HL			; HL = $AD3F
	LD B,$1C	
	LD A,$16	
	CALL FillBlock		; Fill block at $AD3F with $16
	INC HL	
	INC HL			; HL = $AD5D; $AD5D = $AC5D + $100: row 8 column 0
	LD B,$00	
	LD A,$01	
	CALL FillBlock		; Fill block at $AD5D with $01, 256 bytes: fill rows 8..15
	CALL FillBlock		; Fill block at $AE5D with $01, 256 bytes: fill rows 16..23
	CALL FillBlock		; Fill block at $AF5D with $01, 256 bytes: fill rows 24..31
	CALL NextRandom		; Calculate next random value
	LD A,H	
	AND $0F			; 0..15
	ADD A,$07		; A = (Random:H) & 15 + 7 => 7..22
	LD L,A			; column
	LD H,$03		; row = 3
	XOR A	
	CALL MiniMap_Set	; Calc address in MiniMap and Set block number = 0
	INC L			; next column; column = 4
	CALL MiniMap_Set	; Calc address in MiniMap and Set block number = 0
	LD H,$05		; row = 5
	CALL MiniMap_Set	; Calc address in MiniMap and set block number = 0
	DEC L			; previous column; column = 3
	CALL MiniMap_Set	; Calc address in MiniMap and Set block number = 0
	DEC H			; previous row; row = 4
	LD A,$1C		; $1C = place for Octopus, left block
	CALL MiniMap_Set	; Calc address in MiniMap and Set block number = $1C
	INC A			; = $1D = place for Octopus, right block
	INC L			; next column; column = 4
	CALL MiniMap_Set	; Calc address in MiniMap and Set block number = $1D
	LD HL,MiniMap+3*32	; $ACBD = $AC5D + 3 * 32: row 3 column 0
	LD B,$A0		; repeat 160 times = 5 rows, rows 3..7
L9E14	LD A,(HL)		; get block number
	CP $02			; block $02 is dead end to the left, exit to the right
	JR Z,L9E2A	
	CP $06			; block $06 is dead end to the right, exit to the left
	JR Z,L9E2A	
	CP $16			; block $16 looks like floor (seabed)
	JR Z,L9E2A	
	CP $1A			; block $1A looks like ceiling
	JR Z,L9E2A	
L9E25	INC HL			; to the next block
	DJNZ L9E14	
	JR L9E38	
; Block $02 / $06 / $16 / $1A
L9E2A	PUSH BC	
	PUSH HL	
	CALL NextRandom		; Calculate next random value
	BIT 5,H	
	POP HL	
	POP BC	
	JR Z,L9E25	
	INC (HL)		; increase block number
	JR L9E25		; continue the loop
; Coming here after the loop end; continue to build the map
L9E38	LD A,$02		; initial trunk width
	LD (L5B09),A		; set trunk width
	CALL NextRandom		; Calculate next random value
	LD A,H	
	AND $0F			; 0..15
	ADD A,$07		; 7..22
	LD L,A	
	LD H,$07		; row = 7
	XOR A	
	CALL MiniMap_Set	; Calc address in MiniMap and Set block number = 0
	INC L			; next column
	CALL MiniMap_Set	; Calc address in MiniMap and Set block number = 0
	INC H			; next row
	LD A,$1D		; $1D = place for Octopus, right block
	CALL MiniMap_Set	; Calc address in MiniMap and Set block number = $1D - Octopus place
	DEC A			; = $1C = place for Octopus, left block
	DEC L			; previous column
	CALL MiniMap_Set	; Calc address in MiniMap and Set block number = $1C - Octopus place
	DEC L			; previous column
; Building the labyrinth down from the hole with Octopus
	LD (L5B07),HL		; store the octopus place as the current trunk position
L9E5F	LD HL,(L5B07)		; get current trunk position
	INC H			; next row
	LD (L5B07),HL		; set current trunk position
	LD A,(L5B09)		; get trunk width
	LD (L5B0A),A		; and save it
	LD A,(LEVEL)		; LEVEL
	ADD A,A			; *2
	ADD A,A			; *4
	ADD A,$10		; now A = [Game level] * 4 + 16 => 20 / 24 / 28 / 32
	LD C,A	
	DEC C			; now C = 19 / 23 / 27 / 31 - row where the labirinth ends
	CP H			; row = A ?
	JR NZ,L9E99		; no => jump
; The labyrinth ends on this depth	
	DEC H			; previous row
	LD A,(L5B09)		; get trunk width
	LD B,A			; B = loop counter
L9E7D	PUSH HL	
	PUSH BC	
	CALL NextRandom		; Calculate next random value
	LD D,H	
	POP BC	
	POP HL	
	INC L			; next column
	LD A,$16		; block $16 is floor (seabed)
	BIT 1,D	
	JR Z,L9E8D	
	INC A			; change to block $17 - a bit different floor (seabed)
L9E8D	CALL MiniMap_Set	; Calc address in MiniMap and Set block number
	DJNZ L9E7D	
	CALL LA193	
	CALL LB1D4	
	RET	
; Fill the labyrinth on this row level
L9E99	PUSH HL	
	PUSH BC	
	CALL NextRandom		; Calculate next random value
	POP BC	
	POP DE	
	EX DE,HL	
	LD A,D	
	AND $07			; 0..7
	JR NZ,L9EB3	
L9EA6	LD A,$02		; block $02 is dead end to the left, exit to the right
	BIT 6,D	
	JR Z,L9EAD	
	INC A			; change to block $03, same as $02 with a bit different relief
L9EAD	CALL MiniMap_Set	; Calc address in MiniMap and Set block number
	JP L9F0E
L9EB3	LD A,E	
	AND $C0	
	JR NZ,L9EFB	
L9EB8	BIT 4,E	
	JR Z,L9ED8	
	LD A,L	
	OR A	
	JR Z,L9EA6	
	LD A,H	
	CP C	
	JR Z,L9ED8	
	LD A,$05		; block $05 is wall to left/up, exit to right/down
	CALL MiniMap_Set	; Calc address in MiniMap and Set block number
	DEC L			; previous column
	LD (L5B07),HL		; set current trunk position
	INC L			; next column
	LD A,(L5B0A)	
	INC A	
	LD (L5B0A),A	
	JP L9F0E	
L9ED8	LD A,(L5B09)		; get trunk width
	ADD A,L	
	CP $1E	
	JR Z,L9EA6	
	LD A,(L5B0A)	
	DEC A	
	JR Z,L9EA6	
	LD (L5B0A),A	
	LD A,(L5B09)	
	DEC A			; decrease trunk width
	LD (L5B09),A	
	INC L			; next column
	LD (L5B07),HL		; set current trunk position
	LD A,$04		; block $04 is wall to left/down, exit to rigth/up
	CALL MiniMap_Set	; Calc address in MiniMap and Set block number
	JR L9F0E	
L9EFB	DEC L			; previous column
	CALL MiniMap_Check	; Check value in MiniMap table
	PUSH AF	
	INC L			; next column
	POP AF	
	JR NZ,L9EB8	
	LD A,$0B		; block $0B is floor and ceiling, passage to left/right
	CALL MiniMap_Set	; Calc address in MiniMap and Set block number = $0B
	PUSH BC	
	CALL L9F81	
	POP BC	
L9F0E	LD A,(L5B09)		; get trunk width
	LD B,A			; B = loop counter
	XOR A	
L9F13	INC L			; next column
	CALL MiniMap_Set	; Calc address in MiniMap and Set block number = 0
	DJNZ L9F13	
	INC L			; next column
	PUSH HL	
	PUSH BC	
	CALL NextRandom		; Calculate next random value
	POP BC	
	POP DE	
	EX DE,HL	
	LD A,D	
	AND $07			; 0..7
	JR NZ,L9F34	
L9F27	LD A,$06		; block $06 is dead end to the right, exit to the left
	BIT 6,D	
	JR NZ,L9F2E	
	INC A			; change to block $07 - same as $06 with a bit different relief
L9F2E	CALL MiniMap_Set	; Calc address in MiniMap and Set block number
	JP L9F78	
L9F34	LD A,E	
	AND $C0	
	JR NZ,L9F67	
L9F39	BIT 4,E	
	JR Z,L9F4F	
	LD A,(L5B0A)	
	DEC A	
	JR Z,L9F4F	
	LD (L5B0A),A	
	DEC L	
	LD A,$08		; block $08 is wall to right/down, exit to left/up
	CALL MiniMap_Set	; Calc address in MiniMap and Set block number = $08
	JP L9F78	
L9F4F	LD A,L	
	CP $1E	
	JR NC,L9F27	
	LD A,H	
	CP C	
	JR Z,L9F27	
	LD A,$09		; block $09 is wall to right/up, exit to left/down
	CALL MiniMap_Set	; Calc address in MiniMap and Set block number = $09
	LD A,(L5B0A)	
	INC A	
	LD (L5B0A),A	
	JP L9F78	
L9F67	INC L			; next column
	CALL MiniMap_Check	; Check value in MiniMap table
	PUSH AF	
	DEC L			; previous column
	POP AF	
	JR NZ,L9F39	
	LD A,$0A		; block $0A is floor and ceiling, passage to left/right
	CALL MiniMap_Set	; Calc address in MiniMap and Set block number = $0A
	CALL LA03B	
L9F78	LD A,(L5B0A)	
	LD (L5B09),A		; restore trunk width
	JP L9E5F		; continue the loop by mini-map rows

L9F81	LD (L5B01),HL	
L9F84	DEC L	
	PUSH HL	
	CALL NextRandom		; Calculate next random value
	POP DE	
	EX DE,HL	
	LD A,D	
	AND $02			; check bit 1
	JR NZ,L9FEB	
L9F90	DEC L	
	CALL MiniMap_Check	; Check value in MiniMap table
	JR Z,L9FC0	
	LD A,L	
	CP $FF	
	JR NZ,L9FB6	
	LD L,$1F	
	CALL MiniMap_Get	; Calc address in MiniMap and Get block number
	CP $14	
	JR NZ,L9FB4	
	LD A,$0B	
	CALL MiniMap_Set	; Calc address in MiniMap and Set block number
	LD L,$00	
	LD A,$0A	
	CALL MiniMap_Set	; Calc address in MiniMap and Set block number
	LD HL,(L5B01)	
	RET	
L9FB4	LD L,$FF	
L9FB6	INC L	
	LD A,$15	
	CALL MiniMap_Set	; Calc address in MiniMap and Set block number
	LD HL,(L5B01)	
	RET	
L9FC0	INC L	
	BIT 7,E	
	JR Z,L9FE0	
	DEC H	
	CALL MiniMap_Get	; Calc address in MiniMap and Get block number
	INC H	
	SUB $10	
	JR C,L9FE0	
	CP $04	
	JR NC,L9FE0	
	LD A,$18	
	CALL MiniMap_Set	; Calc address in MiniMap and Set block number
	DEC H	
	INC A	
	CALL MiniMap_Set	; Calc address in MiniMap and Set block number
	INC H	
	JP L9F84	
L9FE0	LD A,D	
	AND $03			; 0..3
	ADD A,$10		; $10..$13
	CALL MiniMap_Set	; Calc address in MiniMap and Set block number
	JP L9F84	
L9FEB	BIT 6,E	
	JR NZ,LA011	
	INC H	
	CALL MiniMap_Check	; Check value in MiniMap table
	JR Z,L9FF8	
	DEC H	
	JR L9F90	
L9FF8	DEC L	
	CALL MiniMap_Check	; Check value in MiniMap table
	JR Z,LA002	
	INC L	
	DEC H	
	JR L9F90	
LA002	INC L	
	LD A,$0D	
	CALL MiniMap_Set	; Calc address in MiniMap and Set block number
	DEC H	
	DEC A	
	CALL MiniMap_Set	; Calc address in MiniMap and Set block number
	INC H	
	JP L9F84	
LA011	LD A,$09	
	CP H	
	JP Z,L9F90	
	DEC H	
	CALL MiniMap_Check	; Check value in MiniMap table
	JR Z,LA021	
	INC H	
	JP L9F90	
LA021	DEC L	
	CALL MiniMap_Check	; Check value in MiniMap table
	JR Z,LA02C	
	INC L	
	INC H	
	JP L9F90	
LA02C	LD A,$0E	
	INC L	
	CALL MiniMap_Set	; Calc address in MiniMap and Set block number
	INC H	
	INC A	
	CALL MiniMap_Set	; Calc address in MiniMap and Set block number
	DEC H	
	JP L9F84	

LA03B	LD (L5B01),HL	
LA03E	INC L	
	PUSH HL	
	CALL NextRandom		; Calculate next random value
	POP DE	
	EX DE,HL	
	LD A,D	
	AND $04			; check bit 2
	JR NZ,LA0A5	
LA04A	INC L	
	CALL MiniMap_Check	; Check value in MiniMap table
	JR Z,LA079	
	LD A,L	
	AND $1F			; 0..31
	JR NZ,LA06F	
	LD L,A	
	CALL MiniMap_Get	; Calc address in MiniMap and Get block number
	CP $15	
	JR NZ,LA06D	
	LD A,$0A	
	CALL MiniMap_Set	; Calc address in MiniMap and Set block number
	LD L,$1F	
	LD A,$0B	
	CALL MiniMap_Set	; Calc address in MiniMap and Set block number
	LD HL,(L5B01)	
	RET	
LA06D	LD L,$20	
LA06F	DEC L	
	LD A,$14	
	CALL MiniMap_Set	; Calc address in MiniMap and Set block number
	LD HL,(L5B01)	
	RET	
LA079	DEC L	
	BIT 7,E	
	JR Z,LA09A	
	DEC H	
	CALL MiniMap_Get	; Calc address in MiniMap and Get block number
	INC H	
	SUB $10	
	JR C,LA09A	
	CP $04	
	JR NC,LA09A	
	LD A,$18	
	CALL MiniMap_Set	; Calc address in MiniMap and Set block number
	DEC H	
	LD A,$19	
	CALL MiniMap_Set	; Calc address in MiniMap and Set block number
	INC H	
	JP LA03E	
LA09A	LD A,D	
	AND $03			; 0..3
	ADD A,$10		; $10..$13
	CALL MiniMap_Set	; Calc address in MiniMap and Set block number
	JP LA03E	
LA0A5	BIT 6,E	
	JR NZ,LA0CB	
	INC H	
	CALL MiniMap_Check	; Check value in MiniMap table
	JR Z,LA0B2	
	DEC H	
	JR LA04A	
LA0B2	INC L	
	CALL MiniMap_Check	; Check value in MiniMap table
	JR Z,LA0BC	
	DEC L	
	DEC H	
	JR LA04A	
LA0BC	DEC L	
	LD A,$0F	
	CALL MiniMap_Set	; Calc address in MiniMap and Set block number
	DEC H	
	DEC A	
	CALL MiniMap_Set	; Calc address in MiniMap and Set block number
	INC H	
	JP LA03E	
LA0CB	LD A,$09	
	CP H	
	JP Z,LA04A	
	DEC H	
	CALL MiniMap_Check	; Check value in MiniMap table
	JR Z,LA0DB	
	INC H	
	JP LA04A	
LA0DB	INC L	
	CALL MiniMap_Check	; Check value in MiniMap table
	JR Z,LA0E6	
	INC H	
	DEC L	
	JP LA04A	
LA0E6	LD A,$0C	
	DEC L	
	CALL MiniMap_Set	; Calc address in MiniMap and Set block number
	INC H	
	INC A	
	CALL MiniMap_Set	; Calc address in MiniMap and Set block number
	DEC H	
	JP LA03E	

; Calculate address in relief block
; I: HL = Char coords: H = row, L = column
LA0F5	PUSH HL
	SRL H
	SRL H
	SRL H
	SRL H
	RR L
	SRL H
	RR L
	SRL H
	RR L
	LD DE,MiniMap	
	ADD HL,DE		; now HL = address in MiniMap
	LD H,(HL)		; get block number from MiniMap
	POP DE			; restore row/column
	LD A,E			; get column
	AND $07			; 0..7
	LD E,A	
	LD A,D			; get row
	RRCA	
	RRCA	
	RRCA	
	AND $E0	
	LD L,A	
	SRL H	
	RR L	
	SRL H	
	RR L	
	LD A,L	
	OR E	
	LD L,A	
	LD DE,LA4DD		; address of relief bblocks
	ADD HL,DE		; now HL = address in the block
	RET	

; Calculate address in relief block and Get tile nummber
; I: HL = Char coords: H = row, L = column
GetTileInBlock
	PUSH HL	
	PUSH DE	
	CALL LA0F5		; Calculate address in relief block
	LD A,(HL)		; get tile number
	POP DE	
	POP HL	
	RET

LA132	DEFB $E5,$D5,$F5,$CD,$F5,$A0,$F1,$5F	
	DEFB $7E,$E6,$3F,$7B,$D1,$E1,$C9,$E5	
	DEFB $D5,$F5,$CD,$F5,$A0,$F1,$D1,$BE	
	DEFB $E1,$C9

; Get screen attribute address
GetScrAttrAddr
	PUSH DE	
	LD A,L	
	LD L,$00	
	SRL H	
	RR L	
	SRL H	
	RR L	
	SRL H	
	RR L	
	OR L	
	LD L,A	
	LD DE,$5800		; Screen attributes area
	ADD HL,DE	
	POP DE	
	RET

; Convert char coords HL to ZX screen address
; I: HL = Char coords: H = row 0..23, L = column 0..31
; O: HL	= address on the ZX screen
GetScrAddr
	PUSH DE	
	LD A,H	
	LD D,A	
	AND $18			; 0 / 8 / 16 / 24
	SET 6,A	
	LD H,A	
	LD A,D	
	RRCA	
	RRCA	
	RRCA	
	AND $E0	
	OR L	
	LD L,A	
	POP DE	
	RET

; Draw tile 16x8 at the screen
; I: HL = Char coords: H = row 0..23, L = column 0..31
; I: DE = Tile address; 16 bytes
DrawTile16x8
	CALL GetScrAddr		; Convert char coords HL to ZX screen address
	LD B,$08		; repeat 8 times
LA17B	LD A,(DE)	
	LD (HL),A	
	INC DE	
	INC L	
	LD A,(DE)	
	LD (HL),A	
	DEC L	
	INC H	
	INC DE	
	DJNZ LA17B	
	RET

; Draw tile 8x8 at the screen
; I: HL = Char coords: H = row 0..23, L = column 0..31
; I: DE = Tile address; 8 bytes
DrawTile8x8
	CALL GetScrAddr		; Convert char coords HL to ZX screen address
	LD B,$08		; repeat 8 times
LA18C	LD A,(DE)	
	LD (HL),A	
	INC DE	
	INC H	
	DJNZ LA18C	
	RET

LA193	LD HL,$20FF	
	LD DE,TableStatics	; Table of static objects on the map
	XOR A	
	LD (L5B00),A	
	LD A,$03	
	LD (L5B0F),A	
LA1A2	INC L	
	LD A,L	
	AND $1F			; 0..31
	JR NZ,LA1B0	
	LD L,A	
	DEC H	
	JR NZ,LA1B0	
	LD A,$80		; end of list marker
	LD (DE),A	
	RET	
LA1B0	CALL MiniMap_Get	; Calc address in MiniMap and Get block number
	LD C,A	
	PUSH HL	
	CP $14	
	JR Z,LA1CC	
	CP $15	
	JR NZ,LA1F6	
	LD A,L	
	OR A	
	JR NZ,LA1F6	
	LD L,$04	
	LD A,H	
	ADD A,A	
	ADD A,A	
	ADD A,A	
	ADD A,$04	
	LD H,A	
	JR LA1DA	
LA1CC	LD A,L	
	CP $1F	
	JR NZ,LA1F6	
	LD L,$FB	
	LD A,H	
	ADD A,A	
	ADD A,A	
	ADD A,A	
	ADD A,$06	
	LD H,A	
LA1DA	LD A,(L5B0F)	
	OR A	
	JR Z,LA1F6	
	LD A,$40	
	LD (DE),A	
	INC DE	
	LD A,L	
	LD (DE),A	
	INC DE	
	LD A,H	
	LD (DE),A	
	INC DE	
	LD A,(L5B0F)	
	LD (DE),A	
	INC DE	
	DEC A	
	LD (L5B0F),A	
	POP HL	
	JR LA1A2	
LA1F6	LD H,C	
	LD L,$C0	
	SRL H	
	RR L	
	SRL H	
	RR L	
	LD BC,LA4DD	
	PUSH HL	
	ADD HL,BC	
	POP BC	
	LD B,$10		; repeat 16 times
LA209	LD A,(HL)	
	INC HL	
	INC C	
	SUB $1C	
	JR C,LA278	
	CP $04	
	JR NC,LA278	
	LD A,(HL)	
	INC HL	
	INC C	
	SUB $1C	
	JR C,LA278	
	CP $04	
	JR NC,LA278	
	LD A,C	
	AND $07			; 0..7
	JR Z,LA278	
	PUSH HL	
	PUSH DE	
	PUSH BC	
	CALL NextRandom		; Calculate next random value
	POP BC	
	POP DE	
	LD A,H	
	POP HL	
	AND $02			; check bit 1
	JR NZ,LA278	
	LD A,(L5B00)	
	INC A	
	CP $64	
	JR Z,LA278	
	LD (L5B00),A	
	LD A,$0A	
	LD (DE),A	
	EX (SP),HL	
	LD A,H	
	CP $04	
	JR C,LA249	
	LD A,$00	
	LD (DE),A	
LA249	INC DE	
	LD A,C	
	DEC A	
	DEC A	
	AND $07			; 0..7
	PUSH DE	
	LD D,A	
	LD A,L	
	ADD A,A	
	ADD A,A	
	ADD A,A	
	AND $F8	
	OR D	
	POP DE	
	LD (DE),A	
	INC DE	
	PUSH DE	
	LD A,H	
	ADD A,A	
	ADD A,A	
	ADD A,A	
	AND $F8	
	LD D,A	
	LD A,C	
	AND $38	
	RRCA	
	RRCA	
	RRCA	
	OR D	
	POP DE	
	EX (SP),HL	
	LD (DE),A	
	INC DE	
	LD A,(RANDOM)		; get current Random lo byte
	AND $7F			; 0..127
	LD (DE),A	
	INC DE	
	INC HL	
	INC C	
	DEC B	
LA278	DJNZ LA209	
	POP HL	
	JP LA1A2	

; Table of static objects on the map - oxygen, chests, shells
; each record is 4 bytes wide: byte #0 = flags; byte #1 = column 0..255, byte #2 = row 0..255
; flags: bit 1 = big/small;
;        bit 3 = opened/closed;
;        bit 5 = oxygen;
;        bit 6 = chest;
;        bit 7 = end of list
TableStatics
	DEFB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
LA40C	DEFB $B2,$1D,$EA,$53,$DE,$69,$DE,$01,$53,$48,$46,$B5,$7A,$EA,$5E

LA41B	DEFB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00

;----------------------------------------------------------------------------

	INCLUDE "scubarelf.asm"

;----------------------------------------------------------------------------

; Table of objects on the screen
; Bytes #0,1: record address in TableStatics; bytes #2,3: column and row
LB07D	DEFB $01,$01,$01,$01	
	DEFB $01,$21,$01,$01	
	DEFB $01,$01,$01,$01	
	DEFB $01,$0C,$01,$09	
	DEFB $14,$16,$17,$0D	
	DEFB $01,$32,$14,$00	
	DEFB $00,$00,$00,$00	
	DEFB $23,$32,$00,$00	
	DEFB $00,$00,$00,$00	
	DEFB $0C,$32,$06,$27	
	DEFB $03,$00,$00,$00	

; Draw static objects on the screen; prepare LB07D table
LB0A9	LD DE,(L5B03)		; get Screen position on mini-map
	LD A,D			; get row
	RLCA	
	RLCA	
	RLCA			; *8
	LD D,A	
	LD A,E	
	RLCA	
	RLCA	
	RLCA			; *8
	LD E,A	
	LD (L5B0B),DE		; set screen position on 256x256 map
	LD HL,TableStatics	; Table of static objects on the map
	DI	
	PUSH IY	
	LD IY,LB07D		; Table of objects on the screen
LB0C5	BIT 7,(HL)		; end of objects list?
	JR Z,LB0D0		; no => jump
	LD (IY+$01),$FF		; address hi := 255 - end of list marker in LB07D table
	POP IY	
	RET	
LB0D0	PUSH HL	
	PUSH DE	
	RES 0,(HL)	
	INC HL	
	LD A,(HL)		; get column 0..255
	SUB E			; minus screen row
	CP $18			; within the screen?
	JR NC,LB148		; no => skip
	LD E,A			; now E = column on the screen, 0..23
	INC HL	
	LD A,(HL)		; get row 0..255
	SUB D			; minus screen column
	CP $18			; within the screen?
	JR NC,LB148		; no => skip
	LD D,A			; now D = row on the screen, 0..23
	PUSH DE	
	DEC HL			; back to column 
	DEC HL			; back to flags
	LD (IY+$00),L	
	LD (IY+$01),H		; store record address
	LD (IY+$02),E	
	LD (IY+$03),D		; store position
	INC IY	
	INC IY	
	INC IY	
	INC IY			; next record
	SET 0,(HL)	
	LD C,$46		; screen attribute for chest
	LD DE,LB1B4		; Sprite 16x8 Chest
	BIT 6,(HL)		; check "chest" bit
	JR NZ,LB151		; chest => draw the chest
	LD C,$47		; screen attribute for oxygen
	LD DE,LB1AC		; Sprite 8x8 Oxygen
	BIT 5,(HL)		; check "oxygen" bit
	JR NZ,LB160		; oxygen => draw the oxygen
	LD C,$07		; screen attribute for shells
	BIT 1,(HL)		; check "big/small" bit
	JR Z,LB121		; big => draw big shell
	LD DE,LB1CC		; Sprite 8x8 Small shell opened
	BIT 3,(HL)		; check "closed/opened" bit
	JR Z,LB160		; open => draw the small shell
	LD DE,LB1C4		; Sprite 8x8 Small shell closed
	JR LB160		; => draw the small shell
; Draw Big shell	
LB121	LD DE,LB16C		; Sprite 16x16 Big shell opened
	BIT 3,(HL)		; check "closed/opened" bit
	JR Z,LB12B	
	LD DE,LB18C		; Sprite 16x16 Big shell closed
LB12B	POP HL			; restore (row, column)
	PUSH HL			; save again
	CALL GetScrAttrAddr	; Get screen attribute address
	LD (HL),C		; set screen attribute
	INC HL	
	LD (HL),C		; set screen attribute
	PUSH BC	
	LD BC,$0020	
	OR A	
	SBC HL,BC		; one char line up
	POP BC	
	LD (HL),C		; set screen attribute
	DEC HL	
	LD (HL),C		; set screen attribute
	POP HL			; restore (row, column)
	PUSH HL			; save again
	CALL DrawTile16x8	; Draw tile 16x8 at the screen
	POP HL			; restore (row, column)
	DEC H			; One char line up
	CALL DrawTile16x8	; Draw tile 16x8 at the screen
LB148	POP DE	
	POP HL	
	LD BC,$0004		; record size
	ADD HL,BC		; next record
	JP LB0C5		; continue the loop
; Draw Chest object
LB151	LD A,(HL)	
	POP HL			; restore (row, column)
	PUSH HL			; save again
	CALL GetScrAttrAddr	; Get screen attribute address
	LD (HL),C		; set screen attribute
	INC HL	
	LD (HL),C		; set screen attribute
	POP HL			; restore (row, column)
	CALL DrawTile16x8	; Draw tile 16x8 at the screen
	JR LB148	
; Draw Oxygen or Small shell
LB160	POP HL			; restore (row, column)
	PUSH HL			; save again
	CALL GetScrAttrAddr	; Get screen attribute address
	LD (HL),C		; set screen attribute
	POP HL			; restore (row, column)
	CALL DrawTile8x8	; Draw tile 8x8 at the screen
	JR LB148	

;----------------------------------------------------------------------------
; Sprites for objects

; Sprite 16x16 Big shell opened; first lower part then upper
LB16C	DEFB $60,$1C,$60,$0E,$2C,$1E,$36,$1C	; sprB16C
	DEFB $36,$0C,$16,$C4,$0B,$6C,$05,$B8
LB17C	DEFB $00,$00,$00,$60,$00,$78,$00,$38	; sprB17C
	DEFB $00,$3C,$00,$0E,$40,$36,$40,$3B

; Sprite 16x16 Big shell closed; first lower part then upper
LB18C	DEFB $63,$10,$61,$E0,$2C,$F8,$36,$78	; sprB18C
	DEFB $36,$1C,$16,$C4,$0B,$6C,$05,$B8
LB19C	DEFB $00,$00,$00,$00,$00,$00,$0C,$00	; sprB19C
	DEFB $3F,$00,$0F,$80,$4F,$80,$40,$E0

; Sprite 8x8 Oxygen
LB1AC	DEFB $30,$10,$7C,$6C,$6C,$6C,$7C,$6C	; sprB1AC

; Sprite 16x8 Chest
LB1B4	DEFB $0F,$F8,$1F,$FC,$00,$00,$1F,$FC	; sprB1B4
	DEFB $1F,$7C,$1F,$7C,$1F,$FC,$1F,$FC

; Sprite 8x8 Small shell closed
LB1C4	DEFB $00,$00,$00,$7C,$FE,$06,$FE,$7C	; sprB1C4

; Sprite 8x8 Small shell opened
LB1CC	DEFB $02,$03,$03,$03,$03,$03,$FE,$7C	; sprB1CC

;----------------------------------------------------------------------------

LB1D4	PUSH IY	
	NOP	
	NOP	
	NOP	
	NOP	
	XOR A	
	LD (LB210),A	
LB1DE	LD IY,TableStatics	; Table of static objects on the map
LB1E2	CALL NextRandom		; Calculate next random value
	LD A,H	
	AND $03			; 0..3
	ADD A,$03		; 3..6
	LD B,A	
	LD DE,$0004		; record size
LB1EE	ADD IY,DE		; next record
	DJNZ LB1EE	
	LD A,(IY+$02)		; get row 0..255
	CP $48			; < 72 ? (top 9 blocks = top 3 screens)
	JR C,LB1DE		; yes => jump
	LD A,(IY+$00)		; get flags
	AND $60	
	JR NZ,LB1E2	
	LD (IY+$00),$20	
	LD HL,LB210	
	INC (HL)	
	LD A,(HL)	
	CP $08	
	JR NZ,LB1E2	
	POP IY	
	RET	

LB210	DEFB $00
LB211	DEFB $08
LB212	DEFB $05

LB213	CALL LB2C0		; Octopus delay and process
	LD HL,LB211	
	DEC (HL)	
	RET NZ	
	DI	
	LD A,(L5B0D)		; get value 7 / 5 / 3 / 1, depending on LEVEL
	LD (HL),A	
	LD A,(LB212)	
	BIT 7,A	
	JR NZ,LB230	
	INC A	
	CP $07	
	JR NZ,LB237	
	SET 7,A	
	JR LB237	
LB230	DEC A	
	CP $83	
	JR NZ,LB237	
	RES 7,A	
LB237	LD (LB212),A	
	PUSH IY	
	LD IY,LB079	
LB240	LD DE,$0004	
	LD BC,(L5B0B)		; get screen position on 256x256 map
LB247	ADD IY,DE	
	LD L,(IY+$00)	
	LD H,(IY+$01)	
	LD A,$FF	
	CP H	
	JR NZ,LB257	
	POP IY	
	RET	
LB257	BIT 5,(HL)	
	JR NZ,LB247	
	BIT 6,(HL)	
	JR Z,LB276	
	BIT 3,(HL)	
	JR NZ,LB247	
	LD L,(IY+$02)	
	LD H,(IY+$03)	
	CALL GetScrAttrAddr	; Get screen attribute address
	LD A,(LB212)	
	AND $07			; 0..7
	LD (HL),A	
	INC HL	
	LD (HL),A	
	JR LB240	
LB276	BIT 3,(HL)	
	JR Z,LB27E	
	BIT 4,(HL)	
	JR NZ,LB247	
LB27E	ADD HL,DE	
	DEC HL	
	DEC (HL)	
	JR Z,LB286	
	INC HL	
	JR LB247	
LB286	PUSH HL	
	PUSH DE	
	PUSH BC	
	CALL NextRandom		; Calculate next random value
	POP BC	
	POP DE	
	LD A,H	
	POP HL	
	AND $7F			; 0..127
	ADD A,$80		; 128..255
	LD (HL),A	
	DEC HL	
	LD A,(HL)	
	SUB B	
	LD D,A	
	DEC HL	
	LD A,(HL)	
	SUB C	
	LD E,A	
	DEC HL	
	LD A,$08	
	XOR (HL)	
	LD (HL),A	
	BIT 1,(HL)	
	PUSH HL	
	EX DE,HL	
	JR Z,LB2B1	
	LD DE,LB2EE		; sprite 8x8 address, 8 bytes
	CALL DrawTileXor8x8	; Draw tile 8x8 with XOR
	POP HL	
	JR LB240	
LB2B1	LD DE,LB2F6		; sprite 16x16 address, 32 bytes
	PUSH HL	
	CALL DrawTileXor16x8	; Draw tile 16x8 with XOR
	POP HL	
	DEC H			; one line upper
	CALL DrawTileXor16x8	; Draw tile 16x8 with XOR
	POP HL	
	JR LB240	

; Octopus delay and process
LB2C0	LD HL,LB2CD	
	DEC (HL)	
	RET NZ	
	LD A,(L5B0E)		; get value 10 / 8 / 6 / 4, depending on LEVEL
	LD (HL),A	
	CALL LB317		; Process Octopus, draw if needed
	RET	
LB2CD	DEFB $08

; Draw tile 8x8 with XOR
; I: HL = Char coords: H = 0..23, L = 0..31
; I: DE = Tile address
DrawTileXor8x8
	CALL GetScrAddr		; Convert char coords HL to ZX screen address
	LD B,$08		; tile height 8 pixels
LB2D3	LD A,(DE)	
	XOR (HL)	
	LD (HL),A	
	INC DE	
	INC H	
	DJNZ LB2D3	
	RET

; Draw tile 16x8 with XOR
; I: HL = Char coords: H = 0..23, L = 0..31
; I: DE = Tile address
DrawTileXor16x8
	CALL GetScrAddr		; Convert char coords HL to ZX screen address
	LD B,$08		; tile height 8 pixels
LB2E0	LD A,(DE)	
	XOR (HL)	
	LD (HL),A	
	INC DE	
	INC L	
	LD A,(DE)	
	XOR (HL)	
	LD (HL),A	
	DEC L	
	INC H	
	INC DE	
	DJNZ LB2E0	
	RET	

; Sprite 8x8 address, 8 bytes
LB2EE	DEFB $02,$03,$03,$7F,$FD,$05,$00,$00

; Sprite 16x16, 32 bytes; first lower part then upper
LB2F6	DEFB $03,$0C,$01,$EE,$00,$E6,$00,$64	; sprB2F6
	DEFB $00,$10,$00,$00,$00,$00,$00,$00
LB306	DEFB $00,$00,$00,$60,$00,$78,$0C,$38	; sprB306
	DEFB $3F,$3C,$0F,$8E,$0F,$B6,$00,$DB
; Data block at B316
LB316	DEFB $09	

; Process Octopus, draw if needed
LB317	LD A,(L9C50)		; get Octopus flag
	OR A	
	RET Z	
	LD HL,LB316	
	DEC (HL)	
	RET NZ	
	LD (HL),$10	
	LD A,(L9C55)		; get Octopus phase
	LD B,A	
	BIT 7,A	
	JR NZ,LB339	
	LD A,R	
	JR Z,LB345	
	INC B	
	LD A,$05	
	CP B	
	JR NZ,LB345	
	LD B,$83	
	JR LB345	
LB339	LD A,R	
	JR Z,LB345	
	DEC B	
	LD A,$7F	
	CP B	
	JR NZ,LB345	
	LD B,$01	
LB345	LD A,B
;
; Entry point to draw Octopus
; I: A = Octopus phase
LB346	LD (L9C55),A		; set Octopus phase
	LD HL,L8D74		; Base address for Octopus sprites
	AND $07			; 0..7
	LD B,A	
	OR A	
	JR Z,LB358	
	LD DE,$00C0		; address shift between phases
LB355	ADD HL,DE	
	DJNZ LB355	
LB358	EX DE,HL		; now DE = Octopus sprite address
	CALL LB35D		; Draw Octopus sprite on the screen
	RET	

; Draw Octopus sprite on the screen
; I: DE = Octopus sprite address, 6x4 tiles 8x8 pixels, 192 bytes
LB35D	DI	
	PUSH IY	
	LD IY,(L9C53)	
	LD (L9C53),DE	
	LD HL,(L9C51)		; get the Octopus screen address
	INC HL	
	INC HL	
	LD C,$04		; repeat 4 times - height
LB36F	LD B,$06		; repeat 6 times - width
	PUSH HL	
LB372	PUSH BC	
	LD B,$08		; repeat 8 times
	PUSH HL	
LB376	LD A,(DE)		; get pixels
	XOR (HL)		; XOR with screen pixels
	XOR (IY+$00)	
	LD (HL),A		; write to the screen
	INC DE	
	INC IY	
	INC H			; next pixel row
	DJNZ LB376	
	POP HL	
	INC L			; next column
	POP BC	
	DJNZ LB372		; continue by columns
	POP HL	
	LD A,L	
	ADD A,$20	
	LD L,A	
	DEC C	
	JR NZ,LB36F		; continue by rows
	POP IY	
	RET

LB392	DEFB $00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $32,$32,$32,$1D,$02,$32

; I: IX = record address in block LC4F0
; I: IY = ???
LB3A0	BIT 4,(IX+$0D)	
	JR Z,LB3AB
;
LB3A6	LD HL,(LB7B9)	
	JR LB3B1
;
LB3AB	LD H,(IX+$08)	
	LD L,(IX+$07)	
LB3B1	LD C,(IY+$0A)	
	LD A,(IX+$03)	
	OR A	
	JR NZ,LB3D6	
	SET 6,(IX+$0D)	
	BIT 3,(IX+$0D)	
	JR Z,LB3CE	
	BIT 7,(IX+$12)	
	JR NZ,LB3E9	
	LD A,$08	
	JR LB3D6	
LB3CE	BIT 7,(IX+$12)	
	JR Z,LB3E9	
	LD A,$08	
LB3D6	LD B,A	
	XOR A	
LB3D8	ADD A,(IY+$07)	
	DJNZ LB3D8	
	LD B,A	
	LD C,A	
	LD A,(IY+$0A)	
	SUB C	
	LD C,A	
	XOR A	
LB3E5	LD (HL),A	
	INC HL	
	DJNZ LB3E5	
LB3E9	PUSH HL	
	LD D,(IX+$0C)	
	LD E,(IX+$0B)	
	LD A,(DE)	
	INC A	
	JR NZ,LB3FA	
	LD D,(IX+$0A)	
	LD E,(IX+$09)	
LB3FA	LD (L5B11),DE	
	LD B,(IY+$0B)	
LB401	PUSH BC	
	LD C,(IY+$06)	
	LD B,$00	
	EX DE,HL	
	LDIR	
	EX DE,HL	
	LD (HL),$00	
	INC HL	
	POP BC	
	DJNZ LB401	
	LD B,C	
	LD A,B	
	OR A	
	JR Z,LB41B	
	XOR A	
LB417	LD (HL),A	
	INC HL	
	DJNZ LB417	
LB41B	LD (IX+$0C),D	
	LD (IX+$0B),E	
	LD B,(IX+$02)	
	LD A,B	
	OR A	
	JR NZ,LB446	
	SET 5,(IX+$0D)	
	BIT 4,(IX+$0D)	
	JR Z,LB43E	
	BIT 7,(IX+$11)	
	JR Z,LB43A	
LB438	POP HL	
	RET	
LB43A	LD B,$08	
	JR LB446	
LB43E	BIT 7,(IX+$11)	
	JR NZ,LB43A	
	JR LB438	
LB446	LD A,B	
	CP $04	
	JR C,LB45C	
	SUB $04	
	LD C,A	
	LD B,(IY+$0D)	
	POP HL	
	PUSH HL	
	XOR A	
LB454	RRD	
	INC HL	
	DJNZ LB454	
	LD B,C	
	JR LB446	
LB45C	OR A	
	JR Z,LB438	
LB45F	POP HL	
	PUSH HL	
	PUSH BC	
	LD B,(IY+$0D)	
	OR A	
LB466	RR (HL)	
	INC HL	
	DJNZ LB466	
	POP BC	
	DJNZ LB45F	
	POP HL	
	RET

; I: IX = record address in block LC4F0
; I: IY = record address in block LC092
LB470	BIT 0,(IX+$0D)	
	JR Z,LB479	
	CALL LB3A0	
LB479	LD D,(IX+$08)	
	LD E,(IX+$07)		; get address
	BIT 1,(IX+$0D)	
	JR NZ,LB496	
	PUSH DE	
	LD C,(IY+$09)	
	LD L,(IY+$0A)		; get record size ??
	XOR A	
LB48D	LD B,L			; loop counter = record size
LB48E	LD (DE),A		; clear the record
	INC DE	
	DJNZ LB48E	
	DEC C	
	JR NZ,LB48D	
	POP DE	
LB496	LD HL,(LB7B9)	
	BIT 0,(IX+$0D)	
	JR NZ,LB4A2	
	LD HL,LA41B	
LB4A2	LD B,(IY+$07)	
LB4A5	LD A,(IY+$02)		; get column
	AND $E0			; check column for 0..31 range
	JR Z,LB4B4	
	INC (IY+$02)		; incremment column
	INC DE	
	INC HL	
	DEC B	
	JR LB4A5	
LB4B4	LD A,(IY+$06)	
	ADD A,(IY+$02)		; add column
	LD C,A	
LB4BB	CP $18			; < 24 ?
	JR C,LB4C4	
	DEC C	
	DEC B	
	LD A,C	
	JR LB4BB	
LB4C4	LD (IY+$0E),B		; set width
	LD A,(IY+$07)	
	SUB B	
	LD (IY+$0F),A	
	PUSH IX	
	PUSH HL	
	POP IX	
	LD B,(IY+$09)	
LB4D6	PUSH BC	
LB4D7	LD A,(IY+$03)		; get row
	CP $18			; < 24 ?
	JR C,LB4F0	
	INC (IY+$03)		; increment row
	POP BC	
	DEC B	
	PUSH BC	
	LD L,(IY+$0A)		; get record size
	LD H,$00	
	EX DE,HL	
	ADD HL,DE	
	ADD IX,DE	
	EX DE,HL	
	JR LB4D7	
LB4F0	LD L,(IY+$02)		; get colummn
	LD H,(IY+$03)		; get row
	CALL GetScrAddr		; Convert char coords HL to ZX screen address
	LD B,$08		; repeat 8 times
LB4FB	PUSH BC	
	PUSH HL	
	LD B,(IY+$0E)		; width as loop counter
LB500	LD A,(DE)	
	XOR (HL)		; XOR with pixels on the screen
	LD (IY+$07),A	
	CPL	
	AND (IX+$00)	
	LD (DE),A	
	LD A,(IX+$00)	
	OR (IY+$07)	
	LD (HL),A		; set pixels on the screen
	INC L			; next column
	INC DE	
	INC IX	
	DJNZ LB500	
	LD A,(IY+$06)	
	INC A	
	LD (IY+$07),A	
	LD L,(IY+$0F)	
	LD H,$00	
	EX DE,HL	
	ADD HL,DE	
	ADD IX,DE	
	EX DE,HL	
	POP HL	
	INC H			; next pixel row
	POP BC	
	DJNZ LB4FB	
	POP BC	
	INC (IY+$03)		; increment row
	LD A,(IY+$03)		; get row
	CP $18			; < 24 ?
	JR NC,LB53A	
	DJNZ LB4D6	
LB53A	POP IX	
	BIT 5,(IX+$0D)	
	JR NZ,LB547	
	BIT 6,(IX+$0D)	
	RET Z	
LB547	LD H,(IX+$08)	
	LD L,(IX+$07)	
	LD A,(IX+$02)	
	OR A	
	JR NZ,LB5A4	
	BIT 7,(IX+$11)	
	JR NZ,LB57E	
	PUSH HL	
	POP DE	
	INC HL	
	LD B,(IY+$09)		; number of records to copy
	CALL CopyRecordsFwd	; Copy records forward, from HL to DE
	DEC DE	
	XOR A	
	LD (DE),A	
;
LB565	LD A,(IX+$03)	
	OR A	
	RET NZ	
	LD H,(IX+$08)	
	LD L,(IX+$07)	
	JR LB5A4

; Copy records forward
; I: HL = source address, DE = destination address
; I: IY = ??? (IY+$0A) is record size
; I: B = number of records to copy
CopyRecordsFwd
	PUSH BC	
	LD C,(IY+$0A)		; get record size
	LD B,$00	
	LDIR	
	POP BC	
	DJNZ CopyRecordsFwd	
	RET	

LB57E	LD E,(IY+$0A)		; get record size
	LD D,$00	
	LD B,(IY+$09)	
LB586	ADD HL,DE	
	DJNZ LB586	
	DEC HL	
	LD D,H	
	LD E,L	
	DEC HL	
	LD B,(IY+$09)	
	CALL CopyRecordsBck	; Copy records backward, from HL to DE
	INC DE	
	XOR A	
	LD (DE),A	
	JR LB565

; Copy records backward
; I: HL = source address, DE = destination address
; I: IY = ??? (IY+$0A) is record size
; I: B = number of records to copy
CopyRecordsBck
	PUSH BC	
	LD C,(IY+$0A)		; get record size
	LD B,$00	
	LDDR	
	POP BC	
	DJNZ CopyRecordsBck	
	RET

; I: HL = ???
LB5A4	BIT 7,(IX+$12)	
	JR NZ,LB5C1	
	PUSH HL	
	LD E,(IY+$0A)		; get record size
	LD D,$00	
	ADD HL,DE	
	POP DE	
	LD B,(IY+$08)	
	CALL CopyRecordsFwd	; Copy records forward, from HL to DE
	LD B,(IY+$0A)		; get record size
	XOR A	
LB5BC	LD (DE),A	
	INC DE	
	DJNZ LB5BC	
	RET	
LB5C1	LD E,(IY+$0A)		; get record size
	LD D,$00	
	LD B,(IY+$08)	
LB5C9	ADD HL,DE	
	DJNZ LB5C9	
	DEC HL	
	PUSH HL	
	ADD HL,DE	
	POP DE	
	EX DE,HL	
	LD B,(IY+$08)	
	CALL CopyRecordsBck	; Copy records backward, from HL to DE
	LD B,(IY+$0A)		; get record size
	XOR A	
LB5DB	LD (DE),A		; clear the record
	DEC DE	
	DJNZ LB5DB	
	RET	

; I: IX = object record address; $C4F0 $C505 $C51A $C52F $C544 $C559 $C56E $C583 $C598 $C5AD  $C78F  $C8C7
; I: IY = ???; $C092 $C0A2 $C0B2 $C0EA $C102 $C11A $C132 $C14A $C182
LB5E0	RES 1,(IX+$0D)	
	BIT 0,(IX+$0D)	
	JR Z,LB5EE	
	SET 1,(IX+$0D)	
LB5EE	RES 0,(IX+$0D)	
	LD HL,(L5B0B)		; get screen position on 256x256 map
	LD A,(IX+$00)		; get column 0..255
	SUB L	
	LD C,A	
	CP $18			; < 24 ?
	JR C,LB60A	
	ADD A,(IY+$06)	
	CP $18			; < 24 ?
	LD A,C	
	JR C,LB60A	
	SET 0,(IX+$0D)	
LB60A	LD (IY+$02),A	
	LD (IY+$00),A	
	LD A,(IX+$01)		; get row 0..255
	SUB H	
	CP $18			; < 24 ?
	LD C,A	
	JR C,LB62D	
	ADD A,(IY+$08)	
	CP $18	
	LD A,C	
	JR C,LB62D	
	LD (IY+$01),A	
	RES 0,(IX+$0D)	
	LD A,(IX+$0D)	
	JR LB65F	
LB62D	LD (IY+$03),A	
	LD (IY+$01),A	
	LD A,(IX+$0D)	
	XOR $01			; inverse bit 0
	LD (IX+$0D),A	
	BIT 0,A	
	JR Z,LB65F	
	BIT 1,A	
	RET NZ	
	LD HL,LB676	
	XOR A	
LB646	BIT 7,(HL)	
	JR Z,LB655	
	INC HL	
	INC A	
	BIT 4,A	
	JR Z,LB646	
	SET 7,(IX+$0D)	
	RET	
LB655	AND $0F			; 0..15
	OR $F0	
	LD (IX+$08),A	
	SET 7,(HL)	
	RET	
LB65F	BIT 1,A	
	RET Z	
	BIT 7,(IX+$0D)	
	RET NZ	
	LD A,(IX+$08)	
	AND $0F			; 0..15
	LD HL,LB676	
	LD E,A	
	LD D,$00	
	ADD HL,DE	
	RES 7,(HL)	
	RET

LB676	DEFS $10

; I: IX = object record address; $C4F0 $C62B $C640 $C655 $C66A $C775 $C7A9 $C7C3 $C7DD $C7F7 $C811 $C82B $C845
; I: IY = ???; $C0A2 $C0B2 $C0EA $C102 $C11A $C132 $C14A $C182
LB686	BIT 5,(IX+$0D)	
	JR Z,LB6D6	
	LD L,(IY+$00)		; get column
	LD H,(IY+$01)		; get row
	LD A,$06	
	XOR (IX+$04)	
	LD C,A	
	BIT 7,(IX+$11)	
	JR NZ,LB6A6	
	INC (IX+$00)		; one column right
	INC (IY+$00)	
	JR LB6B1	
LB6A6	DEC (IX+$00)		; one column left
	DEC (IY+$00)	
	LD A,L	
	ADD A,(IY+$06)		; add DX ??
	LD L,A	
LB6B1	LD B,(IY+$09)		; get object height - loop counter
LB6B4	LD A,H	
	AND $E0			; check for 0..31 range
	JR NZ,LB6D3		; out of range => skip
	LD A,H	
	CP $18			; row < 24 ?
	JR NC,LB6D3		; no => skip
	LD A,L	
	AND $E0			; check for 0..31 range
	JR NZ,LB6D3		; out of range => skip
	LD A,L	
	CP $18			; column < 24 ?
	JR NC,LB6D3		; no => skip
	PUSH HL	
	CALL GetScrAttrAddr	; Get screen attribute address
	LD A,C	
	CP (HL)			; same color?
	JR NZ,LB6D2		; no => skip
	LD (HL),$06	
LB6D2	POP HL	
LB6D3	INC H			; next row
	DJNZ LB6B4	
LB6D6	BIT 6,(IX+$0D)	
	JR Z,LB704	
	LD L,(IY+$00)		; get column
	LD H,(IY+$01)		; get row
	LD A,$06	
	XOR (IX+$04)	
	LD C,A	
	BIT 7,(IX+$12)	
	JR NZ,LB6F6	
	INC (IY+$01)		; one row down
	INC (IX+$01)	
	JR LB701	
LB6F6	DEC (IY+$01)		; one row up
	DEC (IX+$01)	
	LD A,H	
	ADD A,(IY+$08)	
	LD H,A	
LB701	CALL LB70F		; Screen attribute change for horizontally oriented object
LB704	LD L,(IY+$00)		; get column
	LD H,(IY+$01)		; get row
	LD C,$06	
	JP LB875	

; Screen attribute change for horizontally oriented object
; I: IX = object record address; $C4F0 $C598 $C655 $C7A9 $C7F7 $C811 $C82B $C845 $C85F $C893 $C8AD
; I: IY = ???; $C092 $C0A2 $C0B2 $C0EA $C102 $C11A $C132
; I: HL = (row, column)
LB70F	LD A,H			; row
	AND $E0			; check for 0..31 range
	RET NZ			; return if out of range
	LD A,H			; row
	CP $18			; < 24 ?
	RET NC			; return if >= 24
	PUSH HL	
	LD B,(IY+$07)		; object width as loop counter
LB71B	LD A,L			; column
	AND $E0			; check for 0..31 range
	JR NZ,LB732		; return if out of range
	LD A,L			; column
	CP $18			; < 24 ?
	JR NC,LB732		; return if >= 24
	PUSH HL	
	CALL GetScrAttrAddr	; Get screen attribute address
	LD A,(HL)		; get color from the screen
	CP C			; same color?
	JR NZ,LB731		; skip if not
	XOR (IX+$04)		; change color
	LD (HL),A		; write color to the screen
LB731	POP HL	
LB732	INC L			; next column
	DJNZ LB71B	
	POP HL	
	RET

; I: IX = record address in block LC4F0; $C4F0 $C52F $C601 $C62B $C640 $C712 $C727 $C85F
LB737	LD D,(IX+$14)	
	LD E,(IX+$13)		; now DE = object record address in block LC092
	PUSH DE	
	POP IY	
	LD A,(IX+$0E)	
	LD (IX+$0F),A	
	RES 5,(IX+$0D)	
	RES 6,(IX+$0D)	
	LD A,(IX+$02)	
	ADD A,(IX+$11)	
	AND $07			; 0..7
	LD (IX+$02),A	
	LD A,(IX+$03)	
	ADD A,(IX+$12)	
	AND $07			; 0..7
	LD (IX+$03),A	
	CALL LB5E0	
	BIT 0,(IX+$0D)	
	JR Z,$B77B	
	BIT 7,(IX+$0D)	
	JR NZ,LB79E	
	CALL LB470	
	CALL LB686	
	JR LB79E	
LB77B	LD A,(IX+$02)	
	RES 7,(IX+$0D)	
	OR A	
	JR NZ,LB789	
	SET 5,(IX+$0D)	
LB789	LD A,(IX+$03)	
	OR A	
	JR NZ,LB793	
	SET 6,(IX+$0D)	
LB793	LD A,(IX+$0D)	
	AND $60	
	JR Z,LB79E	
	CALL LB686	
	RET	
LB79E	LD A,(IX+$02)	
	BIT 2,(IX+$0D)	
	JR Z,LB7AA	
	LD A,(IX+$03)	
LB7AA	CP $04	
	RET NZ	
	LD HL,LB7B8		; address of the return point - put on the stack
	PUSH HL
	LD H,(IX+$06)	
	LD L,(IX+$05)	
	JP (HL)	
; Point of return
LB7B8	RET

LB7B9	DEFS $02

; I: IX = object address in block LC4F0; $C505 $C893 $C8E1
LB7BB	LD A,(IX+$0D)	
	AND $9E	
	LD (IX+$0D),A	
	LD E,(IX+$13)	
	LD D,(IX+$14)		; get record address in block LC092
	PUSH DE	
	POP IY	
	LD A,(IX+$02)	
	ADD A,(IX+$11)	
	AND $07			; 0..7
	LD (IX+$02),A	
	LD A,(IX+$03)	
	ADD A,(IX+$12)	
	AND $07			; 0..7
	LD (IX+$03),A	
	CALL LB5E0	
	BIT 0,(IX+$0D)	
	JR Z,LB7F9	
	BIT 7,(IX+$0D)	
	JR NZ,LB81C	
	CALL LB470	
	CALL LB837	
	JR LB81C	
LB7F9	LD A,(IX+$02)	
	RES 7,(IX+$0D)	
	OR A	
	JR NZ,LB807	
	SET 5,(IX+$0D)	
LB807	LD A,(IX+$03)	
	OR A	
	JR NZ,LB811	
	SET 6,(IX+$0D)	
LB811	LD A,(IX+$0D)	
	AND $60	
	JR Z,LB81C	
	CALL LB686	
	RET	
LB81C	LD A,(IX+$02)	
	BIT 2,(IX+$0D)	
	JR Z,LB828	
	LD A,(IX+$03)	
LB828	CP $04	
	RET NZ	
	LD HL,LB836	
	PUSH HL	
	LD H,(IX+$06)	
	LD L,(IX+$05)	
	JP (HL)	
; Point of return
LB836	RET	

LB837	LD A,$06	
	BIT 0,(IX+$0D)	
	JR NZ,LB842	
	XOR (IX+$04)	
LB842	LD C,A	
	LD L,(IY+$00)	
	LD H,(IY+$01)	
	LD A,(IX+$02)	
	OR A	
	JR NZ,LB85F	
	BIT 7,(IX+$11)	
	JR NZ,LB85B	
	INC (IX+$00)	
	INC L	
	JR LB85F	
LB85B	DEC (IX+$00)	
	DEC L	
LB85F	LD A,(IX+$03)	
	OR A	
	JR NZ,LB875	
	BIT 7,(IX+$12)	
	JR NZ,LB871	
	INC (IX+$01)	
	INC H	
	JR LB875	
LB871	DEC (IX+$01)	
	DEC H	
;
LB875	LD A,(IY+$09)	
	LD (IY+$0E),A	
LB87B	CALL LB70F		; Screen attribute change for horizontally oriented object
	INC H			; next row
	DEC (IY+$0E)	
	JR NZ,LB87B	
	RET

LB885	BIT 0,(IX+$10)		; check "moving" bit
	JP NZ,LB941	
	BIT 7,(IX+$11)	
	LD A,(IX+$00)		; get column 0..255
	JR NZ,LB89C	
	INC A	
	INC A	
	ADD A,(IY+$07)	
	JR LB89F	
LB89C	DEC A	
	DEC A	
	DEC A	
LB89F	LD L,A	
	LD H,(IX+$01)		; get row 0..255
	CALL GetTileInBlock	; Calculate address in relief block and Get tile nummber
	OR A			; empty tile?
	JR NZ,LB903	
	INC H			; down one row
	CALL GetTileInBlock	; Calculate address in relief block and Get tile nummber
	OR A			; empty tile?
	JR NZ,LB903	
	BIT 0,(IX+$0D)	
	JR Z,LB8D7	
	BIT 7,(IX+$0D)	
	JR NZ,LB8D7	
	LD DE,(L5B0B)		; get screen position on 256x256 map
	LD A,H			; get screen row
	SUB D			; now A = [row 0..255] - [screen row]
	CP $18			; result within 0..23 ?
	JR NC,LB8D7	
	LD D,A			; now D = row 0..23
	LD A,L			; get screen column
	SUB E			; now A = [column 0..255] - [screen column]
	CP $18			; result within 0..23 ?
	JR NC,LB8D7	
	LD L,A	
	LD H,D			; now HL = row/column on the screen
	CALL GetScrAttrAddr	; Get screen attribute address
	LD A,(HL)		; get attribute value
	CP $06	
	JR NZ,LB903	
LB8D7	LD A,R	
	JR Z,LB903	
	LD C,A	
	AND $40	
	JR Z,LB8E5	
LB8E0	LD (IX+$12),$00	
	RET	
LB8E5	BIT 2,C	
	JR Z,LB8F6	
	LD A,(IX+$01)	
	CP (IX+$15)	
	JR NC,LB8E0	
	LD (IX+$12),$01	
	RET	
LB8F6	LD A,(IX+$16)	
	CP (IX+$01)	
	JR NC,LB8E0	
	LD (IX+$12),$FF	
	RET	
LB903	BIT 7,(IX+$11)	
	JR Z,LB917	
	LD A,(IX+$17)	
	LD (IX+$18),A	
	LD L,(IY+$16)	
	LD H,(IY+$17)	
	JR LB924	
LB917	XOR A	
	SUB (IX+$17)	
	LD (IX+$18),A	
	LD L,(IY+$14)	
	LD H,(IY+$15)	
LB924	LD (IX+$0B),L	
	LD (IX+$0C),H	
	LD (IX+$11),$00	
	LD (IX+$12),$00	
	SET 0,(IX+$10)		; set "moving" bit
	LD A,(IY+$0C)	
	LD (IX+$19),A	
	SLA (IX+$0E)	
	RET	
LB941	DEC (IX+$19)	
	RET NZ	
	LD A,(IX+$18)	
	LD (IX+$11),A	
	BIT 7,A	
	JR Z,LB957	
	LD L,(IY+$10)	
	LD H,(IY+$11)	
	JR LB95D	
LB957	LD L,(IY+$12)	
	LD H,(IY+$13)	
LB95D	LD (IX+$09),L		; set sprite address
	LD (IX+$0A),H
	LD (IX+$0B),L		; set sprite address
	LD (IX+$0C),H
	RES 0,(IX+$10)		; clear "moving" bit
	SRL (IX+$0E)	
	RET	

LB972	BIT 0,(IX+$10)		; check "moving" bit
	JP NZ,LBA33	
	BIT 7,(IX+$12)	
	LD A,(IX+$01)		; get row 0..255
	JR NZ,LB988	
	ADD A,(IY+$09)	
	INC A	
	JR LB98A	
LB988	DEC A	
	DEC A	
LB98A	LD H,A	
	LD L,(IX+$00)		; get column 0..255
	CALL GetTileInBlock	; Calculate address in relief block and Get tile nummber
	OR A			; empty tile?
	JR NZ,LB9F9	
	LD A,L	
	ADD A,(IY+$06)	
	LD L,A	
	CALL GetTileInBlock	; Calculate address in relief block and Get tile nummber
	OR A			; empty tile?
	JR NZ,LB9F9	
	BIT 0,(IX+$0D)	
	JR Z,LB9C7	
	BIT 7,(IX+$0D)	
	JR NZ,LB9C7	
	LD DE,($5B0B)	
	LD A,H	
	SUB D	
	CP $18	
	JR NC,LB9C7	
	LD D,A	
	LD A,L	
	DEC A	
	SUB E	
	CP $18	
	JR NC,LB9C7	
	LD L,A	
	LD H,D	
	CALL GetScrAttrAddr	; Get screen attribute address
	LD A,(HL)		; get attribute value
	CP $06	
	JR NZ,LB9F9	
LB9C7	LD A,R	
	JR Z,LB9F9	
	BIT 6,A	
	JR Z,LB9D4	
LB9CF	LD (IX+$11),$00	
	RET	
LB9D4	BIT 2,A	
	LD C,$01	
	JR Z,LB9E3	
	LD C,$FF	
	LD A,L	
	SUB (IY+$06)	
	SUB $02	
	LD L,A	
LB9E3	INC L	
	CALL GetTileInBlock	; Calculate address in relief block and Get tile nummber
	OR A			; empty tile?
	JR NZ,LB9CF	
	LD A,H	
	ADD A,(IY+$08)	
	LD H,A	
	CALL GetTileInBlock	; Calculate address in relief block and Get tile nummber
	OR A			; empty tile?
	JR NZ,LB9CF	
	LD (IX+$11),C	
	RET	
LB9F9	BIT 7,(IX+$12)	
	JR Z,LBA0A	
	LD A,(IX+$17)	
	LD L,(IY+$14)	
	LD H,(IY+$15)	
	JR LBA14	
LBA0A	XOR A	
	SUB (IX+$17)	
	LD L,(IY+$16)	
	LD H,(IY+$17)	
LBA14	LD (IX+$18),A	
	LD (IX+$0B),L	
	LD (IX+$0C),H	
	XOR A	
	LD (IX+$11),A	
	LD (IX+$12),A	
	SET 0,(IX+$10)		; set "moving" bit
	LD A,(IY+$0C)	
	LD (IX+$19),A	
	SLA (IX+$0E)	
	RET	
LBA33	DEC (IX+$19)	
	RET NZ	
	LD A,(IX+$18)	
	LD (IX+$12),A	
	BIT 7,A	
	JR Z,LBA49	
	LD L,(IY+$12)	
	LD H,(IY+$13)	
	JR LBA4F	
LBA49	LD L,(IY+$10)	
	LD H,(IY+$11)	
LBA4F	LD (IX+$09),L	
	LD (IX+$0A),H	
	LD (IX+$0B),L	
	LD (IX+$0C),H	
	RES 0,(IX+$10)		; clear "moving" bit
	SRL (IX+$0E)	
	RET	

LBA64	BIT 0,(IX+$10)		; check "moving" bit
	JP NZ,LBC8F	
	SET 5,(IX+$10)	
	BIT 2,(IX+$0D)	
	LD L,(IX+$00)	
	LD H,(IX+$01)	
	JP NZ,LBB94	
	BIT 7,(IX+$11)	
	JR NZ,LBA8F	
	LD A,L	
	ADD A,(IY+$07)	
	LD L,A	
	LD D,(IY+$15)	
	LD E,(IY+$14)	
	JR LBA96	
LBA8F	DEC L	
	LD D,(IY+$17)		; Get ??
	LD E,(IY+$16)	
LBA96	CALL GetTileInBlock	; Calculate address in relief block and Get tile nummber
	OR A			; empty tile?
	JR NZ,LBADE	
	LD A,H	
	ADD A,(IY+$08)	
	LD H,A	
	CALL GetTileInBlock	; Calculate address in relief block and Get tile nummber
	OR A			; empty tile?
	JR NZ,LBADE	
	LD A,R	
	JR Z,LBADE	
	BIT 6,A	
	JR NZ,LBAB4	
LBAAF	LD (IX+$12),$00	
	RET	
LBAB4	LD H,(IX+$01)		; get row 0..255
	LD L,(IX+$00)		; get column 0..255
	DEC H			; one row up
	LD C,$FF	
	BIT 2,A	
	JR Z,LBAC9	
	LD C,$01	
	LD A,H	
	ADD A,(IY+$09)	
	LD H,A	
	INC H			; one row down
LBAC9	CALL GetTileInBlock	; Calculate address in relief block and Get tile nummber
	OR A			; empty tile?
	JR NZ,LBAAF	
	LD A,L	
	ADD A,(IY+$06)	
	LD L,A	
	CALL GetTileInBlock	; Calculate address in relief block and Get tile nummber
	OR A			; empty tile?
	JR NZ,LBAAF	
	LD (IX+$12),C	
	RET	
LBADE	LD (IX+$0B),E	
	LD (IX+$0C),D	
	SET 0,(IX+$10)		; set "moving" bit
	RES 7,(IX+$10)	
	SLA (IX+$0E)	
	LD L,(IX+$00)		; get column 0..255
	LD H,(IX+$01)		; get row 0..255
	BIT 7,(IX+$11)	
	JR NZ,LBB04	
	LD A,L	
	ADD A,(IY+$06)	
	SUB (IY+$08)	
	LD L,A	
LBB04	LD A,R	
	JR Z,LBB32	
	LD A,H	
	CP $4A	
	JR C,LBB32	
	ADD A,(IY+$08)	
	SUB (IY+$07)	
	LD H,A	
	CALL GetTileInBlock	; Calculate address in relief block and Get tile nummber
	OR A			; empty tile?
	JR NZ,LBB32	
	LD A,L	
	LD C,L	
	ADD A,(IY+$08)	
	LD L,A	
	CALL GetTileInBlock	; Calculate address in relief block and Get tile nummber
	OR A			; empty tile?
	LD L,C	
	JR NZ,LBB32	
	SET 6,(IX+$10)	
	XOR A	
	SUB (IX+$17)	
	INC H	
	JR LBB5E	
LBB32	LD A,R	
	JR Z,LBB75	
	LD A,(IX+$01)		; get row 0..255
	CP $FA	
	JR NC,LBB75	
	ADD A,(IY+$07)	
	LD H,A	
	CALL GetTileInBlock	; Calculate address in relief block and Get tile nummber
	OR A			; empty tile?
	JR NZ,LBB75	
	LD C,L	
	LD A,L	
	ADD A,(IY+$08)	
	LD L,A	
	CALL GetTileInBlock	; Calculate address in relief block and Get tile nummber
	OR A			; empty tile?
	LD L,C	
	JR NZ,LBB75	
	SET 6,(IX+$10)	
	LD A,(IX+$17)	
	LD H,(IX+$01)		; get row 0..255
LBB5E	LD (IX+$18),A	
	XOR A	
	LD (IX+$12),A	
	LD (IX+$11),A	
	LD (IX+$1A),L	
	LD (IX+$1B),H	
	LD A,(IX+$15)	
	LD (IX+$19),A	
	RET	
LBB75	RES 6,(IX+$10)	
	RES 5,(IX+$10)	
	LD H,(IX+$01)		; get row 0..255
	LD L,(IX+$00)		; get column 0..255
	BIT 7,(IX+$11)	
	JR NZ,LBB8F	
	XOR A	
	SUB (IX+$17)	
	JR LBB5E	
LBB8F	LD A,(IX+$17)	
	JR LBB5E	
LBB94	BIT 7,(IX+$12)	
	JR NZ,LBBA7	
	LD A,H	
	ADD A,(IY+$09)	
	LD H,A	
	LD D,(IY+$17)	
	LD E,(IY+$16)	
	JR LBBAE	
LBBA7	DEC H			; one row up
	LD D,(IY+$15)	
	LD E,(IY+$14)	
LBBAE	CALL GetTileInBlock	; Calculate address in relief block and Get tile nummber
	OR A			; empty tile?
	JR NZ,LBBF6	
	LD A,L	
	ADD A,(IY+$06)	
	LD L,A	
	CALL GetTileInBlock	; Calculate address in relief block and Get tile nummber
	OR A			; empty tile?
	JR NZ,LBBF6	
	LD A,R	
	JR Z,LBBF6	
	BIT 6,A	
	JR NZ,LBBCC	
LBBC7	LD (IX+$11),$00	
	RET	
LBBCC	LD H,(IX+$01)		; get row 0..255
	LD L,(IX+$00)		; get column 0..255
	DEC L			; one column left
	LD C,$FF	
	BIT 2,A	
	JR Z,LBBE1	
	LD C,$01	
	LD A,L	
	ADD A,(IY+$07)	
	INC A	
	LD L,A	
LBBE1	CALL GetTileInBlock	; Calculate address in relief block and Get tile nummber
	OR A			; empty tile?
	JR NZ,LBBC7	
	LD A,H	
	ADD A,(IY+$08)	
	LD H,A	
	CALL GetTileInBlock	; Calculate address in relief block and Get tile nummber
	OR A			; empty tile?
	JR NZ,LBBC7	
	LD (IX+$11),C	
	RET	
LBBF6	LD (IX+$0B),E	
	LD (IX+$0C),D	
	SET 0,(IX+$10)		; set "moving" bit
	SET 7,(IX+$10)	
	SLA (IX+$0E)	
	LD L,(IX+$00)		; get row 0..255
	LD H,(IX+$01)		; get column 0..255
	BIT 7,(IX+$12)	
	JR NZ,$BC1C	
	LD A,H	
	ADD A,(IY+$08)	
	SUB (IY+$06)	
	LD H,A	
LBC1C	LD A,R	
	JR Z,LBC47	
	LD A,L	
	ADD A,(IY+$06)	
	SUB (IY+$09)	
	LD L,A	
	CALL GetTileInBlock	; Calculate address in relief block and Get tile nummber
	OR A			; empty tile?
	JR NZ,LBC47	
	LD A,H	
	LD C,H	
	ADD A,(IY+$06)	
	LD H,A	
	CALL GetTileInBlock	; Calculate address in relief block and Get tile nummber
	OR A			; empty tile?
	LD H,C	
	JR NZ,LBC47	
	RES 6,(IX+$10)	
	XOR A	
	SUB (IX+$17)	
	INC L	
	JP LBB5E	
LBC47	LD A,(IX+$00)	
	ADD A,(IY+$09)	
	LD L,A	
	CALL GetTileInBlock	; Calculate address in relief block and Get tile nummber
	OR A			; empty tile?
	JR NZ,LBC6E	
	LD C,H	
	LD A,H	
	ADD A,(IY+$06)	
	LD H,A	
	CALL GetTileInBlock	; Calculate address in relief block and Get tile nummber
	OR A			; empty tile?
	LD H,C	
	JR NZ,LBC6E	
	RES 6,(IX+$10)	
	LD A,(IX+$17)	
	LD L,(IX+$00)	
	JP LBB5E	
LBC6E	SET 6,(IX+$10)	
	RES 5,(IX+$10)	
	LD H,(IX+$01)		; get row 0..255
	LD L,(IX+$00)		; get column 0..255
	BIT 7,(IX+$12)	
	JR NZ,LBC89	
	XOR A	
	SUB (IX+$17)	
	JP LBB5E	
LBC89	LD A,(IX+$17)	
	JP LBB5E	
LBC8F	BIT 1,(IX+$10)	
	JP NZ,LBD4E	
	DEC (IX+$19)	
	RET NZ	
	CALL LB5E0	
	BIT 0,(IX+$0D)	
	JR Z,LBCC1	
	BIT 7,(IX+$0D)	
	JR NZ,LBCC1	
	RES 0,(IX+$0D)	
	CALL LB470	
	CALL LB837	
	LD A,(IX+$08)	
	AND $0F			; 0..15
	LD E,A	
	LD D,$00	
	LD HL,LB676	
	ADD HL,DE	
	LD (HL),$00	
LBCC1	BIT 5,(IX+$10)	
	JR Z,LBCEA	
	LD A,(IX+$02)	
	LD B,(IX+$03)	
	LD (IX+$03),A	
	LD (IX+$02),B	
	LD A,(IX+$0D)	
	XOR $04	
	LD (IX+$0D),A	
	LD L,(IY+$04)	
	LD H,(IY+$05)	
	PUSH HL	
	POP IY	
	LD (IX+$13),L		; set X value
	LD (IX+$14),H		; set Y value
LBCEA	LD A,(IX+$1A)	
	LD (IX+$00),A		; set column
	LD A,(IX+$1B)	
	LD (IX+$01),A		; set row
	BIT 6,(IX+$10)	
	JR Z,LBD04	
	BIT 7,(IX+$18)	
	JR Z,LBD0A	
	JR LBD18	
LBD04	BIT 7,(IX+$18)	
	JR Z,LBD18	
LBD0A	LD H,(IY+$19)	
	LD L,(IY+$18)	
	LD D,(IY+$11)	
	LD E,(IY+$10)	
	JR LBD24	
LBD18	LD H,(IY+$1B)	
	LD L,(IY+$1A)	
	LD D,(IY+$13)	
	LD E,(IY+$12)	
LBD24	LD (IX+$0A),D	
	LD (IX+$09),E	
	LD (IX+$0C),H	
	LD (IX+$0B),L	
	LD A,(IX+$16)	
	LD (IX+$19),A	
	SET 1,(IX+$10)	
	CALL LB5E0	
	BIT 0,(IX+$0D)	
	RET Z	
	BIT 7,(IX+$0D)	
	RET NZ	
	CALL LB470	
	CALL LB837	
	RET	
LBD4E	DEC (IX+$19)	
	RET NZ	
	SRL (IX+$0E)	
	LD A,(IX+$0A)	
	LD (IX+$0C),A	
	LD A,(IX+$09)	
	LD (IX+$0B),A	
	RES 0,(IX+$10)		; clear "moving" bit
	RES 1,(IX+$10)	
	LD A,(IX+$18)	
	BIT 6,(IX+$10)	
	JR NZ,LBD7B	
	LD (IX+$11),A	
	RES 2,(IX+$0D)	
	RET	
LBD7B	LD (IX+$12),A	
	SET 2,(IX+$0D)	
	RET	

LBD83	LD A,L	
	AND C	
	ADD A,C	
	LD (IX+$0E),A	
	LD HL,LBDA9	
	INC (HL)	
	LD A,$1F	
	AND (HL)	
	INC A	
	LD (IX+$0F),A	
	PUSH DE	
	CALL NextRandom		; Calculate next random value
	LD A,H	
	AND $07			; 0..7
LBD9B	LD HL,LBDAA		; !!! mutable argument
	ADD A,L	
	LD L,A	
	JR NC,LBDA3	
	INC H	
LBDA3	LD A,(HL)	
	LD (IX+$04),A		; set DX value
	POP DE	
	RET	

LBDA9	DEFB $00
LBDAA	DEFB $41,$44,$05,$45,$02,$42,$43,$40
LBDB2	DEFB $05,$45,$02,$42,$43,$03,$40,$41

LBDBA	CALL LBEC7	
	LD A,(L5B03+1)		; get screen position (row) on mini-map
	CP $03			; < 3 ?
	JR NC,LBDCB	
	CALL LBEB2	
	CALL LBE58	
	RET	
LBDCB	CP $04	
	JR NC,LBDD6	
	CALL LBE58	
	CALL LBE85	
	RET	
LBDD6	CP $06	
	JR NC,LBDDE	
	CALL LBE85	
	RET	
LBDDE	CP $09	
	JR NC,LBDE5	
	CALL LBE85	
LBDE5	CALL LC45C	
	RET	

LBDE9	DEFS $01

LBDEA	LD A,(L5B03+1)		; get screen position (row) on mini-map
	CP $03	
	JR NC,LBDFE	
	CALL LBE9A	
	CALL LBE40	
	LD A,(L5B1A)	
	CALL LC4D5	
	RET	
LBDFE	CP $04	
	JR NC,LBE0F	
	CALL LBE40	
	CALL LBE6D	
	LD A,(L5B1B)	
	CALL LC4D5	
	RET	
LBE0F	CP $06	
	JR NC,LBE1D	
	CALL LBE6D	
	LD A,(L5B1C)	
	CALL LC4D5	
	RET	
LBE1D	CP $09	
	JR NC,LBE36	
	CALL LBE6D	
	LD HL,LBDE9	
	LD A,(L5B1D)	
	CALL LC4D5	
	LD A,$01	
	XOR (HL)	
	LD (HL),A	
	RET Z	
	CALL LC431	
	RET	
LBE36	CALL LC431	
	LD A,(L5B1E)	
	CALL LC4D5	
	RET

LBE40	LD A,(L5B16)		; get value 18 / 22 / 26 / 31, depending on LEVEL	
	LD B,A	
	LD IX,(L5B1F)		; now IX = record address in block LC4F0
LBE48	PUSH BC	
	DEC (IX+$0F)	
	CALL Z,LB737	
	LD BC,$001A		; 26
	ADD IX,BC	
	POP BC	
	DJNZ LBE48	
	RET

LBE58	LD A,(L5B16)		; get value 18 / 22 / 26 / 31, depending on LEVEL
	LD B,A	
	LD IX,(L5B1F)		; now IX = record address in block LC4F0
LBE60	PUSH BC	
	CALL LB7BB	
	LD BC,$001A		; 26
	ADD IX,BC	
	POP BC	
	DJNZ LBE60	
	RET

LBE6D	LD A,(L5B17)		; get value 26 / 34 / 42 / 50, depending on LEVEL	
	LD B,A	
	LD IX,(L5B21)		; now IX = record address in block LC4F0
LBE75	PUSH BC	
	DEC (IX+$0F)	
	CALL Z,LB737	
	LD BC,$001A		; 26
	ADD IX,BC	
	POP BC	
	DJNZ LBE75	
	RET	

LBE85	LD A,(L5B17)		; get value 26 / 34 / 42 / 50, depending on LEVEL
	LD B,A	
	LD IX,(L5B21)	
LBE8D	PUSH BC	
	CALL LB7BB	
	LD BC,$001A		; 26
	ADD IX,BC	
	POP BC	
	DJNZ LBE8D	
	RET

LBE9A	LD A,(L5B15)		; get number of 21-byte records: 27 / 35 / 43 / 51, depending on LEVEL	
	LD B,A			; loop count
	LD IX,LC4F0		; record address
LBEA2	PUSH BC	
	DEC (IX+$0F)	
	CALL Z,LB737	
	LD BC,$0015		; 21 - record size
	ADD IX,BC		; next record
	POP BC	
	DJNZ LBEA2	
	RET

LBEB2	LD A,(L5B15)		; get number of 21-byte records: 27 / 35 / 43 / 51, depending on LEVEL
	LD B,A			; loop count
	LD IX,LC4F0		; record address
LBEBA	PUSH BC	
	CALL LB7BB	
	LD BC,$0015		; 21 - record size
	ADD IX,BC		; next record
	POP BC	
	DJNZ LBEBA	
	RET

LBEC7	LD HL,LB676	
	LD (HL),$80	
	INC HL	
	LD B,$0F		; repeat 15 times
	XOR A	
LBED0	LD (HL),A	
	INC HL	
	DJNZ LBED0	
	LD HL,$F000		; ???
	LD (LB7B9),HL	
	RET

LBEDB	CALL LBEC7	
	LD HL,LBDAA	
	LD (LBD9B+1),HL	
	LD HL,LC20A		; record template for Boat
	LD DE,LC4F0		; record address 
	LD BC,$0015		; 21
	LDIR			; Copy 21 bytes from LC20A to LC4F0
	LD A,(L5B13)		; get Number of Meduza objects
	LD B,A			; loop counter
LBEF3	PUSH BC	
	LD HL,LC21F		; record template for Meduza
	PUSH DE	
	POP IX	
	LD BC,$0015		; 21 - record size
	LDIR			; Copy 21 bytes
	PUSH DE	
	CALL NextRandom		; Calculate next random value
	POP DE	
	LD (IX+$00),H		; set column 0..255
	LD C,$3F	
	CALL LBD83	
	POP BC	
	DJNZ LBEF3	
	LD A,(L5B14)		; get Number of Round fishes	
	LD B,A			; loop counter
	LD C,$00	
	LD A,(RANDOM)		; get current Random lo byte
	BIT 5,A	
	JR Z,LBF1D	
	INC C	
LBF1D	PUSH BC	
	LD HL,LC234		; record template for Round fish
	PUSH DE	
	POP IX	
	LD BC,$0015		; 21 - record size
	LDIR			; Copy template to record
	PUSH DE	
	CALL NextRandom		; Calculate next random value
	POP DE	
	LD (IX+$00),H	
	LD A,L	
	AND $03			; 0..3
	ADD A,(IX+$01)	
	LD (IX+$01),A	
	LD A,L	
	SRL A	
	SRL A	
	AND $07			; 0..7
	JR NZ,LBF44	
	INC A	
LBF44	LD (IX+$03),A	
	LD C,$1F	
	CALL LBD83	
	POP BC	
	BIT 0,C	
	JR Z,LBF5D	
	LD (IX+$09),$85	
	LD (IX+$0B),$85	
	LD (IX+$11),$FE	
LBF5D	DJNZ LBF1D	
	LD (L5B1F),DE		; save address for 26-byte records
	LD A,(L5B16)		; get number of records: 18 / 22 / 26 / 31, depending on LEVEL
	LD HL,$0F1B	
	LD IY,LC2EB		; table address with record templates
	CALL LBFB0	
	LD (L5B21),DE		; save address for 26-byte records
	LD A,(L5B17)		; get number of records: 26 / 34 / 42 / 50, depending on LEVEL	
	LD HL,$2A3B	
	LD IY,LC331		; table address with record templates
	CALL LBFB0	
	LD (L5B23),DE		; save address for 26-byte records
	LD IY,LBFA0	
	LD A,(L5B18)		; get number of records: 5 / 10 / 15 / 20, depending on LEVEL
	LD HL,LBDB2	
	LD (LBD9B+1),HL	
	CALL LC009	
	LD IY,LBFA8	
	LD A,(L5B19)		; get number of records: 12 / 27 / 42 / 57, depending on LEVEL
	CALL LC009	
LBF9F	RET

LBFA0	DEFB $05,$00,$AD,$C3,$1A,$00,$00,$0F
LBFA8	DEFB $10,$11,$29,$C4,$1C,$00,$00,$0F

; I: IX = ???
; I: IY = address of table with 8 record template addresses: LC2EB or LC331
; I: HL = ???
; I: A = number of records, loop counter
; O: DE = address ??
LBFB0	LD (LBFC3+1),IY	
	LD (LC007),HL	
	LD B,A	
LBFB8	PUSH BC	
	PUSH DE	
	CALL NextRandom		; Calculate next random value
	LD A,H	
	AND $0E			; 0 / 2 / 4 / 6 / 8 / 10 / 12 / 14
	LD E,A	
	LD D,$00	
LBFC3	LD HL,$0000		; table address !!! mutable argument
	ADD HL,DE	
	LD C,(HL)	
	INC HL	
	LD B,(HL)		; now BC = record template address
	LD L,C	
	LD H,B	
	LD BC,$001A		; 26 - record size
	POP DE	
	PUSH DE	
	POP IX	
	LDIR			; copy template to record
	LD HL,($C007)		; restore saved HL
	LD (IX+$16),H	
	LD (IX+$15),L	
	LD A,(RANDOM)		; get current Random lo byte
	AND $7F			; 0..127
	ADD A,$40		; 64..191
	LD (IX+$00),A		; set colummn
	LD A,(RANDOM+1)		; get current Random hi byte
	SRL A	
	SRL A	
	SRL A	
	AND $0F			; 0..15
	LD H,A	
	LD A,L	
	SUB H	
	LD (IX+$01),A		; set row
	PUSH DE	
	CALL NextRandom		; Calculate next random value
	POP DE	
	LD C,$0F	
	CALL LBD83	
	POP BC	
	DJNZ LBFB8	
	RET

LC007	DEFW $0000		; temp storage for HL

; I: IY ???
; I: A = number of records
LC009	LD B,A	
	LD (IY+$05),E	
	LD (IY+$06),D	
	LD DE,$F8F8	
	LD HL,MiniMap+$3FF	
LC016	LD A,(HL)	
	CP (IY+$00)	
	JR Z,LC039	
	CP (IY+$01)	
	JR Z,LC039	
LC021	DEC HL	
	LD A,E	
	SUB $08	
	LD E,A	
	CP $F8	
	JR NZ,LC037	
	LD A,D	
	SUB $08	
	LD D,A	
	CP $38	
	JR NZ,LC037	
	LD D,$F8	
	LD HL,MiniMap+$3FF	
LC037	JR LC016	
LC039	PUSH BC	
	PUSH HL	
	PUSH DE	
	CALL NextRandom		; Calculate next random value
	LD D,(IY+$03)	
	LD E,(IY+$02)	
	LD A,L	
	AND $F0	
	JR Z,LC04F	
	POP DE	
	POP HL	
	POP BC	
	JR LC021	
LC04F	LD A,H	
	AND $06			; 0 / 2 / 4 / 6
	LD L,A	
	LD H,$00	
	ADD HL,DE	
	LD E,(HL)	
	INC HL	
	LD D,(HL)	
	EX DE,HL	
	LD C,(IY+$04)	
	LD B,$00	
	LD E,(IY+$05)	
	LD D,(IY+$06)	
	PUSH DE	
	POP IX	
	LDIR	
	LD (IY+$05),E	
	LD (IY+$06),D	
	POP DE	
	LD A,D	
	ADD A,$03	
	LD (IX+$01),A	
	LD A,E	
	ADD A,$04	
	LD (IX+$00),A	
	LD A,(RANDOM+1)		; get current Random hi byte
	LD L,A	
	LD C,(IY+$07)	
	CALL LBD83	
	POP HL	
	POP BC	
	DJNZ LC021	
	LD E,(IY+$05)	
	LD D,(IY+$06)	
	RET	

; Object records
;	(IY+$00) - ??
;	(IY+$01) - ??
;	(IY+$02) - ??
;	(IY+$03) - ??
;	(IY+$06) - ??
;	(IY+$07) - record width
;	(IY+$08) - ??
LC092	DEFW $0000,$0000,$0000,$0807,$0403,$1840,$C080,$0000	
LC0A2	DEFW $0000,$0000,$0000,$0201,$0302,$1010,$2030,$0000	
LC0B2	DEFW $0000,$0000,$C0CE,$0302,$0201,$0818,$1803,$0000,$8285,$8254,$82B6,$82E6,$82C6,$82F6	
LC0CE	DEFW $0000,$0000,$C0B2,$0201,$0302,$1010,$2003,$0000,$8347,$8316,$8378,$83A8,$8388,$83B8	
LC0EA	DEFW $0000,$0000,$0000,$0706,$0302,$1038,$7005,$0000,$64E5,$6364,$6666,$6846	
LC102	DEFW $0000,$0000,$0000,$0807,$0403,$1840,$C006,$0000,$70B7,$6A26,$6CC7,$7358	
LC11A	DEFW $0000,$0000,$0000,$0504,$0201,$0828,$2804,$0000,$7BCE,$7C4F,$7D50,$7CD0	
LC132	DEFW $0000,$0000,$0000,$0403,$0403,$1820,$6001,$0000,$7748,$7869,$7942,$7821	
LC14A	DEFW $0000,$0000,$C166,$0403,$0201,$0820,$2005,$0000,$60D9,$6000,$6061,$613A,$6091,$616A	
LC166	DEFW $0000,$0000,$C14A,$0201,$0403,$1810,$3005,$0000,$628B,$61B2,$6213,$62EC,$6243,$631C	
LC182	DEFW $0000,$0000,$C19E,$0403,$0201,$0820,$2003,$0000,$7A1B,$798A,$79D3,$7A64,$79EB,$7A7C	
LC19E	DEFW $0000,$0000,$C182,$0201,$0403,$1810,$3003,$0000,$7AF5,$7AAC,$7B3E,$7B86,$7B56,$7B9E	
LC1BA	DEFW $0000,$0000,$C1D6,$0504,$0201,$0828,$2805,$0000,$7EF1,$7DD0,$7E51,$7F72,$7E91,$7FB2	
LC1D6	DEFW $0000,$0000,$C1BA,$0201,$0504,$2010,$4005,$0000,$8133,$8012,$8093,$81B4,$80D3,$81F4	
LC1F2	DEFW $0000,$0000,$0000,$0807,$0302,$1040,$8006,$0000,$8839,$83D8,$8599,$89FA

; Record templates
;
; Record template for 21-byte record - Boat
LC20A	DEFB $F8,$06,$04,$01,$05	
	DEFW LC481	
	DEFW $0000	
	DEFW L8C9A,L8C9A	; Boat sprite
	DEFB $18,$28,$28,$01,$00,$00	
	DEFW LC092
; Record template for 21-byte record - Meduza
LC21F	DEFB $00,$07,$04,$05,$00	
	DEFW LBF9F		; -> RET
	DEFW $0000	
	DEFW L8D43,L8D43	
	DEFB $18,$00,$00,$00,$01,$00	
	DEFW LC0A2
; Record template for 21-byte record - Round fish
LC234	DEFB $00,$09,$04,$02,$00	
	DEFW LBF9F		; -> RET
	DEFW $0000	
	DEFW L8254,L8254	
	DEFB $18,$00,$00,$00,$02,$00	
	DEFW LC0B2
; Record template for 26-byte record - Fish
LC249	DEFB $00,$00,$04,$02,$00	
	DEFW LB885	
	DEFW $0000	
	DEFW L6364,L6364	
	DEFB $18,$00,$00,$00,$02,$00	
	DEFW LC0EA	
	DEFB $00,$00,$02,$00,$00,$00
; Record template for 26-byte record - Shark
LC264	DEFB $00,$00,$04,$02,$00	
	DEFW LB885	
	DEFW $0000	
	DEFW L6A26,L6A26	
	DEFB $18,$00,$00,$00,$02,$00	
	DEFW LC102	
	DEFB $00,$00,$02,$00,$00,$00
; Record template for 26-byte record - Small squid horizontal
LC27F	DEFB $00,$00,$04,$02,$00	
	DEFW LB885	
	DEFW $0000	
	DEFW L6000,L6000	
	DEFB $18,$00,$00,$00,$02,$00	
	DEFW LC14A	
	DEFB $00,$00,$02,$00,$00,$00
; Record template for 26-byte record - Long fish
LC29A	DEFB $00,$00,$04,$02,$00	
	DEFW LB885	
	DEFW $0000	
	DEFW L7C4F,L7C4F	
	DEFB $18,$00,$00,$00,$02,$00	
	DEFW LC11A	
	DEFB $00,$00,$02,$00,$00,$00
; Record template for 26-byte record - Small fish cloud ??
LC2B5	DEFB $00,$00,$04,$02,$00	
	DEFW LB885	
	DEFW $0000	
	DEFW L7869,L7869	
	DEFB $18,$00,$00,$00,$02,$00	
	DEFW LC132	
	DEFB $00,$00,$02,$00,$00,$00
; Record template for 26-byte record - Small fish cloud ??
LC2D0	DEFB $00,$00,$04,$02,$00	
	DEFW LB885	
	DEFW $0000	
	DEFW L798A,L798A	
	DEFB $18,$00,$00,$00,$02,$00	
	DEFW LC182	
	DEFB $00,$00,$02,$00,$00,$00
; Table of 8 record templates
LC2EB	DEFW LC249	
	DEFW LC264	
	DEFW LC27F	
	DEFW LC29A	
	DEFW LC2B5	
	DEFW LC2D0	
	DEFW LC29A	
	DEFW LC249
;
; Record template for 26-byte record - Snake fish horizontal
LC2FB	DEFB $00,$00,$04,$02,$00	
	DEFW LB885	
	DEFW $0000	
	DEFW L7DD0,L7DD0	
	DEFB $18,$00,$00,$00,$02,$00	
	DEFW LC1BA	
	DEFB $00,$00,$02,$00,$00,$00
; Record template for 26-byte record - Squid
LC316	DEFB $00,$00,$04,$02,$00	
	DEFW LB885	
	DEFW $0000	
	DEFW L83D8,L83D8	
	DEFB $18,$00,$00,$00,$02,$00	
	DEFW LC1F2	
	DEFB $00,$00,$02,$00,$00,$00
; Table of 8 record templates
LC331	DEFW LC249	
	DEFW LC264	
	DEFW LC27F	
	DEFW LC29A	
	DEFW LC2B5	
	DEFW LC2D0	
	DEFW LC2FB	
	DEFW LC316
;
; Record template for 26-byte record - Round fish vertical
LC341	DEFB $00,$00,$02,$04,$00	
	DEFW LB972	
	DEFW $0000	
	DEFW L8347,L8347	
	DEFB $1C,$00,$00,$00,$00,$02	
	DEFW LC0CE	
	DEFB $00,$00,$02,$00,$00,$00
; Record template for 26-byte record - Small squid vertical
LC35C	DEFB $00,$00,$02,$04,$00	
	DEFW LB972	
	DEFW $0000	
	DEFW L628B,L628B	
	DEFB $1C,$00,$00,$00,$00,$02	
	DEFW LC166	
	DEFB $00,$00,$02,$00,$00,$00
; Record template for 26-byte record - Snake fish vertical
LC377	DEFB $00,$00,$02,$04,$00	
	DEFW LB972	
	DEFW $0000	
	DEFW L8133,L8133	
	DEFB $1C,$00,$00,$00,$00,$02	
	DEFW LC1D6	
	DEFB $00,$00,$02,$00,$00,$00
; Record template for 26-byte record - Small fish cloud ??
LC392	DEFB $00,$00,$02,$04,$00	
	DEFW LB972	
	DEFW $0000	
	DEFW L7AF5,L7AF5	
	DEFB $1C,$00,$00,$00,$00,$02	
	DEFW LC19E	
	DEFB $00,$00,$02,$00,$00,$00
; Table of 4 record templates
LC3AD	DEFW LC341	
	DEFW LC35C	
	DEFW LC377	
	DEFW LC392
;
; Record template for 28-byte record - Round fish
LC3B5	DEFB $00,$00,$04,$02,$00	
	DEFW LBA64	
	DEFW $0000	
	DEFW L8254,L8254	
	DEFB $18,$00,$00,$00,$02,$00	
	DEFW LC0B2	
	DEFB $03,$01,$02,$00,$00,$00,$00,$00	
; Record template for 28-byte record - Small squid horizontal
LC3D2	DEFB $00,$00,$04,$02,$00	
	DEFW LBA64	
	DEFW $0000	
	DEFW L6000,L6000	
	DEFB $18,$00,$00,$00,$02,$00	
	DEFW LC14A	
	DEFB $04,$02,$02,$00,$00,$00,$00,$00
; Record template for 28-byte record
LC3EF	DEFB $00,$00,$04,$02,$00	
	DEFW LBA64	
	DEFW $0000	
	DEFW L798A,L798A	
	DEFB $18,$00,$00,$00,$02,$00	
	DEFW LC182	
	DEFB $03,$01,$02,$00,$00,$00,$00,$00	
; Record template for 28-byte record - Snake fish horizontal
LC40C	DEFB $00,$00,$04,$02,$00	
	DEFW LBA64	
	DEFW $0000	
	DEFW L7DD0,L7DD0	
	DEFB $18,$00,$00,$00,$02,$00	
	DEFW LC1BA	
	DEFB $04,$02,$02,$00,$00,$00,$00,$00	
; Table of 4 record templates
LC429	DEFW LC3B5	
	DEFW LC3D2	
	DEFW LC3EF	
	DEFW LC40C

LC431	LD A,(L5B18)		; get value 5 / 10 / 15 / 20, depending on LEVEL
	LD B,A	
	LD IX,(L5B23)		; now IX = record address in block LC4F0
LC439	PUSH BC	
	DEC (IX+$0F)	
	CALL Z,LB737	
	LD BC,$001A		; 26
	ADD IX,BC	
	POP BC	
	DJNZ LC439	
	LD A,(L5B19)		; get value 12 / 27 / 42 / 57, depending on LEVEL
	LD B,A	
LC44C	PUSH BC	
	DEC (IX+$0F)	
	CALL Z,LB737	
	LD BC,$001C		; 28
	ADD IX,BC	
	POP BC	
	DJNZ LC44C	
	RET

LC45C	LD A,(L5B18)		; get value 5 / 10 / 15 / 20, depending on LEVEL
	LD B,A	
	LD IX,(L5B23)	
LC464	PUSH BC	
	CALL LB7BB	
	LD BC,$001A	
	ADD IX,BC	
	POP BC	
	DJNZ LC464	
	LD A,(L5B19)		; get value 12 / 27 / 42 / 57, depending on LEVEL
	LD B,A	
LC474	PUSH BC	
	CALL LB7BB	
	LD BC,$001C	
	ADD IX,BC	
	POP BC	
	DJNZ LC474	
	RET	

LC481	LD A,(IX+$10)	
	OR A	
	JR NZ,LC490	
	DEC (IY+$0C)	
	RET NZ	
	LD (IX+$10),$03	
	RET	
LC490	DEC A	
	JR NZ,LC4A4	
	DEC (IY+$0C)	
	RET NZ	
	LD (IX+$0E),$28	
	LD (IX+$10),$02	
	LD (IX+$11),$FF	
	RET	
LC4A4	DEC A	
	JR NZ,LC4BA	
	DEC (IX+$0E)	
	LD A,(IX+$0E)	
	CP $08	
	RET NZ	
	LD (IX+$10),$00	
	LD A,R	
	LD (IY+$0C),A	
	RET	
LC4BA	INC (IX+$0E)	
	LD A,(IX+$0E)	
	CP $28	
	RET NZ	
	LD (IX+$0E),$64	
	LD (IX+$11),$00	
	LD (IX+$10),$01	
	LD A,R	
	LD (IY+$0C),A	
	RET	

; Delay ??
LC4D5	OR A	
	RET Z	
LC4D7	LD BC,(L5B25)		; get value 150 / 100 / 50 / 1, depending on Game level
	LD DE,$0000	
	LD HL,$0000	
	LDIR	
	DEC A	
	JR NZ,LC4D7	
	RET

LC4E7	DEFB $FB,$02,$00,$2C,$42,$4C,$46,$49	
	DEFB $4C

; Object records:
;  1. 21-byte records at LC4F0, count in L5B15: Boat, meduzas, round fishes
;  2. 26-byte records at (L5B1F), count in L5B16
;  3. 26-byte records at (L5B21), count in L5B17
;  4. 26-byte records at (L5B23), count in L5B18
;  5. 28-byte records, count in L5B19
; Record format:
; word	(IX+$00),(IX+$01) - column, row 0..255
; byte	(IX+$02) - ?? 0..7
; byte	(IX+$03) - ?? 0..7
;	(IX+$04)
;	(IX+$07),(IX+$08) - address ??
; word	(IX+$09),(IX+$0A) - sprite address
; word	(IX+$0B),(IX+$0C) - sprite address
;	(IX+$0D) - bits 0,1
;	(IX+$0E)
;	(IX+$0F)
;	(IX+$11)
;	(IX+$12)
; word	(IX+$13),(IX+$14) - record address in block LC092
LC4F0	DEFB $2B,$31
LC4F2	DEFB $30,$32,$33,$0D,$DC,$3C	
	DEFB $47,$43,$32,$20,$4A,$52,$20,$47
;	DEFS 5
;LC505
;LC51A
;LC52F
;LC544
;LC559

;----------------------------------------------------------------------------
