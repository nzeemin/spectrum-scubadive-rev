;----------------------------------------------------------------------------

	INCLUDE "scubacodb.txt"

;----------------------------------------------------------------------------

	ORG $D900

	DEFS $90	

; Game
LD990	DI	
	PUSH IX	
	PUSH IY	
	LD HL,$0000	
	LD (L5B44),HL	
	LD (LDEFF),HL	
	LD A,$03	; Number of lives
	LD (L5B37),A	; set the initial value
LD9A3	CALL L9DAA	; Prepare the world mini-map (AC5D table)
	CALL LDBC2	; Initialize variables depending of Game level
	CALL LBEDB	
	LD HL,LE361	
	LD (HL),$00	
LD9B1	CALL LDAAD	; Prepare game screen and some variables
	CALL LE6AB	
	LD HL,(L5B03)	; get Screen position on mini-map
	CALL L9C56	; Draw game screen
	CALL LBDBA	
LD9C0	CALL LB213	
	CALL LBDEA	
	CALL LE2A8	
	LD IX,LE33B	; Diver object record address
	BIT 4,(IX+$26)	
	JR NZ,LD9DB	
	BIT 5,(IX+$10)	
	JR NZ,LD9F4	
	JR LD9C0	
LD9DB	LD A,(L5B37)	; get Number of lives
	CP $04	
	JR NZ,LD9E7	
	LD A,$03	
	LD (L5B37),A	; set Number of lives
LD9E7	LD A,(L5B10)	; Game level 1..4
	CP $04	
	JR Z,LD9EF	
	INC A	
LD9EF	LD (L5B10),A	; Save game level 1..4
	JR LD9A3	
LD9F4	LD B,$00	
LD9F6	PUSH BC	
	CALL LB213	
	CALL LBDEA	
	CALL LE2A8	
	POP BC	
	DJNZ LD9F6	
	LD A,(L5B00)	
	CP $08	
	JR NZ,LDA1E	
	LD A,(L5B0F)	
	CP $03	
	JR NZ,LDA1E	
	LD A,(L5B37)	; get Number of lives
	CP $01	
	JR Z,LDA1E	; No more lives? => no lives, jump
	DEC A	
	LD (L5B37),A	; set Number of lives
	JR LD9DB	
LDA1E	LD A,(L5B37)	; get Number of lives
	CP $01	
	JR Z,LDA33	
	DEC A		; One live less
	LD (L5B37),A	; set Number of lives
	LD HL,LE361	
	LD A,(HL)	
	AND $20	
	LD (HL),A	
	JP LD9B1	
LDA33	POP IY		; Ending the Game routine, the game is over
	POP IX	
	EI	
	RET

; Clear screen, fill attributes with A
LDA39	LD (L5B4A),A	
	LD HL,$4000	
	LD BC,$1800	
LDA42	LD (HL),$00	
	INC HL	
	DEC BC	
	LD A,B	
	OR C	
	JR NZ,LDA42	
	LD A,(L5B4A)	
	LD E,A	
	LD BC,$0300	
LDA51	LD (HL),E	
	INC HL	
	DEC BC	
	LD A,B	
	OR C	
	JR NZ,LDA51	
	RET	

; Print char and shift current position right
LDA59	LD HL,($DC7E)	
	INC L	
	LD ($DC7E),HL	
	DEC L	
	JR $DA6B	
; Print char and shift current position down
LDA63	LD HL,($DC7E)	
	INC H	
	LD ($DC7E),HL	
	DEC H	
; Print char
LDA6B	PUSH HL	
	PUSH AF	
	CALL LA164	; Convert char coords HL to ZX screen address
	EX DE,HL	
	POP AF	
	LD L,A	
	LD H,$00	
	RES 7,L	
	ADD HL,HL	
	ADD HL,HL	
	ADD HL,HL	; *8
	BIT 7,A	
	JR NZ,LDA83	
	LD BC,$3D00	; ROM font address, for chars $20..$7F
	JR $DA86	
LDA83	LD BC,LDC80	; Tiles 8x8 address, for chars $80..$AE
LDA86	ADD HL,BC	
	LD B,$08	
LDA89	LD A,(HL)	
	LD (DE),A	
	INC HL	
	INC D	
	DJNZ LDA89	
	POP HL	
	CALL LA14C	; Get screen attribute address
	LD A,(L5B4A)	
	LD (HL),A	
	RET	

; Print string
; I:HL	String address
; I:BC	Row and column
; I:DE	Print char procedure address
LDA98	LD (LDAA2+1),DE	
;
LDA9C	LD (LDC7E),BC	
	LD A,(HL)	
LDAA1	PUSH HL	
LDAA2	CALL LDA59	; Print char and shift !!! mutable argument DA59 / DA63
	POP HL	
	INC HL	
	LD A,(HL)	
	CP $FF	
	JR NZ,LDAA1	
	RET

; Prepare game screen and some variables
LDAAD	LD A,$30	
	CALL LDA39	; Clear screen with attribute A
	LD A,$01	
	OUT ($FE),A	
	LD HL,LDC09	; Indicator top border
	LD BC,$0018	
	LD DE,LDA59	; Procedure Print char and shift right
	CALL LDA98	; Print string
	LD HL,LDC12	; Indicator bottom border
	LD BC,$1718	
	CALL LDA9C	; Print string
	LD HL,LDC1B	; "HIGH"
	LD BC,$011A	
	CALL LDA9C	; Print string
	LD HL,LDC27	; "SCORE"
	LD BC,$031A	
	CALL LDA9C	; Print string
	LD HL,LDC2D	; "HELD"
	LD BC,$051A	
	CALL LDA9C	; Print string
	LD HL,LDC32	; Indicator left/right border
	LD BC,$0118	
	LD DE,LDA63	; Procedure Print char and shift down
	CALL LDA98	; Print string
	LD HL,LDC32	; Indicator left/right border
	LD BC,$011F	
	CALL LDA9C	; Print string
	LD HL,LDC49	; "OXYGEN"
	LD BC,$0C1A	
	CALL LDA9C	; Print string
	LD HL,LDC50	; "DEPTH"
	LD BC,$0C1C	
	CALL LDA9C	; Print string
	LD HL,LDC56	; "SKILL LIVES"
	LD BC,$081E	
	CALL LDA9C	; Print string
	LD A,$38	
	LD (L5B4A),A	
	LD HL,LDC65	; Vertical gauge
	LD BC,$0719	
	CALL LDA9C	
	LD HL,LDC65	; Vertical gauge
	LD BC,$071B	
	CALL LDA9C	; Print string
	LD A,$0D	
	LD (L5B4A),A	
	LD HL,LDC76	; "1 2 3 4"
	LD BC,$071D	
	CALL LDA9C	; Print string
	LD HL,LDC76	; "1 2 3 4"
	LD BC,$101D	
	CALL LDA9C	; Print string
	LD HL,$58FB	
	LD (LDE55),HL	
	LD (HL),$28	
	DEC HL	
	DEC HL	
	LD (LDE57),HL	
	LD (HL),$20	
	LD A,(L5B10)	; Game level 1..4
	DEC A	
	RRCA	
	RRCA	
	LD E,A	
	LD D,$00	
	LD HL,$58FD	
	ADD HL,DE	
	LD (HL),$4F	
	LD A,(L5B37)	; get Number of lives
	DEC A	
	RRCA	
	RRCA	
	LD E,A	
	LD D,$00	
	LD HL,$5A1D	
	ADD HL,DE	
	LD (LDE59),HL	
	LD (HL),$4F	
	LD IX,LE33B	; Diver object record address
	CALL LDED9	
	CALL LDEE5	
	CALL LDEF1	
	LD A,(L5B10)	; Game level 1..4
	DEC A	
	ADD A,A	
	ADD A,A	
	ADD A,A	
	ADD A,A		; A = ([Game level] - 1) * 16 => 0 / 16 / 32 / 48
	LD E,A	
	LD D,$00	
	LD HL,LDDF0	
	ADD HL,DE	; HL = $DDF0 + ([Game level] - 1) * 16
	LD DE,L5B27	
	LD BC,$000E	
	LDIR	
	LD A,(L5B10)	; Game level 1..4
	LD C,A	
	LD A,$05	
	SUB C	
	LD (IX+$1C),A	
	LD (IX+$1E),A	
	LD C,A	
	LD A,$16	
	SUB C	
	LD (IX+$1D),A	
	LD (IX+$1B),A	
	LD A,(L5B10)	; Game level 1..4
	ADD A,A	
	LD C,A	
	LD A,$0A	
	SUB C	
	ADD A,$02	
	LD (L5B0E),A	; = 10 - [Game level] * 2 + 2 => 10 / 8 / 6 / 4
	SUB $03	
	LD (L5B0D),A	; = 10 - [Game level] * 2 + 2 - 3 => 7 / 5 / 3 / 1
	RET

; Initialize variables depending of Game level
LDBC2	LD A,(L5B10)	; Game level 1..4
	ADD A,A	
	ADD A,A		; *4
	ADD A,$09	; A = [Game level] * 4 + 9 => 13 / 17 / 21 / 25
	LD (L5B13),A	
	LD (L5B14),A	
	ADD A,$05	; A = [Game level] * 4 + 9 + 5 => 18 / 22 / 26 / 31
	LD (L5B16),A	
	SUB $05	
	ADD A,A	
	INC A		; A = ([Game level] * 4 + 9) * 2 + 1 => 27 / 35 / 43 / 51
	LD (L5B15),A	
	DEC A		; A = ([Game level] * 4 + 9) * 2 => 26 / 34 / 42 / 50
	LD (L5B17),A	
	LD A,(L5B10)	; Game level 1..4
	LD C,A	
	ADD A,A	
	ADD A,A	
	ADD A,C		; A = [Game level] * 5 => 5 / 10 / 15 / 20
	LD (L5B18),A	
	LD C,A	
	ADD A,A	
	ADD A,C	
	SUB $03		; A = [Game level] * 5 * 3 - 3 => 12 / 27 / 42 / 57
	LD (L5B19),A	
	LD HL,$0001	
	LD (L5B25),HL	
	LD A,(L5B10)	; Game level 1..4
	NEG	
	ADD A,$04	; 3 / 2 / 1 / 0
	RET Z	
	LD B,A	
	XOR A	
LDC01	ADD A,$32	
	DJNZ LDC01	
	LD (L5B25),A	
	RET	

LDC09	DEFB $80,$81,$81,$81,$81,$81,$81,$85,$FF	; Indicator top border
LDC12	DEFB $83,$81,$81,$81,$81,$81,$81,$84,$FF	; Indicator bottom border
LDC1B	DEFB $28,$29,$27,$28,$FF			; "HIGH"
LDC20	DEFB $10,$10,$10,$10,$10,$10,$FF		; "000000"
LDC27	DEFB $33,$23,$2F,$32,$25,$FF			; "SCORE"
LDC2D	DEFB $28,$25,$2C,$24,$FF			; "HELD"
LDC32	DEFB $82,$82,$82,$82,$82,$82,$82,$82		; Indicator left/right border
	DEFB $82,$82,$82,$82,$82,$82,$82,$82
	DEFB $82,$82,$82,$82,$82,$82,$FF
LDC49	DEFB $2F,$38,$39,$27,$25,$2E,$FF		; "OXYGEN"
LDC50	DEFB $24,$25,$30,$34,$28,$FF			; "DEPTH"
LDC56	DEFB $33,$2B,$29,$2C,$2C,$00,$00,$00		; "SKILL LIVES"
	DEFB $00,$2C,$29,$36,$25,$33,$FF
LDC65	DEFB $86,$87,$87,$87,$87,$87,$87,$87		; Vertical gauge
	DEFB $87,$87,$87,$87,$87,$87,$87,$88
	DEFB $FF
LDC76	DEFB $11,$00,$12,$00,$13,$00,$14,$FF		; "1 2 3 4"

LDC7E	DEFW $0000

LDC80	DEFB $00,$7F,$40,$58,$58,$40,$43,$42
	DEFB $00,$FF,$00,$18,$18,$00,$FF,$00
	DEFB $42,$42,$42,$5A,$5A,$42,$42,$42
	DEFB $42,$43,$40,$58,$58,$40,$7F,$00
	DEFB $42,$C2,$02,$1A,$1A,$02,$FE,$00
	DEFB $00,$FE,$02,$1A,$1A,$02,$C2,$42
	DEFB $FF,$81,$81,$E7,$81,$81,$81,$81
	DEFB $81,$81,$81,$E7,$81,$81,$81,$81
	DEFB $81,$81,$81,$E7,$81,$81,$81,$FF
	DEFB $07,$0F,$1F,$7F,$3F,$3E,$7E,$1F
	DEFB $47,$C3,$61,$7F,$7F,$7F,$7F,$1D
	DEFB $04,$0F,$0F,$7F,$3E,$78,$70,$E0
	DEFB $70,$38,$7C,$7F,$7F,$1F,$07,$02
	DEFB $0C,$1C,$18,$78,$78,$F8,$70,$78
	DEFB $78,$70,$FC,$7F,$7F,$3F,$1F,$00
	DEFB $D2,$FF,$76,$70,$F0,$F9,$7F,$7F
	DEFB $7F,$79,$F0,$F8,$70,$7C,$7F,$50
	DEFB $02,$1F,$3F,$3F,$7D,$78,$F8,$7B
	DEFB $FF,$7F,$7E,$7A,$18,$70,$E0,$60
	DEFB $80,$D0,$F8,$FF,$FE,$DE,$0E,$02
	DEFB $E0,$E0,$F8,$FC,$FC,$FC,$F0,$98
	DEFB $00,$E0,$F0,$F8,$70,$30,$00,$00
	DEFB $00,$00,$0C,$74,$F8,$F8,$E0,$80
	DEFB $70,$F8,$78,$FC,$38,$3C,$18,$38
	DEFB $3C,$18,$38,$30,$F8,$FC,$F0,$C0
	DEFB $C0,$F0,$F0,$FC,$F8,$E0,$E0,$80
	DEFB $C0,$70,$78,$7C,$F8,$F0,$E0,$40
	DEFB $00,$80,$C0,$F0,$E0,$78,$F8,$F0
	DEFB $F8,$F0,$78,$38,$78,$38,$70,$78
	DEFB $B1,$7F,$7F,$7F,$FD,$78,$78,$FC
	DEFB $F0,$70,$78,$70,$F1,$7F,$7F,$CC
	DEFB $03,$07,$01,$03,$01,$03,$00,$01
	DEFB $00,$00,$01,$01,$03,$01,$07,$03
	DEFB $F8,$F0,$78,$7C,$FE,$FE,$7E,$7E
	DEFB $7F,$FF,$FF,$7F,$3F,$1F,$1F,$0A
	DEFB $00,$FF,$FF,$67,$E0,$60,$60,$6E
	DEFB $FF,$F8,$60,$70,$76,$7F,$3F,$01
	DEFB $60,$F4,$FE,$FE,$BF,$3E,$3F,$3F
	DEFB $3F,$1F,$3F,$7F,$FF,$FE,$FC,$EC
	DEFB $80,$C0,$E0,$E0,$E0,$C0,$C0,$C0
	DEFB $80,$C0,$C0,$C0,$80,$C0,$E0,$80
	DEFB $0B,$1F,$1E,$3E,$7F,$3F,$7C,$FC
	DEFB $FC,$FC,$F8,$F8,$F0,$F8,$F0,$E0
	DEFB $80,$F8,$F0,$F0,$78,$00,$00,$00
	DEFB $80,$00,$00,$00,$80,$E0,$F0,$C0
	DEFB $00,$00,$00,$00,$00,$00,$00,$00

; Blocks of 14 bytes to copy to 5B27, blocks aligned to 16 bytes
LDDF0	DEFW $0002,$0005,$004B,$0019,$0032,$00FA,$0019,$0000	; Level 1
	DEFW $0004,$000A,$0032,$0096,$0064,$01F4,$0032,$0000	; Level 2
	DEFW $0006,$000F,$0096,$00E1,$004B,$02EE,$004B,$0000	; Level 3
	DEFW $0008,$0014,$0064,$00C8,$012C,$03E8,$0064		; Level 4

LDE2E	LD (HL),$38	
	RRCA	
	RRCA	
	RRCA	
	LD H,A	
	AND $E0	
	LD L,A	
	LD A,H	
	AND $01	
	LD H,A	
	ADD HL,DE	
	LD (HL),C	
	RET	

LDE3E	LD HL,(LDE55)	
	LD DE,$58FB	
	LD C,$28	
	LD A,(L5B03+1)	
	INC A	
	SRL A	
	AND $0F	
	CALL LDE2E	
	LD (LDE55),HL	
	RET

LDE55	DEFW $0000
LDE57	DEFW $0000
LDE59	DEFW $0000
LDE5B	DEFW $FFFF

LDE5D	LD (LDE5B),HL	
	LD A,H	
	SRL A	
	SRL A	
	SRL A	
	SRL A	
	LD H,A	
	LD A,$0F	
	SUB H	
	LD DE,$58F9	
	LD C,$20	
	LD HL,(LDE57)	
	CP $0E	
	JR C,LDE7E	
	CALL LE645	
	LD C,$10	
LDE7E	CALL LDE2E	
	LD (LDE57),HL	
	RET	

LDE85	LD (IX+$23),B	
	PUSH DE	
	LD IY,LE5E0	
LDE8D	LD C,$FF	
	LD E,(IY+$00)	
	LD D,(IY+$01)	
	BIT 7,D	
	JR Z,LDEA0	
	LD (IX+$23),D	
	LD C,$00	
	JR LDEB6	
LDEA0	INC C	
	OR A	
	SBC HL,DE	
	JR NC,LDEA0	
	ADD HL,DE	
	INC IY	
	INC IY	
	LD A,(IX+$23)	
	OR A	
	JR Z,LDEB6	
	DEC (IX+$23)	
	JR LDE8D	
LDEB6	EX (SP),HL	
	EX DE,HL	
	LD B,$00	
	SLA C	
	SLA C	
	SLA C	
	LD HL,$3D80	; ???
	ADD HL,BC	
	LD B,$08	
	PUSH DE	
LDEC7	LD A,(HL)	
	LD (DE),A	
	INC HL	
	INC D	
	DJNZ LDEC7	
	POP DE	
	INC E	
	EX DE,HL	
	EX (SP),HL	
	BIT 7,(IX+$23)	
	JR Z,LDE8D	
	POP DE	
	RET

LDED9	LD B,$00	
	LD HL,(L5B4B)	
	LD DE,$4059	
	CALL LDE85	
	RET

LDEE5	LD B,$00	
	LD HL,(L5B44)	
	LD DE,$4099	
	CALL LDE85	
	RET

LDEF1	LD B,$02	
	LD HL,(LDEFF)	
	LD DE,$40DA	
	CALL LDE85	
	RET

LDEFD	DEFB $00,$00	
LDEFF	DEFB $00,$00
LDF01	DEFB $00,$00,$00,$00,$00,$00,$02,$03
	DEFB $02,$03,$18,$10,$00,$30
LDF0F	DEFB $00,$00
LDF11	DEFB $00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00

; Table: Angle 0..15 -> (DX, DY)
LDF25	DEFB $00,$FE,$01,$FE,$02,$FE,$02,$FF,$02,$00,$02,$01,$02,$02,$01,$02	
	DEFB $00,$02,$FF,$02,$FE,$02,$FE,$01,$FE,$00,$FE,$FF,$FE,$FE,$FF,$FE	

LDF45	DEFB $00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00	

; I:IX = Object address = $E33B
LDFD5	LD HL,(LB7B9)	
	LD DE,(L5B35)	
	LD (L5B35),HL	
	LD (LB7B9),DE	
	LD IY,LDF01	
	LD HL,LDF11	
	LD (LDF0F),HL	
	LD A,(IX+$09)	
	LD (IX+$0B),A	
	LD A,(IX+$0A)	
	LD (IX+$0C),A	
	BIT 7,(IX+$10)	
	JR Z,LE021	
	LD A,(IX+$06)	; get Angle 0..15
	ADD A,A	
	LD C,A	
	LD B,$00	
	LD HL,LDF25	; Table base address
	ADD HL,BC	
	LD A,(HL)	; get DX value for the Angle
	LD (IX+$04),A	; set DX value
	INC HL	
	LD A,(HL)	; get DY value for the Angle
	LD (IX+$05),A	; set DY value
	BIT 0,(IX+$10)	; check "moving" bit
	JR NZ,LE021	
	LD (IX+$04),$00	; clear DX value
	LD (IX+$05),$00	; clear DY value
LE021	LD A,(IX+$13)	; get X value
	LD HL,(L5B03)	; get Screen position on mini-map
	ADD A,(IX+$04)	; add DX
	LD (IX+$13),A	; set X value
	SRL A	
	AND $07	
	CP (IX+$02)	
	LD (IX+$15),A	
	JR Z,LE08D	
	CP $01	
	JR Z,LE068	
	CP $07	
	JR NZ,LE08D	
	BIT 7,(IX+$04)	; set DX value
	JR Z,LE08D	
	BIT 7,(IX+$11)	
	JR Z,LE08D	
	DEC (IX+$00)	
	RES 4,(IX+$0D)	
	LD A,(IX+$00)	
	CP (IX+$1C)	
	JR NC,LE08D	
	ADD A,$08	
	LD (IX+$00),A	
	SET 2,(IX+$10)	
	DEC L	
	JR LE08D	
LE068	BIT 7,(IX+$04)	
	JR NZ,LE08D	
	BIT 7,(IX+$11)	
	JR NZ,LE08D	
	INC (IX+$00)	
	RES 4,(IX+$0D)	
	LD A,(IX+$00)	
	CP (IX+$1B)	
	JR C,LE08D	
	SUB $08	
	LD (IX+$00),A	
	SET 2,(IX+$10)	
	INC L	
LE08D	LD A,(IX+$14)	; get Y value
	ADD A,(IX+$05)	; add DY
	LD (IX+$14),A	; set Y value
	SRL A	
	AND $07	
	CP (IX+$03)	
	LD (IX+$16),A	
	JP Z,LE120	
	CP $01	
	JR Z,LE0EA	
	CP $07	
	JP NZ,LE120	
	BIT 7,(IX+$05)	; check DY value - moving up?
	JR Z,LE120	
	BIT 7,(IX+$12)	
	JR Z,LE120	
	LD A,(IX+$1A)	
	CP (IX+$28)	
	JR NZ,LE0CC	
	LD A,(IX+$03)	
	LD (IX+$16),A	
	ADD A,A	
	LD (IX+$14),A	; set Y value
	JR LE120	
LE0CC	DEC (IX+$01)	
	DEC (IX+$1A)	
	RES 3,(IX+$0D)	
	LD A,(IX+$01)	
	CP (IX+$1E)	
	JR NC,$E120	
	ADD A,$08	
	LD (IX+$01),A	
	DEC H	
	SET 2,(IX+$10)	
	JR LE120	
LE0EA	BIT 7,(IX+$05)	; check DY value - moving up?
	JR NZ,LE120	
	BIT 7,(IX+$12)	
	JR NZ,LE120	
	INC (IX+$01)	
	INC (IX+$1A)	
	BIT 5,(IX+$26)	
	JR NZ,LE10A	
	LD A,(IX+$1A)	
	CP $46	
	CALL Z,LE9B0	
LE10A	RES 3,(IX+$0D)	
	LD A,(IX+$01)	
	CP (IX+$1D)	
	JR C,LE120	
	SUB $08	
	LD (IX+$01),A	
	INC H	
	SET 2,(IX+$10)	
LE120	LD A,(IX+$0D)	
	AND $18	
	CP $18	
	JR Z,LE174	
	BIT 2,(IX+$10)	
	JR Z,LE171	
	RES 1,(IX+$0D)	
	PUSH IX	
	PUSH IY	
	LD A,L	
	AND $1F	
	LD L,A	
	LD A,H	
	AND $1F	
	LD H,A	
	LD (L5B03),HL	; set Screen position on mini-map
	CALL L9C56	; Draw game screen
	CALL LDE3E	
	LD HL,(LB7B9)	
	LD DE,(L5B35)	
	LD (LB7B9),DE	
	LD (L5B35),HL	
	CALL LBDBA	
	LD HL,(LB7B9)	
	LD DE,(L5B35)	
	LD (LB7B9),DE	
	LD (L5B35),HL	
	POP IY	
	POP IX	
	RES 2,(IX+$10)	
	JR LE174	
LE171	CALL LB3AB	
LE174	SET 4,(IX+$0D)	
	SET 3,(IX+$0D)	
	LD E,(IX+$04)	
	LD A,(IX+$15)	
	CP (IX+$02)	
	JR Z,LE18D	
	LD (IX+$02),A	
	LD (IX+$11),E	
LE18D	LD E,(IX+$05)	
	LD A,(IX+$16)	
	CP (IX+$03)	
	JR Z,LE19E	
	LD (IX+$12),E	
	LD (IX+$03),A	
LE19E	BIT 7,(IX+$10)	
	JR NZ,LE1B8	
	LD A,(IX+$24)	
	LD (IX+$09),A	
	LD (IX+$0B),A	
	LD A,(IX+$25)	
	LD (IX+$0A),A	
	LD (IX+$0C),A	
	JR LE1F4	
LE1B8	BIT 4,(IX+$10)	
	JR NZ,LE1C4	
	BIT 5,(IX+$10)	
	JR Z,LE1D2	
LE1C4	LD A,(IX+$09)	
	LD (IX+$0B),A	
	LD A,(IX+$0A)	
	LD (IX+$0C),A	
	JR LE1F4	
LE1D2	INC (IX+$17)	
	LD H,(IX+$06)	; get Angle 0..15
	LD A,(IX+$17)	
	AND $03	
	RRCA	
	RRCA	
	LD L,A	
	SRL H	
	RR L	
	LD DE,L92CC	; Diver sprites base address
	ADD HL,DE	; now HL = diver sprite address
	LD (IX+$0A),H	
	LD (IX+$0C),H	
	LD (IX+$09),L	
	LD (IX+$0B),L	
LE1F4	CALL LB3A6	
	LD A,(IX+$01)	
	LD (IX+$19),A	; set Row
	LD IY,(LB7B9)	
	LD D,(IX+$08)	
	LD E,(IX+$07)	
	BIT 0,(IX+$0D)	
	JR NZ,LE211	
	LD IY,LA41B	
LE211	BIT 1,(IX+$0D)	
	JR NZ,LE21A	
	LD DE,LA41B	
LE21A	LD B,$03	
LE21C	PUSH BC	
	LD A,(IX+$00)	
	LD (IX+$18),A	; set Column
	LD L,A	
	LD H,(IX+$19)	; get Row
	CALL LA164	; Convert char coords HL to ZX screen address
	LD B,$03	
LE22C	PUSH BC	
	PUSH HL	
	PUSH IY	
	PUSH DE	
	LD B,$08	
LE233	LD A,(DE)	
	XOR (HL)	
	LD C,A	
	AND (IY+$00)	
	JR Z,LE23F	
	SET 1,(IX+$10)	
LE23F	LD A,(IY+$00)	
	XOR C	
	LD (HL),A	
	INC H	
	INC DE	
	INC DE	
	INC DE	
	INC IY	
	INC IY	
	INC IY	
	DJNZ LE233	
	INC (IX+$18)	; one Column right
	BIT 1,(IX+$10)	
	JR Z,LE26E	
	LD HL,(LDF0F)	
	LD A,(IX+$19)	; get Row
	LD (HL),A	
	INC HL	
	LD A,(IX+$18)	; get Column
	DEC A	
	LD (HL),A	
	INC HL	
	LD (LDF0F),HL	
	RES 1,(IX+$10)	
LE26E	POP DE	
	INC DE	
	POP IY	
	INC IY	
	POP HL	
	INC HL	
	POP BC	
	DJNZ LE22C	
	INC (IX+$19)	; one Row down
	LD BC,$0015	
	ADD IY,BC	
	EX DE,HL	
	ADD HL,BC	
	EX DE,HL	
	POP BC	
	DJNZ LE21C	
	LD HL,(LB7B9)	
	LD BC,(L5B35)	
	LD D,(IX+$08)	
	LD E,(IX+$07)	
	LD (LB7B9),BC	
	LD (L5B35),DE	
	LD (IX+$08),H	
	LD (IX+$07),L	
	LD HL,(LDF0F)	
	LD (HL),$FF	
	RET

LE2A8	LD IX,LE33B	; Diver object record address
	DEC (IX+$0F)	
	JR NZ,LE2CC	
	LD A,(IX+$0E)	; get speed factor
	LD (IX+$0F),A	
	RES 1,(IX+$0D)	
	BIT 0,(IX+$0D)	
	JR Z,LE2C5	
	SET 1,(IX+$0D)	
LE2C5	SET 0,(IX+$0D)	
	CALL LDFD5	
LE2CC	DEC (IX+$20)	
	RET NZ	
	LD (IX+$20),$14	
	CALL LE2DB	; Read keyboard input
	CALL LE364	
	RET	

; Read keyboard input
; I: IX = Object address = $E33B
LE2DB	BIT 3,(IX+$10)	
	RET NZ	
	LD E,(IX+$06)	; get Angle
	LD BC,(L5B38)	; get port for Clockwise key
	LD A,(L5B3A)	; get bit mask for Clockwise key
	LD D,A	
	IN A,(C)	; read the port for Clockwise key
	AND D	
	JR NZ,LE2F1	; not pressed => skip rotate
	INC E		; rotate clockwise
LE2F1	LD BC,(L5B3B)	; get port for Anticlockwise key
	LD A,(L5B3D)	; get bit mask for Anticlockwise key
	LD D,A	
	IN A,(C)	; read the port for Anticlockwise key
	AND D	
	JR NZ,LE2FF	; not pressed => skip rotate
	DEC E		; rotate anticlockwise
LE2FF	LD A,$0F	
	AND E	
	LD (IX+$06),A	; set Angle 0..15
	LD BC,(L5B3E)	; get port for Accelerate key
	LD A,(L5B40)	; get bit mask for Accelerate key
	LD D,A	
	IN A,(C)	; read the port for Accelerate key
	AND D	
	JR NZ,LE31F	; not pressed => skip
	LD A,(IX+$0E)	
	SET 0,(IX+$10)	; set "moving" bit
	CP $08	
	RET Z	
	DEC (IX+$0E)	
LE31F	LD BC,(L5B41)	; get port for Decelerate key
	LD A,(L5B43)	; get bit mask for Decelerate key
	LD D,A	
	IN A,(C)	; read the port for Decelerate key
	AND D	
	RET NZ		; not pressed => return
	LD A,(IX+$0E)	
	CP $14	
	JR NZ,LE337	
	RES 0,(IX+$10)	; clear "moving" bit - diver stopped
	RET	
LE337	INC (IX+$0E)	
	RET

; Object record
LE33B	DEFB $0A	; (IX+$00) Column 0..31
LE33C	DEFB $0A	; (IX+$01) Row
LE33D	DEFB $04	; (IX+$02) ???
LE33E	DEFB $04	
LE33F	DEFB $00	; (IX+$04) DX value for the Angle, -2..2, see table DF25
LE340	DEFB $00	; (IX+$05) DY value for the Angle, -2..2, see table DF25
LE341	DEFB $04	; (IX+$06) Angle 0..15, initially 4
LE342	DEFB $8D	
LE343	DEFB $DF	; (IX+$08) ???
LE344	DEFW $0000	; (IX+$09) Sprite address
LE346	DEFW $0000	; (IX+$0B) Sprite address
LE348	DEFB $18	; (IX+$0D) ??? bits 0/1/2/3/4/5/6/7
LE349	DEFB $14	; (IX+$0E) speed factor ??? 12 20 40 100; $08 max speed, $14 min speed
LE34A	DEFB $14	; (IX+$0F) ??? $03
LE34B	DEFB $00	; (IX+$10) ??? bits 0/1/2/3/4/5/6/7; bit0: 1 = diver moving, 0 = diver stopped
LE34C	DEFB $00	; (IX+$11) ??? $00 $FF
LE34D	DEFB $00	; (IX+$12) ??? $00
LE34E	DEFB $08	; (IX+$13) X value
LE34F	DEFB $08	; (IX+$14) Y value
LE350	DEFB $00	; (IX+$15) X shift 0..7
LE351	DEFB $00	; (IX+$16) Y shift 0..7
LE352	DEFB $00	; (IX+$17) ???
LE353	DEFB $00	; (IX+$18) Column 0..31
LE354	DEFB $00	; (IX+$19) Row
LE355	DEFB $0A,$12	
LE357	DEFB $03	; (IX+$1C) ???
LE358	DEFB $12,$03	
LE35A	DEFB $40	; (IX+$1F) Row??
LE35B	DEFB $00	; (IX+$20) ??? $03
LE35C	DEFB $00	; (IX+$21) ???
LE35D	DEFB $00	; (IX+$22) ??? $F5
LE35E	DEFB $00	
LE35F	DEFW $0000	; (IX+$24) Sprite address
LE361	DEFB $00	; (IX+$26) ??? bits 0/1/2/3/4/5/6
LE362	DEFB $00	; (IX+$27) ???
LE363	DEFB $00	; (IX+$28) ???

; I: IX = Object address = $E33B
LE364	BIT 5,(IX+$10)	
	RET NZ	
	BIT 7,(IX+$10)	
	JP Z,LE767	
	LD HL,(LDE5B)	
	LD A,(L5B10)	; Game level 1..4
	ADD A,A	
	ADD A,A	
	ADD A,A	
	ADD A,$0A	
	LD E,A	
	LD D,$00	
	LD A,H	
	SBC HL,DE	
	LD (LDE5B),HL	
	CP H	
	CALL NZ,LDE5D	
	LD A,(LDE5B+1)	
	AND $F0	
	JR NZ,LE393	
	CALL LE43A	; Explosion
	RET	
LE393	LD A,(IX+$22)	
	OR A	
	JR Z,LE39C	
	INC (IX+$22)	
LE39C	BIT 4,(IX+$10)	
	CALL NZ,LE41B	
	LD HL,LDF11	
LE3A6	LD A,(HL)	
	CP $FF	
	RET Z	
	BIT 7,(IX+$10)	
	RET Z	
	LD D,A	
	INC HL	
	LD E,(HL)	
	INC HL	
	PUSH HL	
	EX DE,HL	
	PUSH HL	
	CALL LA14C	; Get screen attribute address
	LD A,(HL)	
	POP HL	
	CP $02	
	JP NZ,LE476	
;
LE3C0	BIT 4,(IX+$10)	
	JR NZ,LE418	
	LD A,(IX+$22)	
	CP $FF	
	JR NZ,LE3D2
;
LE3CD	CALL LE43A	; Explosion
	JR LE418	
LE3D2	OR A	
	JR NZ,LE418	
	SET 3,(IX+$10)	
	LD (IX+$0E),$0A	
	RES 0,(IX+$10)	; clear "moving" bit
	SET 4,(IX+$10)	
	LD A,(IX+$06)	; get Angle 0..15
	INC A	
	AND $0C		; 0 / 4 / 8 / 12
	ADD A,A		; 0 / 8 / 16 / 24
	ADD A,A		; 0 / 16 / 32 / 48
	ADD A,A		; 0 / 32 / 64 / 96
	LD C,A	
	LD B,$00	
	LD HL,L9ACC	; Base address for 4 sprites of drowned diver
	ADD HL,BC	
	LD (IX+$09),L	
	LD (IX+$0B),L	
	LD (IX+$0A),H	
	LD (IX+$0C),H	
	LD (IX+$21),$E6	
	LD HL,$0000	
	LD (LDEFF),HL	
	CALL LDEF1	
	RES 6,(IX+$10)	
	LD HL,LE604	; Melody address
	CALL LE5EC	; Play melody
;
LE418	POP HL	
	JR LE3A6

; I: IX = Object record address
LE41B	INC (IX+$21)	
	RET NZ	
	LD A,(IX+$06)	; get Angle
	ADD A,$08	; rotate 180 degree
	AND $0F		; 0..15
	LD (IX+$06),A	; set Angle
	LD (IX+$22),$F5	
	RES 4,(IX+$10)	
	RES 3,(IX+$10)	
	LD (IX+$0E),$14	
	RET

; Diver explosion ??
; I: IX = Object address = $E33B
LE43A	BIT 3,(IX+$26)	
	RET NZ	
	SET 5,(IX+$10)	
	SET 3,(IX+$10)	
	RES 0,(IX+$10)	; clear "moving" bit
	LD A,(IX+$06)	; get Angle 0..15
	INC A	
	LD HL,L9B4C	; Explosion sprite address
	LD BC,$0020	
	BIT 2,A	
	JR Z,LE45A	
	ADD HL,BC	
LE45A	LD (IX+$09),L	
	LD (IX+$0B),L	
	LD (IX+$0A),H	
	LD (IX+$0C),H	
	LD HL,LE61C	
	CALL LE5EC	; Play melody
	LD HL,$0000	
	LD (LDEFF),HL	
	CALL LDEF1	
	RET	

; I: IX = Object address = $E33B
LE476	LD IY,LB07D	
LE47A	LD A,(IY+$01)	
	CP $FF	
	JP Z,LE915	
	LD D,(IY+$03)	
	LD E,(IY+$02)	
	LD B,A	
	LD C,(IY+$00)	
	LD A,(BC)	
	BIT 5,A	
	JR Z,LE4B2	
	LD A,D	
	CP H	
	JR NZ,LE4AB	
	LD A,E	
	CP L	
	JR NZ,LE4AB	
	LD HL,(LDE5B)	
	LD BC,$00C8	
	ADD HL,BC	
	JR NC,LE4A5	
	LD HL,$FFFF	
LE4A5	CALL LDE5D	
	JP LE418	
LE4AB	LD BC,$0004	
	ADD IY,BC	
	JR LE47A	
LE4B2	BIT 6,A	
	JP Z,LE553	
	LD A,H	
	CP D	
	JR NZ,LE4AB	
	LD A,L	
	CP E	
	JR Z,LE4C3	
	DEC A	
	CP E	
	JR NZ,LE4AB	
LE4C3	BIT 6,(IX+$10)	
	JP NZ,LE418	
	BIT 4,(IX+$10)	
	JP NZ,LE418	
	LD A,(BC)	
	BIT 3,A	
	JP NZ,LE418	
	PUSH BC	
	INC BC	
	INC BC	
	INC BC	
	LD A,(BC)	
	DEC A	
	ADD A,A	
	LD HL,L5B2B	; get value 75 / 50 / 150 / 100, depending on Game level
	LD C,A	
	LD B,$00	
	ADD HL,BC	
	PUSH DE	
	CALL LE5D2	
	LD C,(HL)	
	INC HL	
	LD B,(HL)	
	DEC HL	
	EX DE,HL	
	PUSH HL	
	PUSH BC	
	OR A	
	SBC HL,BC	
	POP BC	
	POP HL	
	JR Z,LE4FB	
	JR C,LE533	
	JR LE509	
LE4FB	LD BC,(L5B33)	
	LD (LDEFF),BC	
	SET 6,(IX+$10)	
	JR LE510	
LE509	LD HL,(LDEFF)	
	ADD HL,BC	
	LD (LDEFF),HL	
LE510	POP DE	
	EX DE,HL	
	CALL LA14C	; Get screen attribute address
	LD (HL),$06	
	INC HL	
	LD (HL),$06	
	POP BC	
	LD A,(BC)	
	SET 3,A	
	LD (BC),A	
	CALL LDEF1	
	CALL LE615	; Make sound
	LD HL,L5B0F	
	INC (HL)	
	LD HL,(L5B48)	
	INC HL	
	LD (L5B48),HL	
	JP $E418	
LE533	PUSH BC	
	EX (SP),HL	
	POP BC	
	OR A	
	SBC HL,BC	
	LD A,L	
	LD (DE),A	
	INC DE	
	LD A,H	
	LD (DE),A	
	LD HL,(L5B33)	
	LD (LDEFF),HL	
	SET 6,(IX+$10)	
	POP DE	
	POP BC	
	CALL LDEF1	
	CALL LE615	; Make sound
	JP LE418	
LE553	BIT 1,A	
	JR Z,LE5A9	
	LD A,H	
	CP D	
	JP NZ,LE4AB	
	LD A,L	
	CP E	
	JP NZ,LE4AB	
	BIT 6,(IX+$10)	
	JP NZ,LE418	
	BIT 4,(IX+$10)	
	JP NZ,LE418	
	LD A,(BC)	
	BIT 3,A	
	JP NZ,LE418	
	LD HL,(L5B27)	; get value 2 / 4 / 6 / 8, depending on Game level
LE578	LD A,(BC)	
	BIT 4,A	
	JP NZ,LE418	
	PUSH BC	
	CALL LE5D2	
	POP BC	
	EX DE,HL	
	OR A	
	SBC HL,DE	
	JP C,LE418	
	LD HL,(LDEFF)	
	ADD HL,DE	
	LD (LDEFF),HL	
	LD A,(BC)	
	SET 4,A	
	LD (BC),A	
	CALL LDEF1	
	CALL LE615	; Make sound
	LD HL,L5B00	
	DEC (HL)	
	LD HL,(L5B46)	
	INC HL	
	LD (L5B46),HL	
	JP LE418	
LE5A9	LD A,H	
	CP D	
	JR Z,LE5B2	
	INC A	
	CP D	
	JP NZ,LE4AB	
LE5B2	LD A,L	
	CP E	
	JR Z,LE5BB	
	DEC A	
	CP E	
	JP NZ,LE4AB	
LE5BB	LD A,(BC)	
	BIT 3,A	
	JP NZ,LE3C0	
	LD A,L	
	CP E	
	JP NZ,LE418	
	BIT 6,(IX+$10)	
	JP NZ,LE418	
	LD HL,(L5B29)	; get value 5 / 10 / 15 / 20, depending on Game level
	JR LE578	

LE5D2	LD BC,(LDEFF)	
	LD DE,(L5B33)	
	EX DE,HL	
	OR A	
	SBC HL,BC	
	EX DE,HL	
	RET

LE5E0	DEFB $10,$27,$E8,$03,$64,$00,$0A,$00	
	DEFB $01,$00,$FF,$FF

; Play melody
; I: HL = Melody address
LE5EC	LD E,(HL)	
	LD A,E	
	INC A	
	RET Z	
	LD D,$00	
	INC HL	
	LD C,(HL)	
	INC HL	
	LD B,(HL)	
	INC HL	
	PUSH BC	
	EX (SP),HL	
	PUSH IX	
	CALL $03B5	; ROM Beeper subroutine
	DI	
	POP IX	
	POP HL	
	JR LE5EC	; continue

; Melodies
LE604	DEFB $02,$00,$20,$03,$00,$30,$FF	
LE60B	DEFB $06,$00,$01,$06,$00,$03,$06,$80	
	DEFB $00,$FF

; Play melody LE60B
LE615	LD HL,LE60B	
	CALL LE5EC	; Play melody
	RET

; Melodies
LE61C	DEFB $01,$00,$20,$01,$00,$18,$02,$00	
	DEFB $10,$03,$00,$08,$FF	
LE629	DEFB $02,$00,$0A,$03,$15,$0E,$FF	
LE630	DEFB $02,$FF
LE632	DEFB $00,$FF	
LE634	DEFB $04,$80,$00,$FF	
LE638	DEFB $14,$00,$02,$1E,$00,$01,$32,$C8	
	DEFB $00,$14,$40,$02,$FF

; Play melody LE629
LE645	PUSH HL	
	PUSH DE	
	PUSH AF	
	LD HL,LE629	
	CALL LE5EC	; Play melody
	POP AF	
	POP DE	
	POP HL	
	RET

LE652	DEFB $00,$0C,$00,$1E,$00,$0C,$00,$1E	
	DEFB $00,$1E,$00,$1E,$FF,$FF,$FF,$FF	
	DEFB $01,$8C,$03,$DE,$01,$8C,$03,$DE	
	DEFB $03,$DE,$03,$DE,$FF,$FF,$FF,$FF	
	DEFB $31,$8C,$7B,$DE,$31,$8C,$7B,$DE	
	DEFB $7B,$DE,$7B,$DE,$FF,$FF,$FF,$FF

LE682	OR A	
	JR NZ,LE68A	
	LD HL,LA41B	
	JR LE697	
LE68A	LD HL,LE652	
	LD DE,$0010	
	DEC A	
	JR Z,LE697	
	LD B,A	
LE694	ADD HL,DE	
	DJNZ LE694	
LE697	LD B,$06	
	LD DE,$8CD6	; ???
LE69C	PUSH BC	
	LDI	
	LDI	
	LD BC,$0005	
	EX DE,HL	
	ADD HL,BC	
	EX DE,HL	
	POP BC	
	DJNZ LE69C	
	RET	

LE6AB	LD IX,LE33B	; Diver object record address
	LD HL,$FFFF	
	LD (LDE5B),HL	
	LD A,(L5B37)	; get Number of lives
	DEC A	
	CALL LE682	
	LD A,(LC4F0)	
	LD C,A	
	SRL A	
	SRL A	
	SRL A	
	DEC A	
	AND $1F	
	LD L,A	
	LD H,$00	
	LD (L5B03),HL	; set Screen position on mini-map
	LD (IX+$1E),$00	
	LD (IX+$11),$FE	
	LD (IX+$12),$00	
	LD (IX+$0D),$00	
	LD (IX+$01),$06	
	LD (IX+$14),$0C	; set Y value = 12
	SET 3,(IX+$10)	
	LD A,(LC4F2)	
	LD (IX+$27),A	
	LD (IX+$04),$00	; clear DX value
	LD (IX+$05),$00	; clear DY value
	LD (IX+$28),$00	
	LD (IX+$1A),$06	
	LD (IX+$10),$00	
	LD (IX+$0E),$14	; speed factor = min speed
	LD (IX+$12),$00	
	LD (IX+$11),$FF	
	LD A,L	
	ADD A,A	
	ADD A,A	
	ADD A,A	
	LD L,A	
	LD A,C	
	SUB L	
	ADD A,$03	
	LD (IX+$00),A	; set Column value
	LD C,A	
	LD A,(LC4F2)	
	ADD A,$03	
	BIT 3,A	
	JR Z,LE72C	
	INC (IX+$00)	; one Column right
	AND $07	
LE72C	ADD A,A	
	LD (IX+$13),A	; set X value
	LD A,(L5B37)	; get Number of lives
	CP $03	
	JR Z,LE759	
	LD B,$0A	
	CP $02	
	JR Z,LE73F	
	LD B,$14	
LE73F	LD A,(IX+$13)	; get X value
	ADD A,B	
LE743	SUB $10	
	JR C,LE74C	
	INC (IX+$00)	; one Column right
	JR LE743	
LE74C	ADD A,$10	
	LD (IX+$13),A	; set X value
	LD (IX+$20),$01	
	LD (IX+$0F),$02	
LE759	LD HL,L9BAC	; Sprite diver sitting on the boat
	LD (IX+$24),L	; set sprite address
	LD (IX+$25),H
	SET 0,(IX+$10)	; set "moving" bit
	RET	

LE767	BIT 0,(IX+$26)	
	JR NZ,LE7E0	
	LD (IX+$20),$03	
	LD (IX+$0F),$03	
	LD (IX+$04),$00	; clear DX value
	LD (IX+$05),$00	; clear DY value
	LD A,(LC4F2)	
	CP (IX+$27)	
	JR Z,LE78C	
	LD (IX+$27),A	
	LD (IX+$04),$FE	; set DX value = -2
LE78C	LD BC,(L5B3E)	; get port for Accelerate key
	LD A,(L5B40)	; get bit mask for Accelerate key
	LD L,A	
	IN A,(C)	; Read from port for the key
	AND L	
	RET NZ		; Return if not pressed
	LD HL,LE634	
	CALL LE5EC	; Play melody
	BIT 2,(IX+$26)	
	JR NZ,LE7C2	
	LD (IX+$0E),$0C	
	LD HL,L9BAC	; Sprite diver sitting on the boat
	LD (IX+$24),L	; set sprite address
	LD (IX+$25),H
	SET 0,(IX+$26)	
	LD (IX+$04),$FF	; set DX value = -1
	LD (IX+$05),$02	; set DY value = +2
	SET 1,(IX+$26)	
	RET	
LE7C2	LD HL,L934C	; Sprite diver directing up-right
	LD (IX+$0E),$14	; speed factor = min speed
	LD (IX+$24),L	; set sprite address
	LD (IX+$25),H
	SET 0,(IX+$26)	
	LD (IX+$04),$02	; set DX value = +2
	LD (IX+$05),$02	; set DY value = +2
	SET 1,(IX+$26)	
	RET	
LE7E0	BIT 2,(IX+$26)	
	JR NZ,LE837	
	BIT 1,(IX+$26)	
	JR Z,LE7F1	
	RES 1,(IX+$26)	
	RET	
LE7F1	SET 1,(IX+$26)	
	LD H,(IX+$25)	
	LD L,(IX+$24)	
	LD BC,$0020	
	ADD HL,BC	
	LD (IX+$24),L	; set sprite address
	LD (IX+$25),H
	LD DE,L9C4C	; address right after last diver sprite
	OR A	
	SBC HL,DE	
	RET NZ	
	LD (IX+$0E),$14	
	LD (IX+$04),$00	; clear DX value
	LD (IX+$05),$00	; clear DY value
	LD (IX+$06),$04	
	SET 7,(IX+$10)	
	RES 0,(IX+$10)	; clear "moving" bit
	LD A,(L5B10)	; Game level 1..4
	LD C,A	
	LD A,$05	
	SUB C	
	LD (IX+$1E),A	
	LD (IX+$28),$07	
	RES 3,(IX+$10)	
	RET	
LE837	BIT 3,(IX+$26)	
	JR NZ,LE88A	
	BIT 1,(IX+$26)	
	JR Z,LE848	
	RES 1,(IX+$26)	
	RET	
LE848	SET 1,(IX+$26)	
	LD H,(IX+$25)	
	LD L,(IX+$24)	
	LD BC,$0080	
	ADD HL,BC	
	LD (IX+$24),L	; set sprite address
	LD (IX+$25),H
	LD DE,L954C	
	OR A	
	SBC HL,DE	
	RET NZ	
	LD (IX+$04),$00	; clear DX value
	LD (IX+$05),$00	; clear DY value
	LD (IX+$06),$04	
	SET 7,(IX+$10)	
	RES 0,(IX+$10)	; clear "moving" bit
	LD A,(L5B10)	; Game level 1..4
	LD C,A	
	LD A,$05	
	SUB C	
	LD (IX+$1E),A	
	LD (IX+$28),$07	
	RES 3,(IX+$10)	
	RET	
LE88A	LD (IX+$1E),$00	
	LD HL,LE632	
	INC (HL)	
	DEC HL	
	DEC HL	
	CALL LE5EC	; Play melody
	LD A,(IX+$1A)	
	CP $06	
	JR NZ,$E8FA	
	LD A,(IX+$03)	
	CP $07	
	JR NZ,LE8FA	
	LD HL,(LDEFF)	
	LD DE,(L5B44)	
	ADD HL,DE	
	LD (L5B44),HL	
	LD HL,$0000	
	LD (LDEFF),HL	
	CALL LDEF1	
	CALL LDEE5	
	LD HL,$FFFF	
	CALL LDE5D	
	RES 6,(IX+$10)	
	RES 3,(IX+$26)	
	RES 0,(IX+$26)	
	LD A,(L5B00)	
	CP $08	
	JR NZ,$E8FA	
	LD A,(L5B0F)	
	CP $03	
	JR NZ,LE8FA	
	LD HL,(L5B31)	
	LD DE,(L5B44)	
	ADD HL,DE	
	LD (L5B44),HL	
	CALL LDEE5	
	SET 4,(IX+$26)	
	LD HL,LE638	
	CALL LE5EC	; Play melody
	LD HL,LE638	
	CALL LE5EC	; Play melody
LE8FA	LD (IX+$04),$00	
	LD (IX+$0F),$03	
	LD (IX+$20),$03	
	LD A,(LC4F2)	
	CP (IX+$27)	
	RET Z	
	LD (IX+$27),A	
	LD (IX+$04),$FE	
	RET	

LE915	LD DE,(LC4F0)	
	BIT 5,(IX+$10)	
	JP NZ,LE418	
	LD BC,(L5B0B)	
	LD A,E	
	SUB C	
	ADD A,$07	
	LD C,A	
	CP L	
	JP NZ,LE3CD	
	LD A,D	
	SUB B	
	INC A	
	CP H	
	JR Z,LE93C	
	INC A	
	CP H	
	JR Z,LE93C	
	INC A	
	CP H	
	JP NZ,LE3CD	
LE93C	SET 1,(IX+$0D)	
	RES 0,(IX+$0D)	
	PUSH IY	
	PUSH BC	
	LD (IX+$04),$00	; clear DX value
	LD HL,(L5B03)	; get Screen position on mini-map
	PUSH HL	
	CALL LDFD5	
	POP HL	
	RES 7,(IX+$10)	
	SET 0,(IX+$26)	
	SET 2,(IX+$26)	
	SET 3,(IX+$26)	
	LD A,(LC4F2)	
	LD (IX+$27),A	
	LD (IX+$04),$00	; clear DX value
	LD (IX+$05),$FF	; set DY = -1
	POP BC	
	LD A,(L5B03)	; get Screen position (column) on mini-map
	CP L	
	JR Z,LE97C	
	LD A,$08	
	ADD A,C	
	LD C,A	
LE97C	LD (IX+$00),C	; set Column value
	LD A,(IX+$27)	
	ADD A,A	
	LD (IX+$13),A	
	LD (IX+$28),$00	
	LD HL,L9B8C	; Sprite diver climbing on the boat
	LD (IX+$24),L	; set sprite address
	LD (IX+$25),H
	SET 0,(IX+$0D)	
	RES 1,(IX+$0D)	
	CALL LDFD5	
	POP IY	
	LD (IX+$0F),$03	
	LD (IX+$20),$03	
	LD HL,LE632	
	LD (HL),$00	
	JP LE418	

; I: IX = Object address = $E33B
LE9B0	SET 5,(IX+$26)	
	PUSH HL	
	PUSH DE	
	LD HL,(LDE59)	
	LD (HL),$0D	
	LD DE,$0040	
	ADD HL,DE	
	LD (LDE59),HL	
	LD (HL),$4F	
	LD HL,L5B37	; get Number of lives
	LD A,(HL)	
	INC (HL)	
	PUSH BC	
	CALL LE682	
	POP BC	
	POP DE	
	POP HL	
	RET

LE9D1	DEFB $00,$00,$00,$00,$00,$00,$00	
LE9D8	DEFB $00,$00,$44,$55,$52,$45,$4C,$4C,$00,$00,$00,$00,$0A,$00,$00,$00	
	DEFB $00,$00,$44,$55,$52,$45,$4C,$4C,$00,$00,$00,$00,$0A,$00,$00,$00	
	DEFB $00,$00,$44,$55,$52,$45,$4C,$4C,$00,$00,$00,$00,$0A,$00,$00,$00	
	DEFB $00,$00,$44,$55,$52,$45,$4C,$4C,$00,$00,$00,$00,$0A,$00,$00,$00	
	DEFB $00,$00,$44,$55,$52,$45,$4C,$4C,$00,$00,$00,$00,$0A,$00,$00,$00	
	DEFB $00,$00,$44,$55,$52,$45,$4C,$4C,$00,$00,$00,$00,$0A,$00,$00,$00	
	DEFB $00,$00,$44,$55,$52,$45,$4C,$4C,$00,$00,$00,$00,$0A,$00,$00,$00	
	DEFB $00,$00,$44,$55,$52,$45,$4C,$4C,$00,$00,$00,$00,$0A,$00,$00,$00	
	DEFB $00,$00,$44,$55,$52,$45,$4C,$4C,$00,$00,$00,$00,$0A,$00,$00,$00	
	DEFB $00,$00,$44,$55,$52,$45,$4C,$4C,$00,$00,$00,$00
LEA74	DEFB $0A,$00,$00,$00,$00,$00

LEA7A	LD (LE9D8),IX	
	DI	
	LD HL,(LE9D8+12)	
	LD DE,(L5B44)	
	LD A,E	
	SUB L	
	LD A,D	
	SBC A,H	
	JR C,LEADE	
	EX DE,HL	
	LD IX,LE9D8+2	
	LD DE,$0010	
	LD B,$0A	
LEA96	LD A,L	
	SUB (IX+$0A)	
	LD A,H	
	SBC A,(IX+$0B)	
	JR C,LEAA4	
	ADD IX,DE	
	DJNZ LEA96	
LEAA4	PUSH IX	
	POP HL	
	OR A	
	SBC HL,DE	
	PUSH HL	
	LD DE,LE9D8+2	
	LD A,$0A	
	SUB B	
	ADD A,A	
	ADD A,A	
	ADD A,A	
	ADD A,A	
	JR Z,LEABF	
	LD C,A	
	LD B,$00	
	LD HL,LE9D8+18	
	LDIR	
LEABF	POP HL	
	LD B,$06	
LEAC2	LD (HL),$80	
	INC HL	
	DJNZ $EAC2	
	LD BC,(L5B48)	
	LD (HL),C	
	INC HL	
	LD (HL),B	
	INC HL	
	LD BC,(L5B46)	
	LD (HL),C	
	INC HL	
	LD (HL),B	
	INC HL	
	LD BC,(L5B44)	
	LD (HL),C	
	INC HL	
	LD (HL),B	
;
LEADE	LD IX,LE33B	; Diver object record address
	LD (IX+$1F),$07	
	LD IX,(LE9D8)	
	LD A,$0F	
	LD ($5C8D),A	
	LD A,$02	
	CALL $1601	; ROM call CHAN-OPEN
	LD A,$01	
	CALL $229B	; ROM call inside BORDER subroutine
	LD A,$02	
	CALL $1601	; ROM call CHAN-OPEN
	CALL $0D6B	; ROM CLS subroutine
	LD A,$02	
	CALL $1601	; ROM call CHAN-OPEN
	LD DE,LEBEE	; Table of records text
	LD BC,$005D	
	CALL $203C	; ROM call PR-STRING
	LD B,$0A	
	LD C,$05	
	LD HL,LE9D8+146	
LEB16	PUSH BC	
	BIT 7,(HL)	
	JR Z,LEB20	
	CALL LEB82	
	JR LEB2B	
LEB20	LD B,$06	
LEB22	LD A,(HL)	
	INC HL	
	PUSH HL	
	PUSH BC	
	RST $10	
	POP BC	
	POP HL	
	DJNZ LEB22	
LEB2B	LD B,$0B	
	CALL LEBDB	
	LD B,$13	
	CALL LEBDB	
	PUSH HL	
	LD BC,$0003	
	LD DE,LEC4B	
	CALL $203C	; ROM call PR-STRING
	DI	
	LD IX,LE33B	; Diver object record address
	POP HL	
	LD E,(HL)	
	INC HL	
	LD D,(HL)	
	INC HL	
	PUSH HL	
	PUSH DE	
	LD L,$19	
	LD H,(IX+$1F)	
	CALL LA164	; Convert char coords HL to ZX screen address
	POP DE	
	EX DE,HL	
	LD B,$00	
	PUSH IY	
	CALL LDE85	
	POP IY	
	POP HL	
	LD DE,$FFE4	; ???
	INC (IX+$1F)	
	LD IX,(LE9D8)	
	EI	
	ADD HL,DE	
	POP BC	
	DJNZ LEB16	
	LD DE,LEC2A	
	LD BC,$001F	
	CALL $203C	; ROM call PR-STRING
	LD DE,LEC59	; "ENTER SKILL (1TO4),K,L OR S."
	LD BC,$0023	
	CALL $203C	; ROM call PR-STRING
	EI	
	RET	

LEB82	LD B,$06	
LEB84	PUSH BC	
	PUSH HL	
	LD BC,$0006	
	LD DE,LEC53	
	CALL $203C	; ROM call PR-STRING
	EI	
LEB90	LD A,(L5C05)	
	OR A	
	JR NZ,LEB90	
LEB96	LD A,(L5C05)	
	OR A	
	JR Z,$EB96	
	LD A,(L5C08)	
	CP $0C	
	JR Z,LEBB9	
	CP $0D	
	JR Z,LEBD3	
	BIT 7,A	
	JR NZ,LEB90	
	CP $20	
	JR C,LEB90	
	PUSH AF	
	RST $10	
	POP AF	
	POP HL	
	LD (HL),A	
	INC HL	
	POP BC	
	DJNZ LEB84	
	RET	
LEBB9	POP HL	
	POP BC	
	LD A,B	
	CP $06	
	JR Z,LEB84	
	INC B	
	DEC HL	
	LD (HL),$80	
	PUSH HL	
	PUSH BC	
	LD DE,LEC4E	
	LD BC,$0005	
	CALL $203C	; ROM call PR-STRING
	POP BC	
	POP HL	
	JR LEB84	
LEBD3	POP HL	
	POP BC	
LEBD5	LD (HL),$20	
	INC HL	
	DJNZ LEBD5	
	RET	

LEBDB	PUSH HL	
	PUSH BC	
	LD A,$17	
	RST $10	
	POP AF	
	RST $10	
	RST $10	
	POP HL	
	LD C,(HL)	
	INC HL	
	LD B,(HL)	
	INC HL	
	PUSH HL	
	CALL $1A1B	; ROM OUT-NUM-1 subroutine
	POP HL	
	RET	

LEBEE	DEFM $10,$07,$11,$01,$16,$01,$09	
	DEFM "* SCUBA DIVE *"	
	DEFM $10,$00,$11,$06,$16,$05,$00	
	DEFM "  NAME   CHESTS  PEARLS  SCORE  "	
LEC2A	DEFM "                               "	
	DEFM "  "	
LEC4B	DEFM $17,$01,$00	
LEC4E	DEFM " ",$08,$08," ",$08	
LEC53	DEFM $12,$01," ",$12,$00,$08	
LEC59	DEFM $16,$15,$01,$11,$01,$10,$06	
	DEFM "ENTER SKILL (1TO4),K,L OR S."

; Redefine keys ??
LEC7C	LD A,$02	
	CALL $1601	; ROM call CHAN-OPEN
	LD A,$04	
	CALL $229B	; ROM call inside BORDER subroutine
	LD A,$02	
	CALL $1601	; ROM call CHAN-OPEN
	LD A,$20	
	LD (L5C8D),A	
	CALL $0D6B	; ROM CLS subroutine
	LD HL,$ED23	
	LD (L5C7B),HL	
	LD A,$02	
	CALL $1601	; ROM call CHAN-OPEN
LEC9E	CALL $02BF	; ROM call KEYBOARD
	LD A,(L5C05)	
	OR A	
	JR NZ,LEC9E	
	LD DE,LED43	
	LD BC,$005F	
	CALL $203C	; ROM call PR-STRING
	CALL LECEB	; Sound
	LD (L5B38),BC	; set port for Clockwise key
	LD (L5B3A),A	; Save key for Clockwise
	LD BC,$0012	
	CALL $203C	; ROM call PR-STRING
	CALL LECEB	; Sound
	LD (L5B3B),BC	
	LD (L5B3D),A	; Save key for Anticlockwise
	LD BC,$0012	
	CALL $203C	; ROM call PR-STRING
	CALL LECEB	; Sound
	LD (L5B3E),BC	; set port for Accelerate key
	LD (L5B40),A	; Save key for Accelerate
	LD BC,$000F	
	CALL $203C	; ROM call PR-STRING
	CALL LECEB	; Sound
	LD (L5B41),BC	; set port for Decelerate
	LD (L5B43),A	; Save key for Decelerate
	RET	

; Sound??
LECEB	LD C,$FE	
	LD B,C	
LECEE	IN A,(C)	
	AND $1F	
	XOR $1F	
	JR NZ,LECFA	
	RLC B	
	JR LECEE	
LECFA	PUSH AF	
	PUSH BC	
	PUSH IX	
	PUSH DE	
	LD HL,LEDD5	
LED02	LD E,(HL)	
	LD A,E	
	INC A	
	JR Z,LED16	
	LD D,$00	
	INC HL	
	LD C,(HL)	
	INC HL	
	LD B,(HL)	
	INC HL	
	PUSH BC	
	EX (SP),HL	
	CALL $03B5	; ROM BEEPER subroutine
	POP HL	
	JR LED02	
LED16	POP DE	
	POP IX	
	POP BC	
	POP AF	
	LD L,A	
LED1C	IN A,(C)	
	AND L	
	JR Z,LED1C	
	LD A,L	
	RET	

LED23	DEFB $18,$24,$42,$87,$87,$42,$20,$18	
	DEFB $18,$20,$42,$87,$87,$42,$24,$18	
	DEFB $08,$0C,$0E,$FF,$FF,$0E,$0C,$08	
	DEFB $00,$00,$10,$30,$7E,$30,$10,$00	
LED43	DEFB $10,$00,$11,$06,$16,$03,$05	
	DEFM "                      "	
	DEFB $16,$04,$05	
	DEFM " PRESS ANY KEY FOR :- "	
	DEFB $16,$05,$05	
LED7C	DEFM "                      "	
	DEFB $16,$08,$0B,$11,$04	
	DEFM "Clockwise "	
	DEFB $90,$16,$08,$07	
	DEFM "Anticlockwise "	
	DEFB $91,$16,$08,$07	
	DEFM "  Accelerate "	
	DEFB $92,$20,$16,$08,$09	
	DEFM "Decelerate "	
	DEFB $93	

; Melody
LEDD5	DEFB $12,$80,$01,$1E,$00,$01,$0C,$40	
	DEFB $01,$FF
LEDDF	DEFB $3E,$08,$32,$6A,$5C,$3E,$02,$CD	
	DEFB $01,$16,$3E,$0F,$32,$8D,$5C,$2A	
	DEFB $78,$5C,$22,$05,$5B,$21,$4D,$5B	
	DEFB $CB,$46,$C2,$A7,$EE,$CB,$C6,$11	
	DEFB $70,$EF,$01,$0F,$00,$CD,$3C,$20	
	DEFB $11,$60,$EC,$01,$1C,$00,$CD,$3C	
	DEFB $20,$06,$0A,$C5,$21,$D5,$ED,$CD	
	DEFB $EC,$E5,$C1,$10,$F6

LEE1C	LD HL,$0000	
	LD (L5C78),HL	
LEE22	CALL LEEAD	
	LD A,(L5C05)	
	EI	
	OR A	
	JR NZ,LEE22	
LEE2C	CALL LEEAD	
	LD A,(L5C05)	
	EI	
	OR A	
	JR Z,LEE2C	
	LD A,(L5C08)	
	CP $4B		; 'K' ?
	JR NZ,LEE45	
	CALL LEC7C	; Redefine keys
	CALL LEADE	
	JR LEE1C	
LEE45	CP $4C		; 'L' ?
	JR NZ,LEE75	
	CALL $0D6B	; ROM CLS subroutine
	LD A,$02	
	CALL $1601	; ROM call CHAN-OPEN
	LD DE,LEF93	; "LOAD ? (Y/N)"
	LD BC,$0013	
	CALL $203C	; ROM call PR-STRING
LEE5A	LD A,(L5C05)	
	OR A	
	JR NZ,LEE5A	
LEE60	LD A,(L5C05)	
	OR A	
	JR NZ,LEE60	
	LD A,(L5C08)	
	CP $4E		; 'N' ?
	JR Z,LEEA7	
	CP $59		; 'Y' ?
	JR NZ,LEE5A	
	LD BC,$0000	
	RET	
LEE75	CP $53		; 'S' ?
	JR NZ,LEE7D	
	LD BC,$0001	
	RET	
LEE7D	SUB $31		; -'1'
	JR C,LEE1C	
	CP $04	
	JR NC,LEE1C	
	INC A		; 1..4
	LD (L5B10),A	; Save game level 1..4
	LD HL,LEDD5	
	CALL LE5EC	; Play melody
	LD HL,$0000	
	LD (L5B46),HL	
	LD (L5B48),HL	
	LD HL,(LEA74)	
	LD (L5B4B),HL	
	CALL LD990	
	CALL LEA7A	
	JP LEE1C	
LEEA7	CALL LEADE	
	JP LEE1C	

LEEAD	LD A,(L5C79)	
	CP $0A	
	EI	
	RET NZ	
	LD A,$04	
	LD (L5B10),A	; Game level 1..4
	LD A,$0D	
	CALL LDA39	; Clear screen with attribute A
	LD A,$01	
	OUT ($FE),A	
	PUSH IX	
	PUSH IY	
	DI	
	CALL L9DAA	
	CALL LDBC2	; Initialize variables depending of Game level
	CALL LBEDB	
	LD HL,$0518	
	LD (L5B03),HL	; set Screen position on mini-map
	CALL L9C56	; Draw game screen
	POP IY	
	POP IX	
	LD DE,LEF7F	; "PRESS ANY KEY"
	LD BC,$0014	
	CALL $203C	; ROM call PR-STRING
	PUSH IX	
	PUSH IY	
	LD BC,$001A	
	LD DE,LDA63	; Procedure Print char and shift down
	LD HL,LEF3A	
	CALL LDA98	; Print string
	INC HL	
	LD BC,$001B	
	CALL LDA9C	; Print string
	INC HL	
	LD BC,$0D1D	
	CALL LDA9C	; Print string
	INC HL	
	LD BC,$0D1E	
	CALL LDA9C	; Print string
	CALL LBDBA	
	POP IY	
	POP IX	
LEF12	PUSH IX	
	PUSH IY	
	CALL LB213	
	CALL LBDEA	
	POP IY	
	POP IX	
	EI	
	PUSH HL	
	POP HL	
	PUSH HL	
	POP HL	
	CALL $02BF	; ROM call KEYBOARD
	LD A,(L5C05)	
	OR A	
	DI	
	JR Z,LEF12	
	EI	
	CALL LEADE	
	LD HL,$0000	
	LD (L5C78),HL	
	RET	

LEF3A	DEFB $89,$8A,$00,$8B,$8C,$00,$8D,$8E	
	DEFB $00,$8F,$90,$00,$91,$92,$FF	
	DEFB $93,$94,$00,$95,$96,$00,$97,$98	
	DEFB $00,$99,$9A,$00,$9B,$9C,$FF	
	DEFB $9D,$9E,$00,$9F,$A0,$00,$A1,$A2	
	DEFB $00,$A3,$A4,$FF,$A5,$A6,$00,$A7	
	DEFB $A8,$00,$A9,$AA,$00,$AB,$AC,$FF	
	DEFB $12,$00,$13,$00,$14,$00,$15,$00	
	DEFB $11,$00,$10,$07,$16,$15,$01
LEF7F	DEFM $16,$0B,$05,$11,$00,$10,$06	
	DEFM "PRESS ANY KEY"
LEF93	DEFB $11,$01,$10,$07,$16,$0B,$0A
	DEFM "LOAD ? (Y/N)"	

LEFA6	DEFS $5A	

LF000	DEFS $0100	
