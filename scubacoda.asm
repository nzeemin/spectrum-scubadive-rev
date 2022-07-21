;----------------------------------------------------------------------------

	ORG $4000
	INCBIN "scubascrn.dat"

;----------------------------------------------------------------------------

	ORG $5B00

L5B00	DEFB $00	; ???
L5B01	DEFW $0000	; ???
L5B03	DEFW $0000	; Screen position on mini-map (H = row, L = column)
L5B05	DEFW $0167	; Current number in pseudo-random sequence, see routine 9D84
L5B07	DEFW $0000	; ???
L5B09	DEFB $00	; ???
L5B0A	DEFB $00	; ???
L5B0B	DEFW $0000	; ???
L5B0D	DEFB $08	; Delay value: 7 / 5 / 3 / 1, depending on Game level 1..4
L5B0E	DEFB $08	; Delay value for Octopus: 10 / 8 / 6 / 4, depending on Game level 1..4
L5B0F	DEFB $00	; ???
L5B10	DEFB $04	; Game level selected: 1..4
L5B11	DEFW $0000	; ???
L5B13	DEFB $19	; Value 13 / 17 / 21 / 25, depending on Game level 1..4
L5B14	DEFB $1E	; Value 13 / 17 / 21 / 25, depending on Game level 1..4
L5B15	DEFB $38	; Value 27 / 35 / 43 / 51, depending on Game level 1..4
L5B16	DEFB $1E	; Value 18 / 22 / 26 / 31, depending on Game level 1..4
L5B17	DEFB $32	; Value 26 / 34 / 42 / 50, depending on Game level 1..4
L5B18	DEFB $14	; Value 5 / 10 / 15 / 20, depending on Game level 1..4
L5B19	DEFB $46	; Value 12 / 27 / 42 / 57, depending on Game level 1..4
L5B1A	DEFB $02	; ???
L5B1B	DEFB $02	; ???
L5B1C	DEFB $02	; ???
L5B1D	DEFB $02	; ???
L5B1E	DEFB $05	; ???
L5B1F	DEFW $0000	; ???
L5B21	DEFW $0000	; ???
L5B23	DEFW $0000	; ???
L5B25	DEFW $0001	; Value 150 / 100 / 50 / 1, depending on Game level 1..4
; 14 bytes copied from DDF0 + ([Game level] - 1) * 16
L5B27	DEFW $0000
L5B29	DEFW $0000
L5B2B	DEFW $0000,$0000
L5B2F	DEFW $0000
L5B31	DEFW $0000
L5B33	DEFW $0000	; ???
;
L5B35	DEFW $DF45	; ???
L5B37	DEFB $03	; Number of lives
L5B38	DEFW $FEFE	; Port for Clockwise key
L5B3A	DEFB $04	; Bit mask for Clockwise key
L5B3B	DEFW $FEFE	; Port for Anticlockwise key
L5B3D	DEFB $02	; Bit mask for Anticlockwise key
L5B3E	DEFW $7FFE	; Port for Accelerate key
L5B40	DEFB $01	; Bit mask for Accelerate key
L5B41	DEFW $7FFE	; Port for Decelerate key
L5B43	DEFB $02	; Bit mask for Decelerate key
L5B44	DEFW $03E8	; Score value
L5B46	DEFW $0000	; ??? $0000 at game start
L5B48	DEFW $0000	; ??? $0000 at game start
L5B4A	DEFB $00	; Screen attribute, see routine DA39
L5B4B	DEFW $0000	; High score value

;----------------------------------------------------------------------------

L5B4D	DEFB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	
	DEFB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	
L5BED	DEFB $00

;----------------------------------------------------------------------------
