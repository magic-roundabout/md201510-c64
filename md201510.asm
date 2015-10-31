;
; MD201510
;

; Code by T.M.R/Cosine
; Graphics by T.M.R/Cosine
; Music by Odie/Cosine


; A small entry for the Unintended OpCode Coding Challenge at the
; C64 Scene Database. The comments get heavier around the more
; "interesting" parts of the code. There are uses of pseudo opcodes
; throughout including DCP for delays and DOP/TOP (ACME's opcodes
; for three and four cycle NOP respectively) but the main routine
; mostly (ab)uses SAX and the logo swing is leaning on LAX


; This source code is formatted for the ACME cross assembler from
; http://sourceforge.net/projects/acme-crossass/
; Compression is handled with PuCrunch which can be downloaded at
; http://csdb.dk/release/?id=6089

; build.bat will call both to create an assembled file and then the
; crunched release version.


; Select an output filename and specify 6510 CPU for pseudo opcodes
		!to "md201510.prg",cbm
		!cpu 6510

; Yank in binary data
		* = $4000
		!binary "data\bloody.chr"


		* = $4800
		!binary "data\witch_1.raw"

		* = $5000
		!binary "data\witch_2.raw"

		* = $5800
		!binary "data\witch_3.raw"

		* = $6000
		!binary "data\witch_4.raw"

		* = $6800
		!binary "data\witch_5.raw"

		* = $7000
		!binary "data\witch_6.raw"


		* = $7800
		!binary "data\logo.chr"

		* = $7ec0
		!binary "data\bat.spr"


		* = $f46f
music		!binary "data\elvira.prg",,2

; Raster split positions
rstr1p		= $0a
rstr2p	= $e3

; Label assignments
ghostwork_1	= $40		; 8 bytes used
ghostwork_2	= $48		; 22 bytes used
ghostwork_3	= $6a		; 22 bytes used

raster_num	= $90
zp_timer	= $91

cos_at_1	= $92
cos_at_2	= $93
cos_at_3	= $94
cos_at_4	= $95

cos_at_5	= $96
cos_at_6	= $97

cos_at_7	= $98
witch_cnt	= $99
witch_d018	= $9a

d016_mirror_1	= $9b

scroll_pos	= $9c		; 2 bytes used
scroll_x	= $9d
d016_mirror_2	= $9e
scrl_col_count	= $9f
char_width	= $a0
bat_anim_cnt	= $a1
bat_priority	= $a2

screen_ram	= $4c00
ghostbyte	= $7fff


; Code entry point at $0812
		* = $0812

; Stop interrupts, disable the ROMS and set up NMI and IRQ interrupt pointers
code_start	sei

		ldx #$40
		lda #$00
nuke_zp		sta $00,x
		inx
		bne nuke_zp

		lda #$00
		sta $d020
		sta $d021

		lda #$c6
		sta $dd00

		lda #$35
		sta $01

		lda #<nmi_int
		sta $fffa
		lda #>nmi_int
		sta $fffb

		lda #<int
		sta $fffe
		lda #>int
		sta $ffff

; Set the VIC-II up for a raster IRQ interrupt
		lda #$7f
		sta $dc0d
		sta $dd0d

		top $dc0d
		top $dd0d

		lda #rstr1p
		sta $d012

		lda #$1b
		sta $d011
		lda #$01
		sta $d019
		sta $d01a

; Initialise some of the labels
		lda #$02
		sta raster_num

		lda #$3b
		sta cos_at_2
		lda #$cb
		sta cos_at_4


; Clear and set the colour RAM
		ldx #$00
		lda #$09
colour_clear	sta $d800,x
		sta $d900,x
		sta $da00,x
		sta $dae8,x
		inx
		bne colour_clear

		ldx #$00
colour_set	lda pic_col_data,x
		sta $d8f0,x
		sta $d918,x
		sta $d940,x
		sta $d968,x
		sta $d990,x
		sta $d9b8,x
		sta $d9e0,x
		sta $da08,x

		sta $da30,x
		sta $da58,x
		sta $da80,x
		sta $daa8,x
		sta $dad0,x
		sta $daf8,x
		sta $db20,x
		sta $db48,x

		sta $db70,x
		inx
		cpx #$28
		bne colour_set

; Reset the scrolling message
		jsr reset
		lda #$01
		sta char_width

; Set the witch's initial position
		lda witch_d018_tbl
		sta witch_d018

; Set up the music driver
		lda #$00
		jsr music+$00

		cli

; Infinite loop
		jmp *


; IRQ interrupt handler
int		pha
		txa
		pha
		tya
		pha

		lda $d019
		and #$01
		sta $d019
		bne ya
		jmp ea31

; An interrupt has triggered
ya		lda #$00
		dcp raster_num
		bne *+$05
		jmp rout2


; Raster split 1
rout1		lda #$00
		sta $d020
		lda #$3e
		sta $d018

; Synchronise the raster
		lda $d012
		cmp #rstr1p+$00
		beq *+$02
;		sta $d020

		ldx #$0a
		dex
		bne *-$01
		top $eaea
		lda $d012
		cmp #rstr1p+$01
		beq *+$02
;		sta $d020

		ldx #$0a
		dex
		bne *-$01
		top $eaea
		lda $d012
		cmp #rstr1p+$02
		beq *+$02
;		sta $d020

		ldx #$09
		dex
		bne *-$01
		dop $ea
		top $eaea
		lda $d012
		cmp #rstr1p+$03
		beq *+$02
;		sta $d020

		ldx #$0a
		dex
		bne *-$01
		top $eaea
		lda $d012
		cmp #rstr1p+$04
		beq *+$02
;		sta $d020

		ldx #$0a
		dex
		bne *-$01
		dop $ea
		lda $d012
		cmp #rstr1p+$05
		beq *+$02
;		sta $d020


; Delay before the upper border splitter
		lda #$05
		sta zp_timer
		lda #$00
		dcp zp_timer
		bne *-$02
		dop $ea
		nop


; Upper border ghostbyte splitter

; This is the first part of the main routine, doing four cycle wide ghostbyte
; splits by loading all three registers at the start of a scanline and then
; dumping the contents back to create patterns.

; It gains an advantage from undocumented commands by using SAX (which ANDs
; together A and X whilst writing to memory) to merge two "textures" and get
; a fourth split on each scanline without losing cycles to load a register.
!set line_count=$00
!set fx_count=$00
!do {
		lda ub_colours+line_count
		sta $d021

		ldx ghostwork_1+fx_count
		ldy ghostwork_2+line_count
		lda ghostwork_3+line_count
		sty ghostbyte
		stx ghostbyte
		sax ghostbyte
		sta ghostbyte
		sty ghostbyte
		sax ghostbyte
		sta ghostbyte
		sty ghostbyte
		sax ghostbyte
		stx ghostbyte

		nop
		top $eaea

		!set fx_count=fx_count+$01
		!if fx_count=$08 {
			!set fx_count=$00
		}

		!set line_count=line_count+$01
}until line_count=$22

; Set registers for the top of the logo
		lda #$09
		sta $d021

		lda d016_mirror_1
		sta $d016

; Update the first border effect
		lda cos_at_1
		clc
		adc #$03
		sta cos_at_1
		tax

		lda cos_at_2
		clc
		adc #$02
		sta cos_at_2
		tay

		lda blob_cosinus,y
		asl
		asl
		asl
		clc
		adc blob_cosinus,x
		and #$3f

; This copies eight bytes using SBX to subtract $08 from X and then
; AND by $3F - the graphics data is a block of 64 bytes with the eight
; pre-shifted values for each scanline grouped together.
		tax
		lda blob_data,x
		sta ghostwork_1+$00

!set line_count=$01
!do {
		txa
		sec
		sbc #$08
		and #$3f
		tax

		lda blob_data,x
		sta ghostwork_1+line_count

		!set line_count=line_count+$01
} until line_count=$08

; Update the second border effect
		lda cos_at_3
		clc
		adc #$01
		sta cos_at_3
		tax
		lda cos_at_4
		clc
		adc #$fe
		sta cos_at_4
		tay

		lda blob_cosinus,x
		clc
		adc blob_cosinus,y
		and #$07
		sta ghostwork_3

!set line_count=$01
!do {
		txa
		clc
		adc #$03
		tax
		tya
		clc
		adc #$06
		tay

		lda blob_cosinus,x
		clc
		adc blob_cosinus,y
		and #$07
		sta ghostwork_3+line_count

		!set line_count=line_count+$01
} until line_count=$22

!set line_count=$00
!do {
		ldx ghostwork_3+line_count
		lda wave_data,x
		sta ghostwork_3+line_count

		!set line_count=line_count+$01
} until line_count=$22

; Update the third border effect
		ldx cos_at_2
		ldy cos_at_4
		lda warp_cosinus,x
		clc
		adc warp_cosinus,y
		and #$07
		sta ghostwork_2

!set line_count=$01
!do {
		txa
		clc
		adc #$02
		tax
		tya
		sec
		sbc #$04
		tay
		lda warp_cosinus,x
		clc
		adc warp_cosinus,y
		and #$07
		sta ghostwork_2+line_count

		!set line_count=line_count+$01
} until line_count=$22

; Wait for the right place and switch fonts for the witching area
		lda #$61
		cmp $d012
		bcs *-$03

		ldx #$08
		dex
		bne *-$01

		lda #$06
		ldx #$04
		ldy witch_d018
		sty $d018
		sta $d021
		stx $d016

; Finish the effect updating
!set line_count=$00
!do {
		ldx ghostwork_2+line_count
		lda warp_data,x
		sta ghostwork_2+line_count

		!set line_count=line_count+$01
} until line_count=$22


; Play the music
		jsr music+$03


; Update the logo position
		ldy cos_at_7
		iny
		sty cos_at_7
		lax logo_cosinus,y
		and #$07
		eor #$07
		sta d016_mirror_1
		txa
		lsr
		lsr
		lsr
		sta logo_copy_1a+$01
		sta logo_copy_1e+$01
		clc
		adc #$40
		sta logo_copy_1b+$01
		sta logo_copy_1f+$01
		clc
		adc #$40
		sta logo_copy_1c+$01
		clc
		adc #$40
		sta logo_copy_1d+$01

; Draw the logo
; This uses LAX to read the same value to A and X, then pushes A directly
; to the screen RAM before loading a colour value for that character with Y
; and writing that to the colour. This is also self-modifying code for speed
; reasons, and I THINK this is the fastest it's possible to get without
; unrolling the loop...?
		ldy #$26
logo_copy_1a	lax logo_data_ln1,y
		sta screen_ram+$000,y
		lda logo_colour_dcd,x
		sta $d800,y

logo_copy_1b	lax logo_data_ln2,y
		sta screen_ram+$028,y
		lda logo_colour_dcd,x
		sta $d828,y

logo_copy_1c	lax logo_data_ln3,y
		sta screen_ram+$050,y
		lda logo_colour_dcd,x
		sta $d850,y

logo_copy_1d	lax logo_data_ln4,y
		sta screen_ram+$078,y
		lda logo_colour_dcd,x
		sta $d878,y

logo_copy_1e	lax logo_data_ln5,y
		sta screen_ram+$0a0,y
		lda logo_colour_dcd,x
		sta $d8a0,y

logo_copy_1f	lax logo_data_ln6,y
		sta screen_ram+$0c8,y
		lda logo_colour_dcd,x
		sta $d8c8,y

		dey
		bpl logo_copy_1a

		lda #$01
		sta raster_num
		lda #rstr2p
		sta $d012

; Exit IRQ interrupt
		jmp ea31


		* = ((*/$100)+1)*$100	; start at next page boundary
; Raster split 2
rout2		nop
		nop
		nop

; Synchronise the raster
		ldx #$04
		dex
		bne *-$01
		top $eaea
		lda $d012
		cmp #rstr2p+$01
		beq *+$02
;		sta $d020

		ldx #$0a
		dex
		bne *-$01
		top $eaea
		lda $d012
		cmp #rstr2p+$02
		beq *+$02
;		sta $d020

		ldx #$0a
		dex
		bne *-$01
		top $eaea
		lda $d012
		cmp #rstr2p+$03
		beq *+$02
;		sta $d020

		ldx #$0a
		dex
		bne *-$01
		dop $ea
		lda $d012
		cmp #rstr2p+$04
		beq *+$02
;		sta $d020

		ldx #$0a
		dex
		bne *-$01
		dop $ea
;		top $eaea
		lda $d012
		cmp #rstr2p+$05
		beq *+$02
;		sta $d020

; Wait for where the scroller colour splits start
		lda #$0c
		sta zp_timer
		lda #$00
		dcp zp_timer
		bne *-$02

		lda #$30
		ldy d016_mirror_2
		sty $d016
		sta $d018

; Colour splits for the scroller
		ldx #$00
scroll_split	lda scroll_colours,x
		sta $d021
		top $d021
		ldy scroll_timers,x
		dey
		bne *-$01
		inx
		cpx #$0f
		bne scroll_split

		lda scroll_colours,x
		sta $d021
		lda #$04
		sta zp_timer
		lda #$00
		dcp zp_timer
		bne *-$02
		nop

		lda #$14
		sta $d011
		lda #$07
		sta $d016


; Lower border splitter

; Almost identical to the upper border (write order of the registers is
; different along with the colour table) so see that comment for more
; detail! =-)
!set line_count=$00
!set fx_count=$00
!do {
		lda lb_colours+line_count
		sta $d021

		ldx ghostwork_1+fx_count
		lda ghostwork_2+line_count
		ldy ghostwork_3+line_count
		stx ghostbyte
		sax ghostbyte
		sty ghostbyte
		sta ghostbyte
		sax ghostbyte
		sty ghostbyte
		sta ghostbyte
		stx ghostbyte
		sax ghostbyte
		sty ghostbyte

		!if line_count<$04 {
		lda #$14
		} else {
		lda #$1b
		}
		sta $d011

		!set fx_count=fx_count+$01
		!if fx_count=$08 {
			!set fx_count=$00
		}

		!set line_count=line_count+$01
}until line_count=$22

		lda #$ff
		sta ghostbyte


; Scroll mover
scroll_upd	ldx scroll_x
		inx
		cpx #$04
		bne sx_xb

; Shift the character lines
		ldx #$00
mover		lda screen_ram+$399,x
		sta screen_ram+$398,x
		ora #$80
		sta screen_ram+$3c0,x
		inx
		cpx #$26
		bne mover

		dec char_width
		beq mread

; No new character yet - bump the current value by one
		lda screen_ram+$398+$26
		clc
		adc #$01
		sta screen_ram+$398+$26
		clc
		adc #$53
		sta screen_ram+$3c0+$26
		jmp no_fetch

; New character needed - fetch one
mread		ldx scroll_text
		bne okay
		jsr reset
		jmp mread

okay		txa
		asl
		sta screen_ram+$398+$26
		ora #$80
		sta screen_ram+$3c0+$26
		lda char_width_dcd,x
		sta char_width

		inc mread+$01
		bne *+$05
		inc mread+$02

no_fetch	ldx #$00
sx_xb		stx scroll_x

		txa
		and #$03
		asl
		eor #$07
		sta d016_mirror_2

		txa
		asl
		and #$07
		eor #$07
		sta d016_mirror_2

; Update the witch's position
		ldx witch_cnt
		lda witch_d018_tbl,x
		sta witch_d018

		inx
		cpx #$26
		bne *+$04
		ldx #$00
		stx witch_cnt

; Update the bat sprites
		lda cos_at_5
		clc
		adc #$02
		sta cos_at_5
		tax

		lda bat_x_cosinus,x
		clc
		adc sprite_x_nudge+$00
		sta sprite_x+$00

!set iter_cnt=$01
!do {
		lda #$ff
		sbx #$2c
		lda bat_x_cosinus,x
		clc
		adc sprite_x_nudge+iter_cnt
		sta sprite_x+iter_cnt

		!set iter_cnt=iter_cnt+$01
} until iter_cnt=$08

		lda cos_at_6
		clc
		adc #$03
		sta cos_at_6
		tax

		lda bat_y_cosinus,x
		sta sprite_y+$00

!set iter_cnt=$01
!do {
		lda #$ff
		sbx #$1f
		lda bat_y_cosinus,x
		sta sprite_y+iter_cnt

		!set iter_cnt=iter_cnt+$01
} until iter_cnt=$08

		ldx bat_anim_cnt
		inx
		cpx #$04
		bne bac_xb

		ldy sprite_dp+$00
		ldx #$00
bat_anim	lda sprite_dp+$01,x
		sta sprite_dp+$00,x
		inx
		cpx #$07
		bne bat_anim
		sty sprite_dp+$07

		ldx #$00
bac_xb		stx bat_anim_cnt

; Work out which sprites need their priority bit set (they fly over the
; moon but behind the witch)
		lda #$00
		sta bat_priority
		ldx #$07
bat_prior_set	lda sprite_x,x
		cmp #$88
		rol bat_priority
		dex
		cpx #$ff
		bne bat_prior_set

; Position the sprites
		ldx #$00
		ldy #$00
sprite_set	lda sprite_x,x
		sta $d000,y
		lda sprite_y,x
		sta $d001,y
		lda sprite_dp,x
		sta $4ff8,x
		sta $57f8,x
		sta $5ff8,x
		sta $67f8,x
		sta $6ff8,x
		sta $77f8,x
		lda #$0b
		sta $d027,x
		iny
		iny
		inx
		cpx #$08
		bne sprite_set

		lda #$ff
		sta $d015
		lda bat_priority
		sta $d01b

; Get ready to exit the interrupt
		lda #$02
		sta raster_num
		lda #rstr1p
		sta $d012

ea31		pla
		tay
		pla
		tax
		pla
nmi_int		rti

; Scroller self mod code reset
reset		lda #<scroll_text
		sta mread+$01
		lda #>scroll_text
		sta mread+$02
		rts


; Colour tables for the upper and lower borders
		* = ((*/$100)+1)*$100	; start at next page boundary
ub_colours	!byte $06,$06,$0b,$06,$0b,$0b,$04,$0b
		!byte $04,$04,$0c,$04,$0c,$0c,$0f,$0c
		!byte $0f,$0f,$0c,$0f,$0c,$0c,$04,$0c
		!byte $04,$04,$0b,$04,$0b,$0b,$06,$0b
		!byte $06,$06

lb_colours	!byte $06,$06,$0b,$06,$0b,$0b,$04,$0b
		!byte $04,$04,$0e,$04,$0e,$0e,$03,$0e
		!byte $03,$03,$0e,$03,$0e,$0e,$04,$0e
		!byte $04,$04,$0b,$04,$0b,$0b,$06,$0b
		!byte $06,$06

; Raster bar colours and timing for the scroller
scroll_colours	!byte $08,$0c,$0c,$0c,$08,$0c,$08,$08
		!byte $08,$02,$08,$02,$02,$02,$02,$02
scroll_timers	!byte $01,$08,$08,$08,$08,$08,$08,$08
		!byte $01,$08,$08,$08,$08,$08,$08,$08

; Graphics data for the ghostbyte effects
blob_data	!byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
		!byte $e7,$f3,$f9,$fc,$7e,$3f,$9f,$cf
		!byte $c3,$e1,$f0,$78,$3c,$1e,$0f,$87
		!byte $81,$c0,$60,$30,$18,$0c,$06,$03
		!byte $81,$c0,$60,$30,$18,$0c,$06,$03
		!byte $c3,$e1,$f0,$78,$3c,$1e,$0f,$87
		!byte $e7,$f3,$f9,$fc,$7e,$3f,$9f,$cf
		!byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff

warp_data	!byte $20,$70,$f8,$fd,$df,$8f,$07,$02

wave_data	!byte $3c,$1e,$0f,$87,$c3,$e1,$f0,$78

; Cosine tables for various movement routines
		* = ((*/$100)+1)*$100	; start at next page boundary
blob_cosinus	!byte $3f,$3f,$3f,$3f,$3f,$3f,$3f,$3f
		!byte $3f,$3f,$3f,$3e,$3e,$3e,$3e,$3d
		!byte $3d,$3d,$3c,$3c,$3c,$3b,$3b,$3b
		!byte $3a,$3a,$39,$39,$38,$38,$37,$37
		!byte $36,$36,$35,$34,$34,$33,$33,$32
		!byte $31,$31,$30,$2f,$2f,$2e,$2d,$2c
		!byte $2c,$2b,$2a,$29,$29,$28,$27,$26
		!byte $26,$25,$24,$23,$23,$22,$21,$20

		!byte $1f,$1f,$1e,$1d,$1c,$1c,$1b,$1a
		!byte $19,$18,$18,$17,$16,$15,$15,$14
		!byte $13,$12,$12,$11,$10,$10,$0f,$0e
		!byte $0e,$0d,$0c,$0c,$0b,$0b,$0a,$09
		!byte $09,$08,$08,$07,$07,$06,$06,$05
		!byte $05,$04,$04,$04,$03,$03,$03,$02
		!byte $02,$02,$01,$01,$01,$01,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00

		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$01,$01,$01,$01,$02
		!byte $02,$02,$03,$03,$03,$04,$04,$05
		!byte $05,$05,$06,$06,$07,$07,$08,$08
		!byte $09,$0a,$0a,$0b,$0b,$0c,$0d,$0d
		!byte $0e,$0f,$0f,$10,$11,$11,$12,$13
		!byte $13,$14,$15,$16,$16,$17,$18,$19
		!byte $19,$1a,$1b,$1c,$1d,$1d,$1e,$1f

		!byte $20,$20,$21,$22,$23,$24,$24,$25
		!byte $26,$27,$27,$28,$29,$2a,$2a,$2b
		!byte $2c,$2d,$2d,$2e,$2f,$2f,$30,$31
		!byte $31,$32,$33,$33,$34,$35,$35,$36
		!byte $36,$37,$37,$38,$38,$39,$39,$3a
		!byte $3a,$3b,$3b,$3b,$3c,$3c,$3d,$3d
		!byte $3d,$3d,$3e,$3e,$3e,$3e,$3f,$3f
		!byte $3f,$3f,$3f,$3f,$3f,$3f,$3f,$3f

warp_cosinus	!byte $27,$27,$27,$27,$27,$27,$27,$27
		!byte $27,$27,$27,$27,$27,$26,$26,$26
		!byte $26,$26,$26,$25,$25,$25,$25,$24
		!byte $24,$24,$24,$23,$23,$23,$22,$22
		!byte $22,$21,$21,$21,$20,$20,$1f,$1f
		!byte $1f,$1e,$1e,$1d,$1d,$1c,$1c,$1c
		!byte $1b,$1b,$1a,$1a,$19,$19,$18,$18
		!byte $17,$17,$16,$16,$15,$15,$14,$14

		!byte $13,$13,$12,$12,$12,$11,$11,$10
		!byte $10,$0f,$0f,$0e,$0e,$0d,$0d,$0c
		!byte $0c,$0b,$0b,$0a,$0a,$0a,$09,$09
		!byte $08,$08,$08,$07,$07,$06,$06,$06
		!byte $05,$05,$05,$04,$04,$04,$03,$03
		!byte $03,$03,$02,$02,$02,$02,$01,$01
		!byte $01,$01,$01,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00

		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$01,$01,$01
		!byte $01,$01,$01,$02,$02,$02,$02,$03
		!byte $03,$03,$03,$04,$04,$04,$05,$05
		!byte $05,$06,$06,$07,$07,$07,$08,$08
		!byte $08,$09,$09,$0a,$0a,$0b,$0b,$0b
		!byte $0c,$0c,$0d,$0d,$0e,$0e,$0f,$0f
		!byte $10,$10,$11,$11,$12,$12,$13,$13

		!byte $14,$14,$15,$15,$16,$16,$17,$17
		!byte $18,$18,$18,$19,$19,$1a,$1a,$1b
		!byte $1b,$1c,$1c,$1d,$1d,$1d,$1e,$1e
		!byte $1f,$1f,$20,$20,$20,$21,$21,$21
		!byte $22,$22,$22,$23,$23,$23,$24,$24
		!byte $24,$24,$25,$25,$25,$25,$26,$26
		!byte $26,$26,$26,$27,$27,$27,$27,$27
		!byte $27,$27,$27,$27,$27,$27,$27,$27

logo_cosinus	!byte $cf,$cf,$cf,$cf,$cf,$cf,$ce,$ce
		!byte $cd,$cd,$cc,$cc,$cb,$ca,$c9,$c9
		!byte $c8,$c7,$c5,$c4,$c3,$c2,$c1,$bf
		!byte $be,$bc,$bb,$b9,$b8,$b6,$b5,$b3
		!byte $b1,$af,$ad,$ab,$a9,$a7,$a5,$a3
		!byte $a1,$9f,$9d,$9b,$98,$96,$94,$92
		!byte $8f,$8d,$8a,$88,$86,$83,$81,$7e
		!byte $7c,$79,$77,$74,$72,$6f,$6c,$6a

		!byte $67,$65,$62,$60,$5d,$5b,$58,$56
		!byte $53,$51,$4e,$4c,$49,$47,$44,$42
		!byte $3f,$3d,$3b,$39,$36,$34,$32,$30
		!byte $2e,$2b,$29,$27,$25,$23,$21,$20
		!byte $1e,$1c,$1a,$19,$17,$15,$14,$12
		!byte $11,$0f,$0e,$0d,$0c,$0a,$09,$08
		!byte $07,$06,$05,$05,$04,$03,$03,$02
		!byte $01,$01,$01,$00,$00,$00,$00,$00

		!byte $00,$00,$00,$00,$00,$00,$01,$01
		!byte $02,$02,$03,$03,$04,$05,$06,$07
		!byte $08,$09,$0a,$0b,$0c,$0d,$0f,$10
		!byte $11,$13,$14,$16,$17,$19,$1b,$1c
		!byte $1e,$20,$22,$24,$26,$28,$2a,$2c
		!byte $2e,$30,$32,$35,$37,$39,$3b,$3e
		!byte $40,$43,$45,$47,$4a,$4c,$4f,$51
		!byte $54,$56,$59,$5b,$5e,$60,$63,$65

		!byte $68,$6b,$6d,$70,$72,$75,$77,$7a
		!byte $7c,$7f,$81,$84,$86,$89,$8b,$8d
		!byte $90,$92,$95,$97,$99,$9b,$9d,$a0
		!byte $a2,$a4,$a6,$a8,$aa,$ac,$ae,$b0
		!byte $b1,$b3,$b5,$b7,$b8,$ba,$bb,$bd
		!byte $be,$c0,$c1,$c2,$c4,$c5,$c6,$c7
		!byte $c8,$c9,$ca,$ca,$cb,$cc,$cd,$cd
		!byte $ce,$ce,$ce,$cf,$cf,$cf,$cf,$cf

bat_x_cosinus	!byte $c7,$c7,$c7,$c7,$c7,$c7,$c7,$c6
		!byte $c6,$c6,$c5,$c5,$c4,$c3,$c3,$c2
		!byte $c1,$c0,$c0,$bf,$be,$bd,$bc,$bb
		!byte $ba,$b9,$b7,$b6,$b5,$b4,$b2,$b1
		!byte $af,$ae,$ad,$ab,$a9,$a8,$a6,$a5
		!byte $a3,$a1,$a0,$9e,$9c,$9a,$98,$97
		!byte $95,$93,$91,$8f,$8d,$8b,$89,$87
		!byte $85,$83,$81,$7f,$7d,$7b,$79,$77

		!byte $75,$73,$71,$6f,$6d,$6b,$69,$67
		!byte $65,$63,$61,$5f,$5e,$5c,$5a,$58
		!byte $56,$54,$52,$50,$4f,$4d,$4b,$49
		!byte $48,$46,$44,$43,$41,$40,$3e,$3d
		!byte $3b,$3a,$39,$37,$36,$35,$33,$32
		!byte $31,$30,$2f,$2e,$2d,$2c,$2b,$2a
		!byte $2a,$29,$28,$28,$27,$26,$26,$25
		!byte $25,$25,$24,$24,$24,$24,$24,$24

		!byte $24,$24,$24,$24,$24,$24,$24,$25
		!byte $25,$26,$26,$27,$27,$28,$28,$29
		!byte $2a,$2b,$2c,$2c,$2d,$2e,$2f,$30
		!byte $32,$33,$34,$35,$36,$38,$39,$3a
		!byte $3c,$3d,$3f,$40,$42,$43,$45,$47
		!byte $48,$4a,$4c,$4d,$4f,$51,$53,$55
		!byte $56,$58,$5a,$5c,$5e,$60,$62,$64
		!byte $66,$68,$6a,$6c,$6e,$70,$72,$74

		!byte $76,$78,$7a,$7c,$7e,$80,$82,$84
		!byte $86,$88,$8a,$8c,$8e,$90,$92,$93
		!byte $95,$97,$99,$9b,$9d,$9e,$a0,$a2
		!byte $a3,$a5,$a7,$a8,$aa,$ab,$ad,$ae
		!byte $b0,$b1,$b3,$b4,$b5,$b6,$b8,$b9
		!byte $ba,$bb,$bc,$bd,$be,$bf,$c0,$c1
		!byte $c1,$c2,$c3,$c4,$c4,$c5,$c5,$c6
		!byte $c6,$c6,$c7,$c7,$c7,$c7,$c7,$c7

bat_y_cosinus	!byte $cc,$cc,$cc,$cc,$cc,$cc,$cc,$cc
		!byte $cb,$cb,$cb,$cb,$ca,$ca,$c9,$c9
		!byte $c8,$c8,$c7,$c7,$c6,$c6,$c5,$c4
		!byte $c4,$c3,$c2,$c1,$c1,$c0,$bf,$be
		!byte $bd,$bc,$bb,$ba,$b9,$b8,$b7,$b6
		!byte $b5,$b4,$b3,$b2,$b1,$b0,$ae,$ad
		!byte $ac,$ab,$aa,$a8,$a7,$a6,$a5,$a3
		!byte $a2,$a1,$a0,$9e,$9d,$9c,$9a,$99

		!byte $98,$97,$95,$94,$93,$91,$90,$8f
		!byte $8e,$8c,$8b,$8a,$89,$87,$86,$85
		!byte $84,$83,$81,$80,$7f,$7e,$7d,$7c
		!byte $7b,$7a,$79,$78,$77,$76,$75,$74
		!byte $73,$72,$71,$70,$6f,$6f,$6e,$6d
		!byte $6c,$6c,$6b,$6a,$6a,$69,$68,$68
		!byte $67,$67,$67,$66,$66,$65,$65,$65
		!byte $64,$64,$64,$64,$64,$64,$64,$64

		!byte $64,$64,$64,$64,$64,$64,$64,$64
		!byte $65,$65,$65,$65,$66,$66,$67,$67
		!byte $68,$68,$69,$69,$6a,$6a,$6b,$6c
		!byte $6c,$6d,$6e,$6f,$70,$70,$71,$72
		!byte $73,$74,$75,$76,$77,$78,$79,$7a
		!byte $7b,$7c,$7d,$7e,$7f,$81,$82,$83
		!byte $84,$85,$87,$88,$89,$8a,$8c,$8d
		!byte $8e,$8f,$91,$92,$93,$94,$96,$97

		!byte $98,$9a,$9b,$9c,$9d,$9f,$a0,$a1
		!byte $a3,$a4,$a5,$a6,$a8,$a9,$aa,$ab
		!byte $ac,$ae,$af,$b0,$b1,$b2,$b3,$b4
		!byte $b5,$b6,$b8,$b9,$ba,$bb,$bb,$bc
		!byte $bd,$be,$bf,$c0,$c1,$c2,$c2,$c3
		!byte $c4,$c5,$c5,$c6,$c6,$c7,$c8,$c8
		!byte $c9,$c9,$ca,$ca,$ca,$cb,$cb,$cb
		!byte $cc,$cc,$cc,$cc,$cc,$cc,$cc,$cc

; Data for the logo
		* = ((*/$100)+1)*$100	; start at next page boundary
logo_data_ln1	!byte $14,$15,$16,$17
		!byte $18,$19,$1a,$1b,$1c,$1d,$1e,$1f
		!byte $00,$00,$00,$00
		!byte $00,$01,$02,$03,$04,$05,$06,$07
		!byte $08,$09,$0a,$0b,$0c,$0d,$0e,$0f
		!byte $10,$11,$12,$13,$14,$15,$16,$17
		!byte $18,$19,$1a,$1b,$1c,$1d,$1e,$1f
		!byte $00,$00,$00,$00
		!byte $00,$01,$02,$03,$04,$05,$06,$07
		!byte $08,$09,$0a,$0b

logo_data_ln2	!byte $34,$35,$36,$37
		!byte $38,$39,$3a,$3b,$3c,$3d,$3e,$3f
		!byte $00,$00,$00,$00
		!byte $20,$21,$22,$23,$24,$25,$26,$27
		!byte $28,$29,$2a,$2b,$2c,$2d,$2e,$2f
		!byte $30,$31,$32,$33,$34,$35,$36,$37
		!byte $38,$39,$3a,$3b,$3c,$3d,$3e,$3f
		!byte $00,$00,$00,$00
		!byte $20,$21,$22,$23,$24,$25,$26,$27
		!byte $28,$29,$2a,$2b

logo_data_ln3	!byte $54,$55,$56,$57
		!byte $58,$59,$5a,$5b,$5c,$5d,$5e,$5f
		!byte $00,$00,$00,$00
		!byte $40,$41,$42,$43,$44,$45,$46,$47
		!byte $48,$49,$4a,$4b,$4c,$4d,$4e,$4f
		!byte $50,$51,$52,$53,$54,$55,$56,$57
		!byte $58,$59,$5a,$5b,$5c,$5d,$5e,$5f
		!byte $00,$00,$00,$00
		!byte $40,$41,$42,$43,$44,$45,$46,$47
		!byte $48,$49,$4a,$4b

logo_data_ln4	!byte $74,$75,$76,$77
		!byte $78,$79,$7a,$7b,$7c,$7d,$7e,$7f
		!byte $00,$00,$00,$00
		!byte $60,$61,$62,$63,$64,$65,$66,$67
		!byte $68,$69,$6a,$6b,$6c,$6d,$6e,$6f
		!byte $70,$71,$72,$73,$74,$75,$76,$77
		!byte $78,$79,$7a,$7b,$7c,$7d,$7e,$7f
		!byte $00,$00,$00,$00
		!byte $60,$61,$62,$63,$64,$65,$66,$67
		!byte $68,$69,$6a,$6b

logo_data_ln5	!byte $94,$95,$96,$97
		!byte $98,$99,$9a,$9b,$9c,$9d,$9e,$9f
		!byte $00,$00,$00,$00
		!byte $80,$81,$82,$83,$84,$85,$86,$87
		!byte $88,$89,$8a,$8b,$8c,$8d,$8e,$8f
		!byte $90,$91,$92,$93,$94,$95,$96,$97
		!byte $98,$99,$9a,$9b,$9c,$9d,$9e,$9f
		!byte $00,$00,$00,$00
		!byte $80,$81,$82,$83,$84,$85,$86,$87
		!byte $88,$89,$8a,$8b

logo_data_ln6	!byte $b4,$b5,$b6,$b7
		!byte $b8,$b9,$ba,$bb,$bc,$bd,$be,$bf
		!byte $00,$00,$00,$00
		!byte $a0,$a1,$a2,$a3,$a4,$a5,$a6,$a7
		!byte $a8,$a9,$aa,$ab,$ac,$ad,$ae,$af
		!byte $b0,$b1,$b2,$b3,$b4,$b5,$b6,$b7
		!byte $b8,$b9,$ba,$bb,$bc,$bd,$be,$bf
		!byte $00,$00,$00,$00
		!byte $a0,$a1,$a2,$a3,$a4,$a5,$a6,$a7
		!byte $a8,$a9,$aa,$ab

; Colour table for the logo (each character has a unique colour value)
		* = ((*/$100)+1)*$100	; start at next page boundary
logo_colour_dcd	!byte $0a,$0a,$0a,$0a,$0a,$0a,$0a,$0a
		!byte $0a,$0a,$0a,$0a,$0a,$0a,$0a,$0a
		!byte $0a,$0a,$0a,$0a,$0a,$0a,$0a,$0a
		!byte $08,$08,$0a,$0a,$0a,$0a,$0a,$0a

		!byte $08,$08,$0a,$0a,$0a,$0a,$0a,$0a
		!byte $0a,$0a,$0a,$0a,$08,$0a,$0a,$0a
		!byte $0a,$0a,$0a,$0a,$08,$0a,$0a,$0a
		!byte $08,$08,$08,$0a,$0a,$0a,$0a,$0a

		!byte $08,$08,$08,$0a,$0a,$0a,$0a,$08
		!byte $08,$08,$08,$08,$08,$0a,$0a,$08
		!byte $08,$08,$08,$08,$08,$0a,$0a,$08
		!byte $08,$08,$0a,$0a,$0a,$0a,$08,$08

		!byte $08,$08,$08,$08,$08,$08,$08,$08
		!byte $08,$08,$08,$08,$08,$08,$08,$08
		!byte $08,$08,$08,$08,$08,$08,$08,$08
		!byte $08,$08,$08,$08,$08,$08,$08,$08

		!byte $08,$08,$08,$08,$08,$08,$08,$08
		!byte $08,$08,$08,$08,$08,$08,$08,$08
		!byte $08,$08,$08,$08,$08,$08,$08,$08
		!byte $08,$08,$08,$08,$08,$08,$08,$08

		!byte $08,$08,$08,$08,$08,$08,$08,$08
		!byte $02,$02,$08,$08,$08,$08,$08,$08
		!byte $08,$08,$08,$08,$08,$08,$08,$08
		!byte $08,$08,$08,$02,$02,$08,$08,$08

; Picture colour data
pic_col_data	!byte $0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f
		!byte $0f,$0f,$0f,$0f,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00

; $d018 values to make the witch bob up and down
witch_d018_tbl	!byte $32,$32,$32,$32,$32,$54,$54,$54
		!byte $54,$76,$76,$76,$98,$98,$98,$ba
		!byte $ba,$ba,$ba

		!byte $dc,$dc,$dc,$dc,$dc,$ba,$ba,$ba
		!byte $ba,$98,$98,$98,$76,$76,$76,$54
		!byte $54,$54,$54

; Sprite data pointer values for the bat flapping animation
sprite_x	!byte $00,$00,$00,$00,$00,$00,$00,$00
sprite_x_nudge	!byte $38,$10,$28,$08,$30,$14,$20,$00
sprite_y	!byte $00,$00,$00,$00,$00,$00,$00,$00
sprite_dp	!byte $fb,$fc,$fd,$fe,$ff,$fe,$fd,$fc


; Width for each character
		* = $8000
char_width_dcd	!byte $01,$02,$02,$02,$02,$02,$02,$02		; @ to G
		!byte $02,$01,$02,$02,$02,$02,$02,$02		; H to O
		!byte $02,$02,$02,$02,$02,$02,$02,$02		; P to W
		!byte $02,$02,$02,$01,$03,$01,$01,$01		; X to Z, 5 * punct.
		!byte $01,$01,$01,$01,$01,$01,$02,$01		; space to '
		!byte $01,$01,$01,$02,$01,$02,$01,$02		; ( to /
		!byte $02,$01,$02,$02,$02,$02,$02,$02		; 0 to 7
		!byte $02,$02,$01,$01,$01,$01,$01,$02		; 8 to ?

; The good old scrolling message - $00 wraps to the start, $1c is a pumpkin!
scroll_text	!scr "welcome to the creepy and ooky    "
		!scr $1c," md201510 ",$1c
		!scr "    released for halloween 2015 from cosine's spooky old "
		!scr "house on the hill"
		!scr "    ",$1c,"    "

		!scr "coding, graphics wiring and tweaking by t.m.r "
		!scr "with appropriate music from odie which was originally written "
		!scr "for the game elvira - mistress of the dark"
		!scr "    ",$1c,"    "

		!scr "this is an entry into the first unintended opcode "
		!scr "coding challenge at the csdb as well as being the "
		!scr "second monthly demo from cosine... and getting a bit of "
		!scr "spoopy halloween stuff in for good measure since it was "
		!scr "released at the end of october - yays for covering three "
		!scr "bases at the same time!"
		!scr "    ",$1c,"    "

		!scr "the compo rules say a breakdown of where the pseudo opcodes "
		!scr "are used needs to be included and that can be found in the "
		!scr "source code which comes shipped with the demo or is available "
		!scr "with the binary data required to assemble it from the github "
		!scr "repository - have a look at the specific page for this demo "
		!scr "at http://cosine.org.uk/ and there should be a github link!"
		!scr "    ",$1c,"    "

		!scr "the main effect is the four cycle wide ghostbyte splitter "
		!scr "with four possible values being pushed in per scanline; "
		!scr "since these are usually done by loading the three registers "
		!scr "with values at the start of each scanline, the fourth relies "
		!scr "on the pseudo opcode sax which ands the contents of a and x "
		!scr "together whilst writing them to memory. the logo movement is"
		!scr " also using lax to save a few cycles per character when "
		!scr "fetching the colour data and there's a liberal sprinkling of "
		!scr "dcp and sbx throughout for flavour."
		!scr "    ",$1c,"    "

		!scr "i'm not really into doing themed demos personally, but the "
		!scr "primary effect is in the upper and lower borders so something "
		!scr "was needed to fill the rest of the screen; this character "
		!scr "set was pretty much done and waiting for a home so, since "
		!scr "md201510 was obviously destined to arrive at some point "
		!scr "during october, it made sense to combine the two and go for "
		!scr "a ghoulish theme.   and hopefully it works....?"
		!scr "    ",$1c,"    "

		!scr "as always seems to be the case these days, there's loads of "
		!scr "spare memory for text but i don't have much to actually say "
		!scr "right now!   so instead here come some gruesome greetings "
		!scr "eerily float towards...    "

		!scr "abyss connection ",$1c," "
		!scr "arkanix labs ",$1c," "
		!scr "artstate ",$1c," "
		!scr "ate bit ",$1c," "
		!scr "booze design ",$1c," "
		!scr "camelot ",$1c," "
		!scr "chorus ",$1c," "
		!scr "chrome ",$1c," "
		!scr "c.n.c.d. ",$1c," "
		!scr "c.p.u. ",$1c," "
		!scr "crescent ",$1c," "
		!scr "crest ",$1c," "
		!scr "covert bitops ",$1c," "
		!scr "defence force ",$1c," "
		!scr "dekadence ",$1c," "
		!scr "desire ",$1c," "
		!scr "d.a.c. ",$1c," "
		!scr "dmagic ",$1c," "
		!scr "dual crew ",$1c," "
		!scr "exclusive on ",$1c," "
		!scr "fairlight ",$1c," "
		!scr "fire ",$1c," "
		!scr "focus ",$1c," "
		!scr "funkscientist productions ",$1c," "
		!scr "genesis project ",$1c," "
		!scr "gheymaid inc. ",$1c," "
		!scr "hitmen ",$1c," "
		!scr "hokuto force ",$1c," "
		!scr "level64 ",$1c," "
		!scr "m and m ",$1c," "
		!scr "maniacs of noise ",$1c," "
		!scr "meanteam ",$1c," "
		!scr "metalvotze ",$1c," "
		!scr "noname ",$1c," "
		!scr "nostalgia ",$1c," "
		!scr "nuance ",$1c," "
		!scr "offence ",$1c," "
		!scr "onslaught ",$1c," "
		!scr "orb ",$1c," "
		!scr "oxyron ",$1c," "
		!scr "padua ",$1c," "
		!scr "plush ",$1c," "
		!scr "psytronik ",$1c," "
		!scr "reptilia ",$1c," "
		!scr "resource ",$1c," "
		!scr "rgcd ",$1c," "
		!scr "secure ",$1c," "
		!scr "shape ",$1c," "
		!scr "side b ",$1c," "
		!scr "slash ",$1c," "
		!scr "slipstream ",$1c," "
		!scr "success and trc ",$1c," "
		!scr "style ",$1c," "
		!scr "suicyco industries ",$1c," "
		!scr "taquart ",$1c," "
		!scr "tempest ",$1c," "
		!scr "tek ",$1c," "
		!scr "triad ",$1c," "
		!scr "trsi ",$1c," "
		!scr "viruz ",$1c," "
		!scr "vision ",$1c," "
		!scr "wow ",$1c," "
		!scr "wrath ",$1c," "
		!scr "xenon ",$1c," "

		!scr "and of course a now traditional apology to anybody who "
		!scr "was forgotten!  also, muahahaha!!"
		!scr "    ",$1c,"    "

		!scr "and, since i'm working to something of a deadline here and "
		!scr "this text has been left to the last minute on the day of "
		!scr "release with all manner of pr-type things like the youtube "
		!scr "video still pending, i might as well call it sorted and leave "
		!scr "you all to enjoy the excellent music and swirly effects."
		!scr "    ",$1c,"    "

		!scr "this was tmr of cosine on 31st october 2015, drifting "
		!scr "silently into the night... .. .  ."
		!scr "    ",$1c,"    "
		!byte $00
