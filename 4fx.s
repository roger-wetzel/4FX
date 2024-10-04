; SPREADPOINT
; 4FX (AMIGA, PAL, OCS, 68000, 512 KB)
; MUSIC: VIRGILL, INSPIRATION: MNEMOTRON, CODE AND GFX: DEPECHE
; (C) 2024 DEPECHE

; Build with vasm
; vasmm68k_mot -kick1hunks -Fhunkexe -o 4fx -nosym 4fx.s

AbsExecBase	equ	4
OldOpenLibrary	equ	-408
CloseLibrary	equ	-414
Write		equ	-48
Output		equ	-60
AvailMem	equ	-216
AllocMem	equ	-198
FreeMem		equ	-210
TypeOfMem	equ	-534
WaitTOF		equ	-270
Forbid		equ	-132
Permit		equ	-138
LoadView	equ	-222

custom		equ	$dff000

pwidth		equ	40
laddpwidth	equ	80
center		equ	psize/2+(pwidth/2)
numplanes	equ	2

pheight		equ	256 ; px
psize		equ	pheight*pwidth
safetyzone	equ	2*pwidth ; probably not necessary

numrays		equ	3

numinstactions	equ	(instactionsend-instactions)/6	; 173 + 1
instoffset	equ	510	; -(-510) $fe02
instnop		equ	$216	; no instrument was played (nop)

numvertices	equ	96	; LMS
numanimsteps	equ	256
numrasterlines	equ	256
numlmslines	equ	(lmslinesend-lmslines)/4

txtpwidth	equ	38 ; depends on max line text width
lineheight	equ	13
num4fxlines	equ	24
numtxtlines	equ	62 ; see text -> count lines
txtlinepadding	equ	11 ; empty lines at beginning, after 4fx, before credits and end
numgentxtlines	equ	4*txtlinepadding+num4fxlines+numtxtlines

nummtns		equ	11 ; SPREADPOINT

; profiling
numbers		equ	0

instruments	equ	0	; in combination with "numbers" only
availablemem	equ	0
timing		equ	0
testing		equ	0

; DMACON
SET		equ	1<<15		; 0=clear, 1=set bits that are set to 1 below
BLTPRI		equ	1<<10		; Blitter DMA priority (over CPU) "blitter nasty"
DMAEN		equ	1<<9		; Enable all DMA below
BPLEN		equ	1<<8		; Bitplane DMA
COPEN		equ	1<<7		; Copper DMA
BLTEN		equ	1<<6		; Blitter DMA
SPREN		equ	1<<5		; Sprite DMA


*------	MACROS ----------------------------------------------------------------*

; actor bits
actor_srf		equ	0
actor_player		equ	1
actor_lms		equ	2
actor_lmsup		equ	3
actor_lmsdown		equ	4
actor_txt		equ	5
actor_lsp		equ	6
actor_prepare_txt	equ	7
actor_mtn		equ	8
actor_srfexpand		equ	9
actor_srfshrink		equ	10
actor_ladd		equ	11
actor_prepare_4fx	equ	12
actor_ladd_fading	equ	13

; note: elseif does not work correctly
	macro CHECKACTOR
	if \1<8
		btst	#\1,v_actors+3(a5)
		mexit
	endif
	if \1<16
		btst	#\1-8,v_actors+2(a5)
		mexit
	endif
	if \1<24
		btst	#\1-16,v_actors+1(a5)
		mexit
	endif
	if \1<32
		btst	#\1-24,v_actors(a5)
		mexit
	endif
	if \1>=32
		fail Out of range
	endif
	endm

	macro STARTACTOR
	if \1<8
		bset	#\1,v_actors+3(a5)
		mexit
	endif
	if \1<16
		bset	#\1-8,v_actors+2(a5)
		mexit
	endif
	if \1<24
		bset	#\1-16,v_actors+1(a5)
		mexit
	endif
	if \1<32
		bset	#\1-24,v_actors(a5)
		mexit
	endif
	if \1>=32
		fail Out of range
	endif
	endm

	macro STOPACTOR
	if \1<8
		bclr	#\1,v_actors+3(a5)
		mexit
	endif
	if \1<16
		bclr	#\1-8,v_actors+2(a5)
		mexit
	endif
	if \1<24
		bclr	#\1-16,v_actors+1(a5)
		mexit
	endif
	if \1<32
		bclr	#\1-24,v_actors(a5)
		mexit
	endif
	if \1>=32
		fail Out of range
	endif
	endm


*------	ENTRY -----------------------------------------------------------------*

base	movem.l	d0-a6,-(a7)		;
	bsr	allocandinit		;
	bne	.exit			; out of memory error?

	if availablemem
	move.l	AbsExecBase.w,a6	;
	move.l	#MEMF_CHIP,d1		;
	jsr	AvailMem(a6)		;
	move.l	b_vars(pc),a5		;
	move.l	d0,v_number(a5)		; free (available) chip memory
	endif

	move.l	AbsExecBase.w,a6	;
	lea	.gfx(pc),a1		;
	jsr	OldOpenLibrary(a6)	; open gfx library
	move.l	d0,a6			;
	beq	.exit			; could not open gfx library
	move.l 	34(a6),-(a7)		; view
	move.l	d0,-(a7)		; gfx base
	move.l 	38(a6),-(a7)		; copper list 1
	move.l 	50(a6),-(a7)		; copper list 2
	sub.l	a1,a1			;
	jsr	LoadView(a6)		;
	jsr	WaitTOF(a6)		;
	jsr	WaitTOF(a6)		;
	move.l	AbsExecBase.w,a6	;
	jsr	Forbid(a6)		;
	
	lea	custom,a6		;

	move.w	$02(a6),-(a7)		; store DMA control
	move.w	$1c(a6),-(a7)		; store interrupt enable bits
	move.l	$6c.w,-(a7)		; store irq3

	bsr	waitblitter		;
	bsr	waitraster		; avoid flickering (?) and sprite dma "bug"

	move.w	#$7fff,d0		;
	move.w	d0,$9a(a6)		;
	move.w	d0,$9c(a6)		; delete all interrupt requests
	move.w	d0,$9c(a6)		;
	move.w	d0,$96(a6)		; disable all DMAs

	bsr	volumetozero		;

	move.w	#mtnc0,d0		; color
	moveq	#32-1,d7		;
	lea	$180(a6),a0		;
.black	move.w	d0,(a0)+		;
	dbf	d7,.black		;		

	clr.w	-(a7)			; store LED state
	btst	#1,$bfe001		;
	beq	.ledstate		;
	not.w	(a7)			;
.ledstate
	bset	#1,$bfe001		; LED dark

*------	START -----------------------------------------------------------------*

	lea	irq3(pc),a0		;
	move.l	a0,$6c.w		;

	move.l	b_clist5(pc),$80(a6)	; Ladd clist

	move.w	#SET+DMAEN+BPLEN+BLTEN+COPEN+SPREN,$96(a6) ;
	move.w	#$c020,$9a(a6)		; enable vertb interrupt

	sub.l	a2,a2			; VBR 68000 (improve for > 68000)
	bsr	LSP_MusicDriver_CIA_Start ;

*------	IDLE LOOP -------------------------------------------------------------*

	bsr	backgroundtasks		;
	bsr	waitraster		;
	
*------	RESTORE STATE AND EXIT ------------------------------------------------*
	
	tst.w	(a7)+			; restore state
	bne	.leddark		;
	bclr	#1,$bfe001		; LED bright
.leddark
	bsr	waitblitter		;
	bsr	waitraster		;

	move.w	#$7fff,d0		;
	move.w	d0,$9a(a6)		;
	move.w	d0,$9c(a6)		;
	move.w	d0,$96(a6)		;

	bsr	LSP_MusicDriver_CIA_Stop ;
	bsr	volumetozero		;

	move.l	(a7)+,$6c.w		; 
	move.w	(a7)+,d0		;
	or.w	#$c000,d0		;
	move.w	d0,$9a(a6)		;
	move.w	(a7)+,d0		;
	or.w	#$8000,d0		;
	move.w	d0,$96(a6)		;

	move.l	(a7)+,$84(a6)		; copper list 2
	move.l	(a7)+,$80(a6)		; copper list 1
	move.l	(a7)+,a6		; gfx base
	move.l	(a7)+,a1		; view
	jsr	LoadView(a6)		;
	jsr	WaitTOF(a6)		;
	jsr	WaitTOF(a6)		;
	move.l	a6,a1			; parameter for CloseLibrary
	move.l	AbsExecBase.w,a6	;
	jsr	CloseLibrary(a6)	; close gfx library
	jsr	Permit(a6)		;

	bsr	dealloc			;
.exit	movem.l	(a7)+,d0-a6		;
	moveq	#0,d0			;
	rts				;

.gfx	dc.b	"graphics.library",0
	even


*------	VOLUME TO ZERO --------------------------------------------------------*

volumetozero
	moveq	#0,d0			; volume to zero
	move.w	d0,$a8(a6)		;
	move.w	d0,$b8(a6)		;
	move.w	d0,$c8(a6)		;
	move.w	d0,$d8(a6)		;
	rts				;


*------	VARS ------------------------------------------------------------------*

; # = do not change order
	rsreset
v_doquit	rs.b	1	; signal quit
v_wait		rs.b	1

v_txtmirror	rs.b	256	; mirrored byte values

v_actors	rs.l	1

v_cmdspointer	rs.l	1

v_matrix	rs.w	3*3	; 3D rotation matrix
v_a		rs.w	1
v_b		rs.w	1
v_c		rs.w	1

v_bitplane	rs.l	1
v_db1a2a	rs.l	1	; #
v_db1b2b	rs.l	1	; #

; txt
v_txtindex	rs.w	1
v_txty		rs.w	1
v_txtcolindex	rs.w	1
v_txtbuffer	rs.l	1
v_txtbuffertemp	rs.l	1
v_txtpointer	rs.l	1
v_txtline	rs.w	1	; v_txtlines index (multiple of sizeoftxt)
v_txtlines	rs.b	numgentxtlines*sizeoftxt

v_frame		rs.w	1	; frame counter

	if numbers
v_number	rs.l	1	; test value
v_waitcount	rs.w	1

v_ninst0	rs.w    1	; #
v_ninst1	rs.w    1	; # 
v_ninst2	rs.w    1	; #
v_ninst3	rs.w    1	; #
	endif

v_zero		rs.l	10	; cpu kill: init 10 registers with value 0

	if instruments
v_instlow	rs.w	1	; $fe02 (-510)
v_insthigh	rs.w	1	; $0210 (528)    1038 = 6
	endif
v_inst0		rs.w    1	; #
v_inst1		rs.w    1	; # 
v_inst2		rs.w    1	; #
v_inst3		rs.w    1	; #
v_instneg	rs.w	1	;

v_instbdindex	rs.w	1	;
v_instwdindex	rs.w	1	; cmd_srfexpand, cmd_srfshrink

v_rays		rs.w	0	; marker only
v_rayindex1	rs.w	1	; #
v_rayindexadd1	rs.w	1	; #
v_rayvertex1	rs.w	2	; x y #
v_raycolor1a	rs.w	1	; #
v_raycolor1b	rs.w	1	; #
v_rayprevious1	rs.l	1	; #

v_rayindex2	rs.w	1	; #
v_rayindexadd2	rs.w	1	; #
v_rayvertex2	rs.w	2	; x y #
v_raycolor2a	rs.w	1	; #
v_raycolor2b	rs.w	1	; #
v_rayprevious2	rs.l	1	; #

v_rayindex3	rs.w	1	; #
v_rayindexadd3	rs.w	1	; #
v_rayvertex3	rs.w	2	; x y #
v_raycolor3a	rs.w	1	; #
v_raycolor3b	rs.w	1	; #
v_rayprevious3	rs.l	1	; #

v_raysxy	rs.w	1024*2	; precaculated x and y

v_lmsanimindex	rs.w	1
v_cosmicsindex	rs.w	1
v_cosmicsidxd	rs.w	1	; delta
v_cosmicsidxs	rs.w	1	; stop index
v_lmsnumlines	rs.w	1
v_lmscolindex	rs.w	1

v_mtns		rs.b	nummtns*sizeofmtn
v_mtnunitx	rs.w	1	; unit vector (x) #
v_mtnunity	rs.w	1	; unit vector (y) #
v_mtnlines	rs.w	nummtns*4	; max possible "abbrechen" at the same time

v_laddcolindex	rs.w	1
v_laddcbank	rs.w	1
sizeofvars	rs.w	0

	rsreset
ray_index	rs.w	1	; #
ray_indexadd	rs.w	1	; #
ray_vertex	rs.w	2	; #
ray_colora	rs.w	1	; #
ray_colorb	rs.w	1	; #
ray_previous	rs.l	1	; #
sizeofray	rs.w	0

	rsreset
txt_data	rs.l	1
txt_xpos	rs.w	1
sizeoftxt	rs.w	0

	rsreset
mtn_delay	rs.w	1	; # negative = done
mtn_bitplane	rs.w	1	; #
mtn_x		rs.w	1	; #
mtn_y		rs.w	1	; #
mtn_angle	rs.w	1	; #
mtn_direction	rs.w	1	; # left or right
mtn_length	rs.w	1	; #
mtn_duration	rs.w	1	; #
mtn_data	rs.l	1	; #
sizeofmtn	rs.w	0

; mtn_data for each mtn char

delta	equ	8
delay	equ	73
delayg	equ	220
mtny	equ	-70

mtn_s	dc.w	9*delay+delayg, 0, -145,mtny, 1024, delta, 30, 512/delta ; mtn_delay ... mtn_duration
	dc.w	4,delta
	dc.w	1,delta
	dc.w	3,-delta
	dc.w	1,-delta
	dc.w	3,delta
	dc.w	3,delta
	dc.w	4,delta
	dc.w	1,delta
	dc.w	3,-delta
	dc.w	1,-delta
	dc.w	3,delta
	dc.w	3,0 ; 0 = done

mtn_p1	dc.w	6*delay+delayg, 0, -145+50,mtny, 1024, delta, 24, 512/delta ; mtn_delay ... mtn_duration
	dc.w	4,delta
	dc.w	3,delta
	dc.w	3,-delta
	dc.w	2,delta
	dc.w	1,delta
	dc.w	3,delta
	dc.w	3,-delta
	dc.w	1,-delta
	dc.w	3,delta
	dc.w	1,0 ; 0 = done

mtn_r	dc.w	1*delay+delayg, 0, -145+(2*50),mtny, 1024, delta, 28, 512/delta ; mtn_delay ... mtn_duration
	dc.w	4,delta
	dc.w	3,delta
	dc.w	1,-delta
	dc.w	2,delta
	dc.w	1,delta
	dc.w	2,-delta
	dc.w	1,-delta
	dc.w	2,delta
	dc.w	1,delta
	dc.w	3,delta
	dc.w	3,-delta
	dc.w	1,-delta
	dc.w	3,delta
	dc.w	1,0 ; 0 = done

mtn_e	dc.w	10*delay+130+delayg, 0, -145+(3*50),mtny, 1024, delta, 30, 512/delta ; mtn_delay ... mtn_duration
	dc.w	4,delta
	dc.w	1,delta
	dc.w	3,-delta
	dc.w	1,-delta
	dc.w	3,delta
	dc.w	1,delta
	dc.w	3,-delta
	dc.w	1,-delta
	dc.w	3,delta
	dc.w	1,delta
	dc.w	4,delta
	dc.w	5,0 ; 0 = done

mtn_a	dc.w	5*delay-30+delayg, 0, -145+(4*50),mtny, 1024, delta, 28, 512/delta ; mtn_delay ... mtn_duration
	dc.w	4,delta
	dc.w	5,delta
	dc.w	1,delta
	dc.w	2,-delta
	dc.w	2,-delta
	dc.w	2,delta
	dc.w	1,delta
	dc.w	3,delta
	dc.w	3,-delta
	dc.w	1,-delta
	dc.w	3,delta
	dc.w	1,0 ; 0 = done

mtn_d	dc.w	4*delay+delayg, 0, -145+(5*50),mtny, 1024, delta, 28, 512/delta ; mtn_delay ... mtn_duration
	dc.w	4,delta
	dc.w	5,delta
	dc.w	4,delta
	dc.w	3,delta
	dc.w	1,delta
	dc.w	2,-delta
	dc.w	2,-delta
	dc.w	3,-delta
	dc.w	3,delta
	dc.w	1,0 ; 0 = done

mtn_p2	dc.w	8*delay+delayg, psize, -155+50,mtny+60, 1024, delta, 24, 512/delta ; mtn_delay ... mtn_duration
	dc.w	4,delta
	dc.w	3,delta
	dc.w	3,-delta
	dc.w	2,delta
	dc.w	1,delta
	dc.w	3,delta
	dc.w	3,-delta
	dc.w	1,-delta
	dc.w	3,delta
	dc.w	1,0 ; 0 = done

mtn_o	dc.w	3*delay+delayg, psize, -155+(2*50),mtny+60, 1024, delta, 28, 512/delta ; mtn_delay ... mtn_duration
	dc.w	4,delta
	dc.w	5,delta
	dc.w	4,delta
	dc.w	1,delta
	dc.w	3,-delta
	dc.w	3,-delta
	dc.w	2,-delta
	dc.w	2,delta
	dc.w	1,delta
	dc.w	3,0 ; 0 = done

mtn_i	dc.w	0*delay+20, psize, -155+(3*50),mtny+60, 1024, delta, 12, 512/delta ; mtn_delay ... mtn_duration
	dc.w	1,delta
	dc.w	5,delta
	dc.w	1,delta
	dc.w	5,0 ; 0 = done

mtn_n	dc.w	7*delay+delayg, psize, -155+(3*50+20),mtny+60, 1024, delta, 26, 512/delta ; mtn_delay ... mtn_duration
	dc.w	4,delta
	dc.w	5,delta
	dc.w	1,delta
	dc.w	4,-delta
	dc.w	2,-delta
	dc.w	4,delta
	dc.w	1,delta
	dc.w	5,0 ; 0 = done

mtn_t	dc.w	2*delay+delayg, psize, -155+(4*50+20),mtny+60, 1024, delta, 18, 512/delta ; mtn_delay ... mtn_duration
	dc.w	4,delta
	dc.w	5,delta
	dc.w	1,delta
	dc.w	4,-delta
	dc.w	3,delta
	dc.w	1,0 ; 0 = done


*------ PRINT NUMBER ----------------------------------------------------------*

; d0.l: number, d1.w: pos
	if numbers
printnumber
	move.l	v_db1a2a(a5),d6		;
	beq	.stop			;
	move.l	d6,a0			;
	add.w	d1,a0			;
 	moveq	#8-1,d7			; 8 digits
.loop	move.w	d0,d1			; number
	and.w	#$000f,d1		; mask digit out
	asl.w	#3,d1			; offset to font data
	lea	.digits(pc,d1.w),a1	;
	move.l	a0,a2			;
	rept 5
	move.b	(a1)+,(a2)		; print digit
	add.w	#pwidth,a2		; next line
	endr
	asr.l	#4,d0			; next digit
	subq.w	#1,a0			; next x position
	dbf	d7,.loop		;
.stop	rts				;

.digits	dc.b	%11111000	; 0
	dc.b	%10001000
	dc.b	%10001000
	dc.b	%10001000
	dc.b	%11111000
	ds.b	3

	dc.b	%00100000	; 1
	dc.b	%00100000
	dc.b	%00100000
	dc.b	%00100000
	dc.b	%00100000
	ds.b	3

	dc.b	%11111000	; 2
	dc.b	%00001000
	dc.b	%11111000
	dc.b	%10000000
	dc.b	%11111000
	ds.b	3

	dc.b	%11111000	; 3
	dc.b	%00001000
	dc.b	%11111000
	dc.b	%00001000
	dc.b	%11111000
	ds.b	3

	dc.b	%10001000	; 4
	dc.b	%10001000
	dc.b	%11111000
	dc.b	%00001000
	dc.b	%00001000
	ds.b	3

	dc.b	%11111000	; 5
	dc.b	%10000000
	dc.b	%11111000
	dc.b	%00001000
	dc.b	%11111000
	ds.b	3

	dc.b	%11111000	; 6
	dc.b	%10000000
	dc.b	%11111000
	dc.b	%10001000
	dc.b	%11111000
	ds.b	3

	dc.b	%11111000	; 7
	dc.b	%00001000
	dc.b	%00010000
	dc.b	%00100000
	dc.b	%00100000
	ds.b	3

	dc.b	%11111000	; 8
	dc.b	%10001000
	dc.b	%11111000
	dc.b	%10001000
	dc.b	%11111000
	ds.b	3

	dc.b	%11111000	; 9
	dc.b	%10001000
	dc.b	%11111000
	dc.b	%00001000
	dc.b	%11111000
	ds.b	3

	dc.b	%11111000	; A
	dc.b	%10001000
	dc.b	%11111000
	dc.b	%10001000
	dc.b	%10001000
	ds.b	3

	dc.b	%11110000	; B
	dc.b	%10001000
	dc.b	%11110000
	dc.b	%10001000
	dc.b	%11110000
	ds.b	3

	dc.b	%11111000	; C
	dc.b	%10000000
	dc.b	%10000000
	dc.b	%10000000
	dc.b	%11111000
	ds.b	3

	dc.b	%11110000	; D
	dc.b	%10001000
	dc.b	%10001000
	dc.b	%10001000
	dc.b	%11110000
	ds.b	3

	dc.b	%11111000	; E
	dc.b	%10000000
	dc.b	%11111000
	dc.b	%10000000
	dc.b	%11111000
	ds.b	3

	dc.b	%11111000	; F
	dc.b	%10000000
	dc.b	%11111000
	dc.b	%10000000
	dc.b	%10000000
	ds.b	3

	endif


*------	WAIT BLITTER ----------------------------------------------------------*

waitblitter
	move.w	#SET+DMAEN+BLTPRI,$96(a6)	;
	btst	#14-8,$02(a6)			; DMAB_BLTDONE = 14
.wait	btst	#14-8,$02(a6)			;
	bne	.wait				;
	move.w	#BLTPRI,$96(a6)			;
	rts					;


*------	WAIT RASTER -----------------------------------------------------------*

waitraster
.wait	move.l	$04(a6),d0			;
	and.l	#$0001ff00,d0			;
	cmp.l	#312<<8,d0			; line to wait for
	bne	.wait				;
	rts					;


*------	BACKGROUND TASKS ------------------------------------------------------*

backgroundtasks
.idle	
	CHECKACTOR actor_prepare_4fx
	beq	.txt				;

	bsr	precalcrays			;
	tst.b	v_doquit(a5)			;	
	bne	.quit				;
	bsr	generaterings			;
	tst.b	v_doquit(a5)			;	
	bne	.quit				;

	move.w	#-128,v_a(a5)			; LMS  tilt around x axis
	move.l	b_lmsanimation(pc),a2		; destination
	move.w	#numanimsteps-1,d7		;
.loop	move.l	d7,-(a7)			;
	bsr	mtx				; (a2 is untouched)
	move.l	b_lmsvertices(pc),a3		; vertices source
	moveq	#numvertices-1,d7		;
	bsr	applymtx			;
	move.l	(a7)+,d7			;
	subq.w	#2048/numanimsteps,v_b(a5)	;
	if timing
	move.w	d7,$dff180			;
	endif
	tst.b	v_doquit(a5)			;	
	bne	.quit				;
	dbf	d7,.loop			;

	STOPACTOR actor_prepare_4fx

.txt	CHECKACTOR actor_prepare_txt
	beq	.done				;

	move.l	v_txtbuffer(a5),a0		;
	move.w	#numtxtlines*txtpwidth/4-1,d7	;
.clear	rept lineheight
	clr.l	(a0)+				;
	endr
	dbf	d7,.clear			;

	lea	text(pc),a0			;
	move.l	a0,v_txtpointer(a5)		;
	bsr	generatetext			;

	STOPACTOR actor_prepare_txt

	if testing
	move.w	v_frame(a5),v_number(a5)	;
	endif

.done	tst.b	v_doquit(a5)			;
	beq	.idle				;
.quit	move.w	#$0200,$100(a6)			; avoid flickering?
	rts					;


*------	IRQ3 ------------------------------------------------------------------*

irq3	movem.l	d0-a6,-(a7)			;
	move.l	b_vars(pc),a5			;
	lea	custom,a6			;

	CHECKACTOR actor_player
	beq	.srf				;
	bsr	play				;
.srf	CHECKACTOR actor_srf
	beq	.mtn				;
	bsr	srfeffect			;
	bra	.eofx				;
.mtn	CHECKACTOR actor_mtn
	beq	.lms				;
	bsr	mnemotroneffect			;
	bra	.eofx				;
.lms	CHECKACTOR actor_lms
	beq	.txt				;
	bsr	lmseffect			;
	bra	.eofx				;
.txt	CHECKACTOR actor_txt
	beq	.ladd				;
	bsr	txteffect			;
	bra	.eofx				;
.ladd	CHECKACTOR actor_ladd
	beq	.eofx
	bsr	laddfading			;
	bra	.nodb				; Ladd does not use double buffering

.eofx	movem.l	v_db1a2a(a5),d0/d1		; double buffering
	exg	d0,d1				; v_db1a2a <-> v_db1b2b
	movem.l	d0/d1,v_db1a2a(a5)		;

.nodb
	if timing|numbers
	bsr	waitblitter			;
	endif

	if timing
	move.w	#$0440,$180(a6)			; dark yellow color indicates numbers consumption
	endif

	if numbers
	moveq	#0,d0				; value
	move.w	v_frame(a5),d0			; value
	asl.l	#8,d0				;
	move.b	v_waitcount(a5),d0		;
	asl.l	#8,d0				;
	move.b	v_wait(a5),d0			;
	moveq	#8-1,d1				; pos
	bsr	printnumber			;

	if instruments
	move.l	v_instlow(a5),d0		;
	move.w	#10*pwidth+8-1,d1		; pos
	bsr	printnumber			;

	move.l	v_ninst0(a5),d0			;
	move.w	#20*pwidth+8-1,d1		; pos
	bsr	printnumber			;

	move.l	v_ninst2(a5),d0			;
	move.w	#30*pwidth+8-1,d1		; pos
	bsr	printnumber			;
	else
	move.l	v_number(a5),d0			; value
	move.w	#10*pwidth+8-1,d1		; pos
	bsr	printnumber			;
	endif
	endif

	addq.w	#1,v_frame(a5)			; advance frame number

	btst	#6,$bfe001			; left mouse button pressed?
	bne	.noquit				; (do NOT use seq)
	st	v_doquit(a5)			;
.noquit
	if timing
	move.w	#$0030,$180(a6)			; dark green color indicates free capacity
	endif

	moveq	#$0020,d0			; delete vertb request
	move.w	d0,$9c(a6)			; 3 times? https://amycoders.org/tutorials/frametime.html
	move.w	d0,$9c(a6)			;
	move.w	d0,$9c(a6)			;

	movem.l	(a7)+,d0-a6			;
	rte					;


*------	LINDENMAYER SYSTEM ----------------------------------------------------*

lmsangle	equ	200			; 512 = 90 deg

lmsinit	move.l	b_lmsvertices(pc),a2		;
	moveq	#-7,d0				; x
	move.w	#9<<8,d1			; y
	move.w	d0,(a2)+			; root vertex
	move.w	d1,(a2)+			;
	move.w	#1024+16,d2			; angle (up) tilt a bit

	move.l	v_db1b2b(a5),a3			; use bitplanes stack
	add.w	#2*psize,a3			; go to "top" of stack

	lea	lmsfns(pc),a4			; functions

	lea	axiom(pc),a0			; F[+F]F[-F][F]
	move.w	#axiomend-axiom-1,d7		;
.loop	moveq	#0,d5				;
	move.b	(a0)+,d5			; function
	move.l	(a4,d5.l),a1			;
	jsr	(a1)				; call function
	dbf	d7,.loop			;

	move.l	b_lmsvertices(pc),a1		;
.scale	move.w	(a1),d0				; x
	asr.w	#4,d0				;
	move.w	d0,(a1)+			;
	move.w	(a1),d0				; y
	asr.w	#4,d0				;
	move.w	d0,(a1)+			;
	cmp.l	a1,a2				;
	bne	.scale				;
	rts					;

lmsforward
	move.l	b_sintab(pc),a1			; sin
	and.w	#$07fe,d2			;

	move.w	(a1,d2.w),d3			; sin angle
	add.w	d3,d0				; x' (scaled 256)

	add.w	#512,a1				;
	move.w	(a1,d2.w),d4			; cos angle
	add.w	d4,d1				; y' (scaled 256)

	move.l	b_lmsvertices(pc),a1		; add vertex
.search	movem.w	(a1)+,d3/d4			; loop vertices in list
	cmp.w	d0,d3				;
	bne	.next				;
	cmp.w	d1,d4				;
	beq	.contained			; contained in list already
.next	cmp.l	a1,a2				;
	bne	.search				;

	move.w	d0,(a2)+			; not contained - add vertex
	move.w	d1,(a2)+			;
.contained
	rts					;

lmspush	movem.w	d0-d2,-(a3)			;
	rts					;

lmspop	movem.w	(a3)+,d0-d2			;
	rts					;

lmsleft	add.w	#lmsangle,d2			;
	rts					;

lmsright
	sub.w	#lmsangle,d2			;
	rts					;


*------	LINDENMAYER SYSTEM LINES ----------------------------------------------*

f	equ	2	; word offset
lmslines
	dc.w	0*f,1*f, 1*f,2*f, 1*f,3*f, 3*f,4*f, 3*f,6*f
	dc.w	6*f,7*f, 6*f,8*f, 8*f,10*f, 8*f,9*f
	dc.w	3*f,5*f, 5*f,11*f, 5*f,12*f

	dc.w	12*f,13*f, 13*f,15*f, 13*f,16*f, 16*f,18*f, 16*f,17*f

	dc.w	12*f,23*f, 23*f,24*f, 23*f,25*f, 25*f,26*f
	dc.w	25*f,28*f, 28*f,29*f, 28*f,30*f, 30*f,32*f, 30*f,31*f
	dc.w	25*f,27*f, 27*f,33*f, 27*f,34*f, 34*f,35*f, 35*f,37*f
	dc.w	35*f,38*f, 38*f,40*f, 38*f,39*f
	dc.w	34*f,36*f, 36*f,41*f, 36*f,42*f, 42*f,44*f, 42*f,43*f

	dc.w	12*f,14*f, 14*f,19*f, 14*f,20*f, 20*f,21*f
	dc.w	20*f,45*f, 45*f,46*f, 45*f,47*f, 47*f,49*f, 47*f,48*f

	dc.w	20*f,22*f, 22*f,50*f, 22*f,51*f

	dc.w	51*f,52*f, 52*f,54*f, 52*f,55*f, 55*f,56*f
	dc.w	55*f,62*f, 62*f,63*f, 62*f,64*f, 64*f,66*f, 64*f,65*f
	
	dc.w	55*f,57*f
	dc.w	57*f,67*f, 57*f,68*f, 68*f,69*f
	dc.w	69*f,71*f, 69*f,72*f, 72*f,74*f, 72*f,73*f
	dc.w	68*f,70*f, 70*f,75*f, 70*f,76*f, 76*f,78*f, 76*f,77*f

	dc.w	51*f,53*f, 53*f,58*f, 53*f,59*f, 59*f,60*f

	dc.w	59*f,79*f, 79*f,80*f, 79*f,81*f, 81*f,83*f, 81*f,82*f

	dc.w	59*f,61*f, 61*f,84*f, 61*f,85*f

	dc.w	85*f,86*f, 86*f,88*f, 86*f,89*f, 89*f,91*f, 89*f,90*f
	
	dc.w	85*f,87*f, 87*f,92*f, 87*f,93*f, 93*f,95*f, 93*f,94*f
lmslinesend


*------	LINDENMAYER SYSTEM COLORS ---------------------------------------------*

lmscolors
	dc.w	$0aaf,$0bbf,$0ccf,$0ddf
lmscolorlast
	dc.w	$0eef


*------	LINDENMAYER SYSTEM AXIOM ----------------------------------------------*

lmsfw	equ	0*4	; forward
lmsps	equ	1*4	; push
lmspp	equ	2*4	; pop
lmslt	equ	3*4	; left
lmsrt	equ	4*4	; right

axiom	dc.b	lmsfw,lmsps,lmsrt,lmsfw,lmspp,lmsfw,lmsps,lmslt
	dc.b	lmsfw,lmspp,lmsps,lmsfw,lmspp,lmsps,lmsrt,lmsfw
	dc.b	lmsps,lmsrt,lmsfw,lmspp,lmsfw,lmsps,lmslt,lmsfw
	dc.b	lmspp,lmsps,lmsfw,lmspp,lmspp,lmsfw,lmsps,lmsrt
	dc.b	lmsfw,lmspp,lmsfw,lmsps,lmslt,lmsfw,lmspp,lmsps
	dc.b	lmsfw,lmspp,lmsps,lmslt,lmsfw,lmsps,lmsrt,lmsfw
	dc.b	lmspp,lmsfw,lmsps,lmslt,lmsfw,lmspp,lmsps,lmsfw
	dc.b	lmspp,lmspp,lmsps,lmsfw,lmsps,lmsrt,lmsfw,lmspp
	dc.b	lmsfw,lmsps,lmslt,lmsfw,lmspp,lmsps,lmsfw,lmspp
	dc.b	lmspp,lmsps,lmsrt,lmsfw,lmsps,lmsrt,lmsfw,lmspp
	dc.b	lmsfw,lmsps,lmslt,lmsfw,lmspp,lmsps,lmsfw,lmspp
	dc.b	lmsps,lmsrt,lmsfw,lmsps,lmsrt,lmsfw,lmspp,lmsfw
	dc.b	lmsps,lmslt,lmsfw,lmspp,lmsps,lmsfw,lmspp,lmspp
	dc.b	lmsfw,lmsps,lmsrt,lmsfw,lmspp,lmsfw,lmsps,lmslt
	dc.b	lmsfw,lmspp,lmsps,lmsfw,lmspp,lmsps,lmslt,lmsfw
	dc.b	lmsps,lmsrt,lmsfw,lmspp,lmsfw,lmsps,lmslt,lmsfw
	dc.b	lmspp,lmsps,lmsfw,lmspp,lmspp,lmsps,lmsfw,lmsps
	dc.b	lmsrt,lmsfw,lmspp,lmsfw,lmsps,lmslt,lmsfw,lmspp
	dc.b	lmsps,lmsfw,lmspp,lmspp,lmspp,lmsfw,lmsps,lmsrt
	dc.b	lmsfw,lmspp,lmsfw,lmsps,lmslt,lmsfw,lmspp,lmsps
	dc.b	lmsfw,lmspp,lmsps,lmsrt,lmsfw,lmsps,lmsrt,lmsfw
	dc.b	lmspp,lmsfw,lmsps,lmslt,lmsfw,lmspp,lmsps,lmsfw
	dc.b	lmspp,lmspp,lmsfw,lmsps,lmsrt,lmsfw,lmspp,lmsfw
	dc.b	lmsps,lmslt,lmsfw,lmspp,lmsps,lmsfw,lmspp,lmsps
	dc.b	lmslt,lmsfw,lmsps,lmsrt,lmsfw,lmspp,lmsfw,lmsps
	dc.b	lmslt,lmsfw,lmspp,lmsps,lmsfw,lmspp,lmspp,lmsps
	dc.b	lmsfw,lmsps,lmsrt,lmsfw,lmspp,lmsfw,lmsps,lmslt
	dc.b	lmsfw,lmspp,lmsps,lmsfw,lmspp,lmspp,lmsps,lmslt
	dc.b	lmsfw,lmsps,lmsrt,lmsfw,lmspp,lmsfw,lmsps,lmslt
	dc.b	lmsfw,lmspp,lmsps,lmsfw,lmspp,lmsps,lmsrt,lmsfw
	dc.b	lmsps,lmsrt,lmsfw,lmspp,lmsfw,lmsps,lmslt,lmsfw
	dc.b	lmspp,lmsps,lmsfw,lmspp,lmspp,lmsfw,lmsps,lmsrt
	dc.b	lmsfw,lmspp,lmsfw,lmsps,lmslt,lmsfw,lmspp,lmsps
	dc.b	lmsfw,lmspp,lmsps,lmslt,lmsfw,lmsps,lmsrt,lmsfw
	dc.b	lmspp,lmsfw,lmsps,lmslt,lmsfw,lmspp,lmsps,lmsfw
	dc.b	lmspp,lmspp,lmsps,lmsfw,lmsps,lmsrt,lmsfw,lmspp
	dc.b	lmsfw,lmsps,lmslt,lmsfw,lmspp,lmsps,lmsfw,lmspp
	dc.b	lmspp,lmspp,lmsps,lmsfw,lmsps,lmsrt,lmsfw,lmspp
	dc.b	lmsfw,lmsps,lmslt,lmsfw,lmspp,lmsps,lmsfw,lmspp
	dc.b	lmsps,lmsrt,lmsfw,lmsps,lmsrt,lmsfw,lmspp,lmsfw
	dc.b	lmsps,lmslt,lmsfw,lmspp,lmsps,lmsfw,lmspp,lmspp
	dc.b	lmsfw,lmsps,lmsrt,lmsfw,lmspp,lmsfw,lmsps,lmslt
	dc.b	lmsfw,lmspp,lmsps,lmsfw,lmspp,lmsps,lmslt,lmsfw
	dc.b	lmsps,lmsrt,lmsfw,lmspp,lmsfw,lmsps,lmslt,lmsfw
	dc.b	lmspp,lmsps,lmsfw,lmspp,lmspp,lmsps,lmsfw,lmsps
	dc.b	lmsrt,lmsfw,lmspp,lmsfw,lmsps,lmslt,lmsfw,lmspp
	dc.b	lmsps,lmsfw,lmspp,lmspp,lmspp
axiomend
	even


*------	SRF EFFECT ------------------------------------------------------------*

srfeffect
	move.l	b_clist1(pc),a1			;
	add.w	#clist1bpl-clist1+2,a1		;
	move.l	v_db1a2a(a5),d0			; active bitplanes
	move.w	d0,4(a1)			;
	swap	d0				;
	move.w	d0,(a1)				;
	swap	d0				;
	add.l	#psize,d0			;
	move.w	d0,12(a1)			;
	swap	d0				;
	move.w	d0,8(a1)			;

	bsr	cls				;
	bsr	processinstruments		;

	lea	$52(a6),a6			;
	bsr	srfdraw				;
	bsr	fillrays			;
	lea	-$52(a6),a6			; a6 becomes $dff000 again
	rts					;


*------	MNEMOTRON EFFECT ------------------------------------------------------*

mnemotroneffect
	move.l	b_clist2(pc),a1			;
	add.w	#clist2bpl-clist2+2,a1		;
	move.l	v_bitplane(a5),d0		; static bitplane
	move.w	d0,4(a1)			;
	swap	d0				;
	move.w	d0,(a1)				;
	move.l	v_db1a2a(a5),d0			; active bitplanes
	move.w	d0,12(a1)			;
	swap	d0				;
	move.w	d0,8(a1)			;
	swap	d0				;
	add.l	#psize,d0			;
	move.w	d0,20(a1)			;
	swap	d0				;
	move.w	d0,16(a1)			;

	bsr	cls				;

	lea	$52(a6),a6			;
	bsr	mtndraw				;
	lea	-$52(a6),a6			; a6 becomes $dff000 again
	rts					;


*------	LINDENMAYER SYSTEM EFFECT ---------------------------------------------*

lmseffect
	move.l	b_clist3(pc),a1			;
	add.w	#clist3bpl-clist3+2,a1		;
	move.l	v_db1a2a(a5),d0			; active bitplanes
	move.w	d0,4(a1)			; same bitplane overlaps
	move.w	d0,12(a1)			;
	swap	d0				;
	move.w	d0,(a1)				;
	move.w	d0,8(a1)			;

	bsr	cls				;

	CHECKACTOR actor_lmsup
	beq	.notup				;

	addq.w	#1,v_lmsnumlines(a5)		;
	cmp.w	#numlmslines,v_lmsnumlines(a5)	;
	bne	.notup				;
	STOPACTOR actor_lmsup
.notup	
	CHECKACTOR actor_lmsdown
	beq	.notdown			;
	subq.w	#1,v_lmsnumlines(a5)		;
	tst.w	v_lmsnumlines(a5)		;
	bne	.notdown			;
	STOPACTOR actor_lmsdown
	STARTACTOR actor_prepare_txt

.notdown
	move.l	b_lspbank(pc),a0		; random data
	moveq	#0,d0				;
	move.b	v_wait(a5),d0			; use this as counter
	add.w	d0,a0				;

	move.l	b_clist3(pc),a1			;
	add.w	#clist3rl-clist3+6,a1		;

	lea	cosmics(pc),a2			;
	move.w	v_cosmicsindex(a5),d4		;
	move.b	(a2,d4.w),d3			; All I wish for Xmas is movem.b
	move.b	1(a2,d4.w),d4			;

	move.w	#numrasterlines-1,d7		;
.looprl	move.b	(a0)+,d1			;
;	move.b	d7,d1				; TESTING
	and.w	#$000f,d1			;
	move.w	d1,d2				; used below
	or.b	#$70,d1				; "center" (see moveq #-7 at init)
	
	cmp.b	d3,d2				; left side
	bge	.good				;
	bra	.restr				;
.good	cmp.b	d4,d2				; right side
	ble	.good2				;
.restr	moveq	#$77,d1				; no cosmic rain
.good2	move.b	d1,1(a1)			;
	addq.b	#1,d0				; next line

	add.w	d2,d2				;
	move.w	.colors(pc,d2.w),4(a1)		;
;	move.w	#$0f36,4(a1)			; TESTING
	add.w	#12,a1				;
	dbf	d7,.looprl			;

	move.w	v_cosmicsidxs(a5),d0		;
	cmp.w	v_cosmicsindex(a5),d0 		;
	beq	.done				;
	move.w	v_cosmicsidxd(a5),d0		;
	add.w	d0,v_cosmicsindex(a5)		;
.done	bsr	processinstruments		;

	lea	$52(a6),a6			;
	bsr	lmsdraw				;
	lea	-$52(a6),a6			; a6 becomes $dff000 again
	rts					;

.colors	dc.w	$0605	; 0
	dc.w	$0805	; 1
	dc.w	$0a05	; 2
	dc.w	$0c05	; 3

	dc.w	$0d16	; 4
	dc.w	$0e26	; 5
	dc.w	$0f36	; 6
	dc.w	$0f36	; 7

	dc.w	$0f36	; 8 middle
	dc.w	$0f36	; 9
	dc.w	$0f36	; 10
	dc.w	$0e26	; 11

	dc.w	$0d15	; 12
	dc.w	$0c05	; 13
	dc.w	$0a05	; 14
	dc.w	$0805	; 15

cosmics	dc.b	7,7, 7,8, 7,8, 6,8, 6,8, 6,9, 6,9
	dc.b	5,9, 5,9, 5,10, 5,10, 4,10, 4,10
	dc.b	4,11, 4,11, 3,11, 3,11, 3,12, 3,12
	dc.b	2,12, 2,12, 2,13, 2,13, 1,13, 1,13
	dc.b	1,14, 1,14, 0,14, 0,14, 0,15
cosmicsend


*------	TEXT EFFECT -----------------------------------------------------------*

hscrollspeed	equ	1
txtnumlines	equ	11

txteffect
	move.l	b_clist4(pc),a1			;
	add.w	#clist4bpl4-clist4+2,a1		;
	move.l	v_db1a2a(a5),d0			; active bitplanes
	move.w	d0,4(a1)			;
	swap	d0				;
	move.w	d0,0(a1)			;

	move.w	v_txtcolindex(a5),d0		;
	lea	cols(pc),a0			;
	lea	$1a2(a6),a1			;
	moveq	#15-1,d7			;
	
.colors	move.w	(a0,d0.w),(a1)+			;
	addq.w	#2,d0				; next color
	cmp.w	#colsend-cols,d0		;
	bne	.col1				;
	moveq	#0,d0				;
.col1	dbf	d7,.colors			;

	btst	#0,v_frame+1(a5)		; only any other frame
	beq	.col2				;
	addq.w	#2,v_txtcolindex(a5)		; next color
	cmp.w	#colsend-cols,v_txtcolindex(a5) ;
	bne	.col2				;
	clr.w	v_txtcolindex(a5)		;
.col2
	moveq	#0,d6				; y line pos
	moveq	#txtnumlines-1,d7		;
	lea	v_txtlines(a5),a4		;
	add.w	v_txtline(a5),a4		;
.lines	move.l	txt_data(a4),a0			; source

	move.l	v_db1b2b(a5),a1			; destination
	subq.l	#2,a1				; compensate 16bit blitter shift
	sub.w	v_txty(a5),a1			; vertical scrolling
	add.l	d6,a1				; horizontal scrolling (line offset)

	bsr	waitblitter			;
	move.l	#(txtpwidth-(pwidth/2)-2)<<16+((pwidth/2)-2),$64(a6) ; modulo A D
	moveq	#0,d0				; 16 bit horizontal fine scrolling
	move.w	txt_xpos(a4),d0			;
	move.l	d0,d1				;
	asr.w	#3,d1				;
	add.w	d1,a0				;
	moveq	#$f,d1				;
	and.l	d1,d0				;
	sub.l	d0,d1				;
	ror.l	#4,d1				;
	or.l	#$09f00000,d1			;
	move.l	d1,$40(a6)			;
	move.l	a0,$50(a6)			; source A
	move.l	a1,$54(a6)			; destination D
	moveq	#-1,d0				;
	move.l	d0,$44(a6)			; first/last word mask
	move.w	#lineheight<<6+((pwidth/2)+2)>>1,$58(a6) ;

	addq.w	#hscrollspeed,txt_xpos(a4)	;
	addq.w	#sizeoftxt,a4			; next text line
	add.w	#lineheight*pwidth,d6		;
	dbf	d7,.lines			;

	add.w	#pwidth,v_txty(a5)		;
	cmp.w	#lineheight*pwidth,v_txty(a5)	;
	bne	.ygood				;
	clr.w	v_txty(a5)			;
	addq.w	#sizeoftxt,v_txtline(a5)	;
.ygood	bsr	waitblitter			;

	move.l	v_db1b2b(a5),a0			; horizontal mirror
	move.l	a0,a1				;
	moveq	#0,d0				;
	moveq	#128-1,d7			; height
.loop
index set 39
	rept	pwidth/2
	move.b	(a0)+,d0			;
	move.b	v_txtmirror(a5,d0.w),index(a1)	;
index set index-1 
	endr
	add.w	#pwidth/2,a0			;
	add.w	#pwidth,a1			;
	dbf	d7,.loop			;
	rts					;

cols	dc.w	$0f36
	dc.w	$0e36
	dc.w	$0d35
	dc.w	$0c35
	dc.w	$0b24
	dc.w	$0a24
	dc.w	$0923
	dc.w	$0823
	dc.w	$0712
	dc.w	$0612
	dc.w	$0511
	dc.w	$0411
	dc.w	$0301
	dc.w	$0201
	
	dc.w	$0301
	dc.w	$0401
	dc.w	$0511
	dc.w	$0612
	dc.w	$0712
	dc.w	$0823
	dc.w	$0923
	dc.w	$0a24
	dc.w	$0b24
	dc.w	$0c35
	dc.w	$0d35
	dc.w	$0e36
colsend


*------	GENERATE TXT RINGS ----------------------------------------------------*

numringplanes	equ	4
ringplaneheight	equ	128

generaterings
	move.l	b_ringplanes(pc),a0		;
	lea	plan(pc),a2			;
	bsr	gencircles			;

	move.l	b_ringplanes(pc),a0		;
	add.w	#1*ringplaneheight*pwidth,a0	;
	lea	plan2(pc),a2			;
	bsr	gencircles			;

	move.l	b_ringplanes(pc),a0		;
	add.w	#2*ringplaneheight*pwidth,a0	;
	lea	plan3(pc),a2			;
	bsr	gencircles			;

	move.l	b_ringplanes(pc),a0		;
	add.w	#3*ringplaneheight*pwidth,a0	;
	lea	plan4(pc),a2			;
	bsr	gencircles			;
	rts					;

; a0: bitplane a2: plan
gencircles
	moveq	#-127,d1			; y
	moveq	#7,d4				; bit to set
.yloop	move.l	#-160,d0			; x
.xloop	move.l	d0,d2				; copy of x
	muls	d2,d2				; x² (hint: muls LUT isn't faster)
	move.l	d1,d3				; copy of y
	muls	d3,d3				; y²
	add.l	d2,d3				; x² + y²

	move.l	a2,a1				;
	move.w	(a1)+,d7			; num rings
.rings	movem.l	(a1)+,d5/d6			;
	cmp.l	d5,d3				; x² + y² <= r²
	bge	.next				;
	cmp.l	d6,d3				;
	blt	.next				;
	bset	d4,(a0)				;
	bra	.done				;
.next	dbf	d7,.rings			;
	
.done	addq.l	#1,d0				; advance x
	subq.b	#1,d4				;
	bpl	.stay				;
	moveq	#7,d4				; advance to next byte
	addq.l	#1,a0				;
.stay	;move.w	d3,$dff186			; flashing cursor
	cmp.w	#-160+320,d0			;
	bne	.xloop				;

	tst.b	v_doquit(a5)			;
	bne	.quit				;

	addq.l	#1,d1				; advance y
	cmp.w	#-127+ringplaneheight,d1	;
	bne	.yloop				;
.quit	rts					;

r	equ	14

plan	dc.w	(p1end-p1)/8-1
p1	dc.l	r*15*r*15,r*14*r*14
	dc.l	r*13*r*13,r*12*r*12
	dc.l	r*11*r*11,r*10*r*10
	dc.l	r*9*r*9,r*8*r*8
	dc.l	r*7*r*7,r*6*r*6
	dc.l	r*5*r*5,r*4*r*4
	dc.l	r*3*r*3,r*2*r*2
	dc.l	r*r,0*0
p1end

plan2	dc.w	(p2end-p2)/8-1
p2	dc.l	r*14*r*14,r*12*r*12
	dc.l	r*10*r*10,r*8*r*8
	dc.l	r*6*r*6,r*4*r*4
	dc.l	r*2*r*2,0*0
p2end

plan3	dc.w	(p3end-p3)/8-1
p3	dc.l	r*12*r*12,r*8*r*8
	dc.l	r*4*r*4,0*0
p3end

plan4	dc.w	(p4end-p4)/8-1
p4	dc.l	r*20*r*20,r*16*r*16
	dc.l	r*8*r*8,0*0
p4end


*------	THE LADD COMPANY EFFECT -----------------------------------------------*

generateladd
	lea	ladddata(pc),a0			;
	move.l	v_db1b2b(a5),a1			; use this as static buffer
	sub.w	#2*pwidth,a1			;
.newline
	add.w	#2*pwidth,a1			;
.line	moveq	#0,d0				;
	moveq	#0,d1				;
	move.b	(a0)+,d0			;
	move.b	(a0),d1				;
	beq	.done				; end of data?

	tst.b	d0				; new line?
	beq	.newline			;

	addq.w	#1,a0				; skip x2

	add.w	#18,d0				; adjust x1 data
	add.w	#18,d1				; adjust x2 data

	cmp.w	d1,d0				;
	blt	.good				;
	exg	d0,d1				;
.good	move.w	d1,d7				;
	sub.w	d0,d7				; segment length
.segment
	moveq	#7,d1				;
	sub.w	d0,d1				;
	move.w	d0,d2				; "x-copy" ;-)
	asr.w	#3,d2				; allow neg x coords
	lea	(a1,d2.w),a2			;
	bset	d1,(a2)				;

	addq.w	#1,d0				; x++
	dbf	d7,.segment			;
	bra	.line				;

.done	rts					;

; Data taken from https://github.com/DanielHansen-IMD/apple-ii-hgr-grid
; X @danielbhansen
ladddata
	dc.b	130,136,139,139,147,147,153,155,158,163,0
	dc.b	127,141,144,148,151,157,159,172,0
	dc.b	171,126,116,114,0
	dc.b	180,179,176,126,122,122,120,112,109,108,0
	dc.b	103,105,107,122,124,124,126,130,132,132,134,138,140,140,142,160,162,184,0
	dc.b	99,120,125,135,140,140,142,161,163,172,174,187,0
	dc.b	193,193,190,189,187,178,176,176,174,174,172,164,162,142,140,139,135,130,127,125,122,122,119,117,112,111,109,108,105,99,0
	dc.b	190,190,187,180,177,170,168,162,160,135,133,133,131,131,127,127,122,122,119,119,117,117,111,111,109,109,107,107,103,98,0
	dc.b	97,104,106,110,112,113,115,116,118,119,121,123,127,135,137,176,179,185,187,188,191,191,194,194,0
	dc.b	87,88,90,90,96,116,119,120,123,123,127,149,151,175,178,178,180,186,188,189,191,195,0
	dc.b	197,183,179,152,150,124,121,120,116,112,108,97,92,84,0
	dc.b	201,189,186,183,180,155,153,152,150,144,142,117,115,111,109,109,107,82,0
	dc.b	81,99,102,108,111,135,138,140,142,142,144,144,147,147,150,157,159,161,164,181,184,184,188,188,192,203,0
	dc.b	81,85,87,109,112,141,144,145,147,147,149,154,157,180,183,183,185,191,193,196,198,202,0
	dc.b	207,204,200,196,193,187,184,184,181,175,172,171,169,166,163,149,147,147,145,145,143,140,138,114,112,80,0
	dc.b	212,196,192,189,186,186,182,173,171,154,152,148,146,145,143,124,122,108,106,88,86,80,78,75,0
	dc.b	72,73,75,86,88,98,100,103,105,105,107,109,112,114,118,119,123,126,128,131,134,134,136,140,143,151,154,158,160,181,183,192,194,211,213,213,0
	dc.b	70,73,75,101,103,108,112,118,121,127,129,133,135,142,145,147,150,151,154,212,214,215,0
	dc.b	218,209,206,164,162,159,157,155,153,153,121,149,146,146,143,142,140,128,125,121,118,111,108,108,105,83,81,68,0
	dc.b	220,219,216,210,206,162,160,155,153,148,146,128,125,75,72,67,0
	dc.b	63,78,83,97,100,126,128,128,131,132,134,148,151,153,156,160,163,208,212,218,220,223,0
	dc.b	62,62,64,66,69,69,71,77,81,95,98,99,103,129,131,159,161,161,163,168,171,206,209,228,0
	dc.b	229,203,201,196,193,185,182,180,177,176,174,163,158,145,143,141,138,133,131,122,120,119,117,104,101,97,92,60,0
	dc.b	230,209,207,204,201,191,187,184,181,172,170,162,159,153,151,142,139,133,130,128,125,122,118,111,109,101,98,98,95,95,92,61,0
	dc.b	61,84,86,115,118,124,128,128,130,139,142,142,145,146,149,152,155,158,160,167,170,171,174,180,182,204,210,219,222,222,225,226,228,228,230,232,0
	dc.b	62,64,67,87,91,92,95,116,118,119,122,126,129,146,150,154,156,157,161,173,176,180,183,183,188,200,203,205,212,221,225,225,227,227,0
	dc.b	231,226,222,216,213,213,211,211,207,206,204,198,195,190,186,177,174,160,156,153,151,150,145,131,129,124,119,114,112,105,103,97,89,68,62,60,0
	dc.b	232,223,221,219,216,213,210,191,188,154,150,150,146,131,128,122,120,116,114,111,109,104,101,100,98,96,94,88,86,81,77,63,0
	dc.b	65,65,67,77,79,86,88,99,102,113,115,129,132,140,143,146,149,150,152,159,161,162,165,173,176,177,179,198,201,201,204,209,212,212,215,216,220,228,230,233,0
	dc.b	61,68,70,71,77,82,85,85,87,114,117,118,121,121,123,146,149,162,165,168,172,172,176,177,180,199,201,202,204,206,216,229,232,232,0
	dc.b	229,225,222,222,219,219,209,208,205,205,201,173,171,171,165,158,156,154,152,148,146,125,122,122,117,117,115,84,82,81,78,71,68,61,59,59,0
	dc.b	229,226,211,207,204,204,201,170,164,163,154,153,151,137,133,133,129,128,123,122,118,117,115,80,78,75,72,60,0
	dc.b	61,69,76,110,113,124,128,129,133,133,137,141,143,154,158,158,162,163,169,202,204,211,213,214,0
	dc.b	62,65,68,98,103,125,129,130,133,133,138,140,144,145,147,179,181,183,186,209,212,215,0
	dc.b	213,180,178,157,154,148,145,143,140,139,134,134,131,130,127,124,122,105,100,67,65,63,0
	dc.b	215,207,205,194,192,181,179,173,168,164,161,157,153,152,150,147,144,143,141,139,135,135,132,130,124,122,119,117,114,106,103,98,94,65,0
	dc.b	50,51,54,57,65,120,124,128,131,132,135,135,139,144,147,148,152,153,156,159,161,161,164,170,172,177,180,183,186,186,189,200,202,207,209,210,213,217,0
	dc.b	48,50,52,56,60,60,63,72,75,80,85,85,88,88,90,94,96,101,104,118,120,121,125,128,130,133,135,136,138,141,147,149,152,157,160,163,165,165,170,174,177,188,190,198,200,200,203,210,216,217,0
	dc.b	211,203,198,194,192,189,187,180,178,174,171,171,164,164,160,153,148,143,141,132,130,125,122,122,120,105,102,95,93,90,86,83,79,75,70,64,62,59,56,52,50,47,0
	dc.b	221,218,209,209,199,198,195,195,189,181,179,175,172,172,163,162,156,151,147,143,141,137,134,117,114,112,110,88,85,85,83,83,81,81,78,90,71,65,62,49,46,46,0
	dc.b	42,42,44,50,55,59,61,61,67,67,69,82,85,86,89,92,94,97,100,111,113,115,118,131,134,147,150,154,161,189,199,200,209,215,218,223,227,231,0
	dc.b	41,48,51,52,56,57,61,63,66,101,104,112,115,118,121,129,131,147,149,153,161,162,166,172,174,185,187,192,201,204,208,214,219,222,224,227,230,234,0
	dc.b	236,219,216,209,206,202,199,198,193,191,188,186,183,174,167,165,161,159,152,140,137,112,101,101,96,90,87,86,82,66,53,51,49,39,0
	dc.b	238,225,222,208,206,199,196,195,189,178,175,172,164,164,160,159,153,141,137,118,115,114,111,106,104,103,100,99,92,89,87,86,84,67,64,63,51,51,49,46,44,41,39,38,0
	dc.b	38,45,47,50,58,58,60,60,64,66,69,78,81,85,88,103,107,120,123,131,133,134,136,138,141,154,157,164,167,179,182,186,188,204,216,237,0
	dc.b	36,43,46,49,54,58,60,60,63,69,74,75,81,86,90,91,96,107,110,128,134,134,138,162,164,165,168,170,173,202,206,218,220,226,230,232,235,236,0
	dc.b	239,235,232,230,226,222,217,205,203,190,187,183,180,177,175,175,171,152,149,134,127,95,91,76,71,69,67,62,59,51,46,46,41,37,0
	dc.b	241,237,232,232,225,225,222,181,178,178,175,154,148,141,138,137,135,131,128,125,118,78,73,64,61,57,54,52,50,49,41,37,0
	dc.b	36,40,50,53,55,61,63,65,68,72,74,110,115,118,126,130,134,138,141,148,153,157,159,159,161,162,165,179,182,184,167,191,194,222,225,225,231,232,237,242,0
	dc.b	36,36,46,47,50,56,59,60,62,62,64,65,68,68,74,75,77,84,87,88,90,110,128,130,132,133,137,139,141,147,151,159,162,180,183,186,189,191,193,193,197,200,202,222,226,226,228,235,239,242,0
	dc.b	243,241,236,201,192,171,168,150,147,135,133,128,122,117,109,88,83,82,80,76,74,71,66,61,56,56,53,52,50,49,46,44,0
	dc.b	242,242,238,208,206,205,193,174,172,159,157,157,155,154,151,148,146,136,133,129,122,116,112,110,107,107,103,86,82,73,69,60,56,56,46,44,0
	dc.b	45,45,54,54,56,56,62,74,77,101,104,108,111,113,116,126,129,133,137,150,153,154,157,162,165,165,168,172,176,176,179,194,208,217,219,237,0
	dc.b	53,54,56,56,58,58,61,66,68,85,88,88,90,101,103,118,121,127,130,131,137,137,140,148,152,153,155,163,167,174,178,185,187,194,208,211,213,221,224,228,231,241,0
	dc.b	241,239,236,233,229,227,220,215,211,211,208,208,197,197,195,189,184,166,164,150,147,139,132,118,155,99,97,91,88,87,82,72,68,67,65,65,60,53,0
	dc.b	242,241,238,237,231,226,223,221,218,216,213,213,210,208,205,203,200,197,194,190,183,179,177,149,147,138,133,118,116,100,95,73,69,68,63,63,61,52,0
	dc.b	52,59,61,64,67,72,75,75,79,88,92,95,99,114,120,127,130,132,138,150,153,183,191,195,199,201,205,214,218,225,228,229,238,241,0
	dc.b	51,57,60,67,69,72,79,89,97,113,120,133,138,149,154,179,181,181,184,184,187,193,195,196,204,209,214,214,218,223,226,226,230,230,238,238,0
	dc.b	238,238,227,217,212,211,209,206,196,196,194,194,191,186,184,175,171,152,148,139,134,130,125,121,116,98,95,83,79,75,73,71,68,52,0
	dc.b	228,227,225,221,213,209,204,204,194,192,189,177,174,169,166,165,162,155,152,150,147,138,135,131,128,122,119,97,95,92,89,85,82,81,79,75,72,69,66,59,56,52,0
	dc.b	50,50,54,54,57,67,69,79,82,92,94,94,97,98,102,102,106,107,110,114,119,130,132,135,138,151,156,165,170,172,175,178,180,187,190,195,202,205,213,214,219,221,226,228,0
	dc.b	50,50,55,66,69,72,74,79,81,83,86,88,90,91,103,103,106,108,111,111,116,119,121,131,133,159,162,164,169,170,177,185,187,189,191,196,198,207,213,213,221,221,228,230,0
	dc.b	208,200,196,194,188,186,183,179,174,171,169,167,164,162,160,148,146,130,128,119,107,107,104,104,89,86,82,78,72,71,69,68,64,63,60,58,0
	dc.b	211,209,207,203,196,189,186,183,178,166,163,159,156,155,152,147,144,132,127,122,104,104,88,87,77,77,73,70,62,59,0
	dc.b	59,60,88,88,125,126,135,143,147,147,149,158,165,166,170,173,176,181,190,193,206,212,0
	dc.b	61,61,128,129,136,143,150,154,164,164,170,172,176,180,189,191,202,214,0
	dc.b	213,209,207,204,180,179,177,177,171,171,169,168,163,163,150,148,143,136,132,131,0
	dc.b	213,213,211,210,208,207,204,204,191,191,179,179,149,146,144,136,134,133,0
	dc.b	134,148,209,211,0
	dc.b	135,146,210,210,0
	dc.b	135,145,0
	dc.b	144,136,0
	dc.b	144,136,0
	dc.b	136,144,0
	dc.b	136,144,0
	dc.b	145,135,0
	dc.b	145,134,0
	dc.b	134,146,0
	dc.b	133,147,0
	dc.b	148,132,0
	dc.b	38,240,0
	
	dc.b	0 ; end of data
	even


*------	PROCESS INSTRUMENTS ---------------------------------------------------*

processinstruments
	lea	instactions+instoffset(pc),a0	;

	lea	v_inst0(a5),a4			;
	moveq	#4-1,d7				; 4 instruments/channels

	if numbers
	lea	v_ninst0(a5),a3			;
	endif
	
.loop	move.w	(a4)+,d0			;
	beq	.noaction			;
	move.l	(a0,d0.w),d1			;
	beq	.noaction			;
	move.l	d1,a1				;

	jsr	(a1)				; must not trash a0/a3/a4/d0/d7
;	move.l	a1,v_number(a5)
.noaction

	if numbers
	cmp.w	#instnop,d0			;
	beq	.noshow				;
	move.w	d0,(a3)				;
.noshow
	endif

	if numbers
	addq.w	#2,a3				;
	endif

	dbf	d7,.loop			;

	bsr	srfexpandshrink			;
	bsr	instb				;

	move.l	#instnop<<16+instnop,v_inst0(a5) ; probably fine here
	move.l	#instnop<<16+instnop,v_inst2(a5) ;
	rts					;


*------	FIND FIRST RAY --------------------------------------------------------*

; returns d0=index a2=pointer to ray struct
findfirstray
	lea	v_rays(a5),a0			;
	moveq	#-1,d0				; max value
	moveq	#numrays-1,d7			;	
.loop	cmp.w	ray_index(a0),d0		;
	blo	.lower				; (unsigned) d0 is lower than ray_index(a0)
	move.w	ray_index(a0),d0		;
	move.l	a0,a2				; remember that struct (a2)
.lower	add.w	#sizeofray,a0			; next ray struct
	dbf	d7,.loop			;
	rts					;


*------	SLOW LINE DRAWER FOR FILLED VECTORS -----------------------------------*

srfdraw	move.w	#pwidth,$60-$52(a6)		; init line
	move.w	#pwidth,$66-$52(a6)		;
	move.l	#$ffff8000,$72-$52(a6)		; texture data/index
	move.w	#$8000,$44-$52(a6)		; first word mask

	lea	v_raysxy(a5),a4			;
	move.w	#$0ffe,d5			;

	move.w	v_rayindexadd1(a5),d0		;
	tst.w	v_instneg(a5)			;
	beq	.noneg1				;
	neg.w	d0				;
.noneg1	add.w	d0,v_rayindex1(a5)		; 4 = 2 word values (x and y)
	and.w	d5,v_rayindex1(a5)		; important: keep value in range
	move.w	v_rayindex1(a5),d4		; ray 1
	move.l	(a4,d4.w),v_rayvertex1(a5)	; move.l -> x and y at once

	move.w	v_rayindexadd2(a5),d0		;
	tst.w	v_instneg(a5)			;
	beq	.noneg2				;
	neg.w	d0				;
.noneg2	add.w	d0,v_rayindex2(a5)		;
	and.w	d5,v_rayindex2(a5)		;
	move.w	v_rayindex2(a5),d4		; ray 2
	move.l	(a4,d4.w),v_rayvertex2(a5)	; move.l -> x and y at once

	move.w	v_rayindexadd3(a5),d0		;
	tst.w	v_instneg(a5)			;
	beq	.noneg3				;
	neg.w	d0				;
.noneg3	add.w	d0,v_rayindex3(a5)		;
	and.w	d5,v_rayindex3(a5)		;
	move.w	v_rayindex3(a5),d4		; ray 3
	move.l	(a4,d4.w),v_rayvertex3(a5)	; move.l -> x and y at once

	bsr	findfirstray			; a2=result/ray

	tst.w	d0				; (= tst.w ray_index(a2))
	beq	.go				;  top right corner

	move.l	ray_previous(a2),a0		;
	move.w	ray_colorb(a0),d5		; ("%11")
	
	cmp.w	#xmax,ray_vertex(a2)		;
	bne	.fullright			;

	move.w	#xmax,d0			;
	move.w	#ymin,d1			;
	movem.w	ray_vertex(a2),d2/d3		;
	bsr	drawline			;
	bra	.go				;
	
.fullright
	move.w	#xmax,d0			;
	move.w	#ymin,d1			;
	move.w	d0,d2				;
	move.w	#ymax,d3			;
	bsr	drawline			;

	; 3 rays
.go
	;  improve with loop? no -> would require "next vertex"
	moveq	#0,d0				; ray 1		x1
	moveq	#0,d1				; y1
	movem.w	v_rayvertex1(a5),d2/d3		; x2 y2
	move.w	v_raycolor1a(a5),d5		;
	bsr	drawline			;
	movem.w	v_rayvertex2(a5),d6/d7		; next vertex x2 y2	
	move.w	v_raycolor1b(a5),d5		;
	bsr	extra				;

	moveq	#0,d0				; ray 2		x1
	moveq	#0,d1				; y1
	movem.w	v_rayvertex2(a5),d2/d3		; x2 y2
	move.w	v_raycolor2a(a5),d5
	bsr	drawline			;
	movem.w	v_rayvertex3(a5),d6/d7		; next vertex x2 y2
	move.w	v_raycolor2b(a5),d5
	bsr	extra				;

	moveq	#0,d0				; ray 3		x1
	moveq	#0,d1				; y1
	movem.w	v_rayvertex3(a5),d2/d3		; x2 y2
	move.w	v_raycolor3a(a5),d5
	bsr	drawline			;
	movem.w	v_rayvertex1(a5),d6/d7		; next vertex x2 y2
	move.w	v_raycolor3b(a5),d5		;
	bsr	extra				;
.quit	rts					;


; d6=x next vertex d7=y next vertex
extra	cmp.w	#xmax,d2			;
	bne	.nodraw				;
	cmp.w	#xmax,d6			;
	bne	.bottom				;
	cmp.w	d7,d3				;
	blt	.draw				; draw if d3<d7
.bottom	move.w	#xmax,d6			;
	move.w	#ymax,d7			;
.draw	move.w	d2,d0				; begin
	move.w	d3,d1				;
	move.w	d6,d2				; end
	move.w	d7,d3				;
	bsr	drawline			;
.nodraw	rts					;


; d5=color d0=x1 d1=y1 d2=x2 d3=y2
drawline
	movem.l	d6/d7,-(a7)			;
	move.l	v_db1b2b(a5),a0			; bitplane buffer
	add.w	#center,a0			;
	btst	#0,d5				; plane 1?
	beq	.noplane1			;
	movem.l	d0-d3,-(a7)			;
	bsr	.draw				;
	movem.l	(a7)+,d0-d3			;
.noplane1
	btst	#1,d5				; plane 2?
	beq	.noplane2			;
	add.w	#psize,a0			; address plane 2
	movem.l	d0-d3,-(a7)			;
	bsr	.draw				;
	movem.l	(a7)+,d0-d3			;
.noplane2
	movem.l	(a7)+,d6/d7			;
	rts					;

.draw	cmp.w	d3,d1				; compare y
	bgt	.noswap				;
	exg	d0,d2				; swap -> second larger
	exg	d1,d3				;
.noswap	moveq	#4,d7				; clears upper word too
	move.l	d7,a4				; octant code
	move.w	d0,d7				; x1
	and.w	#$000f,d7			;
	ror.w	#4,d7				; startbit of line
	or.w	#$0b4a,d7			; $0b4a  $0bca=or
	swap	d7				;
	sub.w	d0,d2				; x2-x1
	bpl	.rightwards			;
	addq.w	#1,a4				;
	neg.w	d2				;
.rightwards
	sub.w	d1,d3				; y2-y1
	bpl	.upwards			;
	addq.w	#2,a4				; bset #1
	neg.w	d3				; d3=y
.upwards
	move.w	d3,d6				;
	sub.w	d2,d6				; d6=y-x
	bmi	.nsteep				; steepness <1
	exg	d2,d3				; swap x and y
	subq.w	#4,a4				; bclr #2
	neg.w	d6				;
.nsteep	lsl.w	#6,d2				; 64 = 1<<6
	add.w	#64+2,d2			; +2 required (width)
	move.w	d6,d4				; d2=y-x
	add.w	d3,d4				; d2=2y-x
	bpl	.nosign				;
	addq.w	#8,a4				; sign of 2y-x (in oct code)
.nosign	lsl.w	#2,d3				; d3=4y
	lsl.w	#2,d6				; d6=4y-4x
	swap	d3				;
	move.w	d6,d3				;
	clr.w	d7				;
	move.b	.octs(pc,a4.w),d7		; read octant from table

	move.w	d1,d6				;
	asl.w	#5,d1				; *32
	asl.w	#3,d6				; *8
	add.w	d6,d1				; = *40 (pwidth)

	lea	(a0,d1.w),a4			;
	moveq	#7,d1				;
	sub.w	d0,d1				; see bchg below
	asr.w	#3,d0				; allow neg x coords
	add.w	d0,a4				;

	move.l	a6,a3				;
	btst	#14-8,$02-$52(a6)		;
.waitblitter2
	btst	#14-8,$02-$52(a6)		;
	bne	.waitblitter2			;

	move.l	d3,$62-$52(a6)			; 4y,4y-4x	BLTB/AMOD
	move.l	d7,$40-$52(a6)			;		BLTCON0,1
	move.l	a4,$48-$52(a6)			;
	move.w	d4,(a3)+			; 2y-x		BLTAPTL
	move.l	a4,(a3)+			; start address
	bchg	d1,(a4)				; flip pixel (important for filling)
	move.w	d2,(a3)				; start
	rts					;

; +2 = SING Single bit per horizontal line for use with subsequent area fill
.octs	dc.b	0*4+1+2, 2*4+1+2, 1*4+1+2, 3*4+1+2
	dc.b	4*4+1+2, 5*4+1+2, 6*4+1+2, 7*4+1+2
	dc.b	0*4+65+2, 2*4+65+2, 1*4+65+2, 3*4+65+2
	dc.b	4*4+65+2, 5*4+65+2, 6*4+65+2, 7*4+65+2


*------	FILL SRF RAYS ---------------------------------------------------------*

fillrays
	moveq	#0,d0				; modulos
	moveq	#-1,d1				;
	move.l	v_db1b2b(a5),a0			;
	add.w	#2*psize-2,a0			;

	btst	#14-8,$02-$52(a6)		;
.waitblitter
	btst	#14-8,$02-$52(a6)		;
	bne	.waitblitter			;

	move.l	d0,$64-$52(a6)			; modulo A and D
	move.l	#$09f00012,$40-$52(a6)		; 09f00012 exlusive, 09f0000a inclusive
	move.l	d1,$44-$52(a6)			; first/last word mask
	move.l	a0,$50-$52(a6)			; source A
	move.l	a0,$54-$52(a6)			; destination D
	move.w	#(pheight*2)<<6+pwidth>>1,$58-$52(a6) ; bltsize and start
	rts					;


*------	DRAW LINDENMAYER SYSTEM -----------------------------------------------*

lmsdraw	move.l	v_db1b2b(a5),a0			; bitplane buffer
	add.w	#center,a0			;

	move.l	b_lmsanimation(pc),a1		; vertices
	moveq	#0,d0				;
	move.w	v_lmsanimindex(a5),d0		; "192" (2*96 vertices)
	and.w	#$00ff,d0			; 0 - 255 (numanimsteps)
	asl.l	#6,d0				; *64
	add.l	d0,a1				;
	asl.l	#1,d0				; *128 -> 64+128=192
	add.l	d0,a1				;

	lea	lmslines(pc),a2			;
	move.w	v_lmsnumlines(a5),d5		;
	beq	.done				;

	btst	#14-8,$02-$52(a6)		;
.waitblitter
	btst	#14-8,$02-$52(a6)		;
	bne	.waitblitter			;
	move.w	#pwidth,$60-$52(a6)		;
	move.w	#pwidth,$66-$52(a6)		;
	move.l	#$ffff8000,$72-$52(a6)		; texture data/index
	move.w	#$8000,$44-$52(a6)		; first word mask

.loop	movem.w	(a2)+,d6/d7			; index p1 p2

	move.b	(a1,d6.w),d0			; x1 (byte size -> saves memory)
	ext.w	d0				; 
	move.b	1(a1,d6.w),d1			; y1
	ext.w	d1				;
	move.b	(a1,d7.w),d2			; x2
	ext.w	d2				;
	move.b	1(a1,d7.w),d3			; y2
	ext.w	d3				;

	moveq	#4,d7				; note: moveq clears d7's upper word
	move.l	d7,a4				; octant code
	move.w	d0,d7				; x1
	and.w	#$000f,d7			;
	ror.w	#4,d7				; startbit of line
	or.w	#$0bca,d7			;
	swap	d7				;
	sub.w	d0,d2				; x2-x1
	bpl	.rightwards			;
	addq.w	#1,a4				;
	neg.w	d2				;
.rightwards
	sub.w	d1,d3				; y2-y1
	bpl	.upwards			;
	addq.w	#2,a4				; bset #1
	neg.w	d3				; d3=y
.upwards
	move.w	d3,d6				;
	sub.w	d2,d6				; d6=y-x
	bmi	.nsteep				; steepness <1
	exg	d2,d3				; swap x and y
	subq.w	#4,a4				; bclr #2
	neg.w	d6				;
.nsteep	lsl.w	#6,d2				; 64 = 1<<6
	add.w	#64+2,d2			; +2 required (width)
	move.w	d6,d4				; d2=y-x
	add.w	d3,d4				; d2=2y-x
	bpl	.nosign				;
	addq.w	#8,a4				; sign of 2y-x (in oct code)
.nosign	lsl.w	#2,d3				; d3=4y
	lsl.w	#2,d6				; d6=4y-4x
	swap	d3				;
	move.w	d6,d3				;
	clr.w	d7				;
	move.b	.octs(pc,a4.w),d7		; octant

	move.w	d1,d6				;
	asl.w	#5,d1				; *32
	asl.w	#3,d6				; *8
	add.w	d6,d1				; = *40 (pwidth)

	lea	(a0,d1.w),a4			;
	asr.w	#3,d0				;
	add.w	d0,a4				;

	move.l	a6,a3				;
	btst	#14-8,$02-$52(a6)		;
.waitblitter2
	btst	#14-8,$02-$52(a6)		;
	bne	.waitblitter2			;
	move.l	d3,$62-$52(a6)			; 4y, 4y-4x	BLTB/AMOD
	move.l	d7,$40-$52(a6)			;		BLTCON 0,1
	move.l	a4,$48-$52(a6)			;
	move.w	d4,(a3)+			; 2y-x		BLTAPTL
	move.l	a4,(a3)+			; set starting address
	move.w	d2,(a3)				; start
	subq.w	#1,d5				;
	bne	.loop				;

	CHECKACTOR actor_lmsup
	bne	.done				;

	move.l	b_clist3(pc),a1			; flashing color
	add.w	#clist3cols+2-clist3,a1		;
	lea	lmscolors(pc),a0		;
	move.w	v_lmscolindex(a5),d0		;
	beq	.zero				;
	subq.w	#2,v_lmscolindex(a5)		;


.zero	move.w	(a0,d0.w),(a1)			;
	move.w	(a0,d0.w),4(a1)			;

	moveq	#1,d1				; speed
	tst.w	v_instneg(a5)			; direction
	beq	.noneg				;
	neg.w	d1				;
.noneg	add.w	d1,v_lmsanimindex(a5)		;
.done	rts					;

.octs	dc.b	0*4+1,2*4+1,1*4+1,3*4+1
	dc.b	4*4+1,5*4+1,6*4+1,7*4+1
	dc.b	0*4+65,2*4+65,1*4+65,3*4+65
	dc.b	4*4+65,5*4+65,6*4+65,7*4+65


*------ RAYS --------------------------------------------------------------*

adjust	equ	256-34 ; start = upper right corner -> 256-34 results in 009f (159) ff80 (-128)

precalcrays
	moveq	#0,d0				; x1
	moveq	#0,d1				; y1
	move.w	#-adjust,d6			; angle
	move.l	b_sintab(pc),a2			; sin
	lea	512(a2),a3			; cos
	lea	v_raysxy(a5),a4			;
.loop	move.w	d6,d3				;
	and.w	#$07fe,d3			;
	move.w	(a3,d3.w),d2			; x2 0...256
	move.w	(a2,d3.w),d3			; y2 0...256

	bsr	clip				; important: call twice
	bsr	clip				; 

	move.w	d2,(a4)+			; x
	move.w	d3,(a4)+			; y

	tst.b	v_doquit(a5)			;
	bne	.quit				;

	addq.w	#2,d6				;
	cmp.w	#2048-adjust,d6			; done?
	bne	.loop				;
.quit	rts					;


*------ CLIP --------------------------------------------------------------*

	if 0
xmin 	equ	-100
xmax 	equ	150
ymin	equ	-90
ymax	equ	70
	else
xmin 	equ	-160
xmax 	equ	159
ymin	equ	-128
ymax	equ	127
	endif

clip	cmp.w	#xmin,d2			; if x < xmin { code |= left }
	bge	.checkright			; xmin >= x?

	if testing
	move.w	#$0303,$180(a6)			;
	endif
	
	; y = y0 + (y1 - y0) * (xmin - x0) / (x1 - x0)
	; x = xmin

	move.w	d3,d4				; d4 = y1
	sub.w	d1,d4				; d4 = (y1 - y0)
	sub.w	d0,d2				; d2 = (x1 - x0)
	move.w	#xmin,d3			;
	sub.w	d0,d3				; d3 = (xmin - x0)

	muls	d4,d3				;
	divs	d2,d3				;
	add.w	d1,d3				; y = y0 + ...
	move.w	#xmin,d2			; x = xmin	
	rts					;

.checkright
	cmp.w	#xmax,d2			; else if x > xmax { code |= right }
	ble	.checkbottom			; xmax <= x?

	if testing
	move.w	#$0003,$180(a6)			;
	endif
	
	; y = y0 + (y1 - y0) * (xmax - x0) / (x1 - x0)
	; x = xmax

	move.w	d3,d4				; d4 = y1
	sub.w	d1,d4				; d4 = (y1 - y0)
	sub.w	d0,d2				; d2 = (x1 - x0)
	move.w	#xmax,d3			;
	sub.w	d0,d3				; d3 = (xmax - x0)

	muls	d4,d3				;
	divs	d2,d3				;
	add.w	d1,d3				; y = y0 + ...
	move.w	#xmax,d2			; x = xmax	
	rts					;

.checkbottom
	cmp.w	#ymin,d3			; if y < ymin { code |= bottom }
	bge	.checktop			; ymin >= y?

	if testing
	move.w	#$0330,$180(a6)			;
	endif

	; x = x0 + (x1 - x0) * (ymin - y0) / (y1 - y0)
	; y = ymin

	move.w	d2,d4				;
	sub.w	d0,d4				; d4 = (x1 - x0)
	sub.w	d1,d3				; d3 = (y1 - y0)
	moveq	#ymin,d2			;
	sub.w	d1,d2				; d2 = (ymin - y0)

	muls	d4,d2				;
	divs	d3,d2				;
	add.w	d0,d2				; x := x0 + ...
	moveq	#ymin,d3			; y := ymin
	rts					;

.checktop
	cmp.w	#ymax,d3			; else if y > ymax { code |= top }
	ble	.done				; ymax <= y?

	if testing
	move.w	#$0300,$180(a6)			;
	endif

        ; x = x0 + (x1 - x0) * (ymax - y0) / (y1 - y0)
        ; y = ymax

	move.w	d2,d4				;
	sub.w	d0,d4				; d4 = (x1 - x0)
	sub.w	d1,d3				; d3 = (y1 - y0)
	moveq	#ymax,d2			;
	sub.w	d1,d2				; d2 = (ymax - y0)

	muls	d4,d2				;
	divs	d3,d2				;
	add.w	d0,d2				; x = x0 + ...
	moveq	#ymax,d3			; y = ymax
.done	rts					;


*------	DRAW MNEMOTRON EFFECT -------------------------------------------------*

mtndraw	move.l	v_db1b2b(a5),a0			; bitplane buffer
	add.w	#center,a0			;

	lea	v_mtns(a5),a1			;
	lea	v_mtnlines(a5),a2		;

	btst	#14-8,$02-$52(a6)		;
.waitblitter
	btst	#14-8,$02-$52(a6)		;
	bne	.waitblitter			;
	move.w	#pwidth,$60-$52(a6)		;
	move.w	#pwidth,$66-$52(a6)		;
	move.l	#$ffff8000,$72-$52(a6)		; texture data/index
	move.w	#$8000,$44-$52(a6)		; first word mask

	moveq	#nummtns-1,d5			;
.loop	tst.w	mtn_delay(a1)			;
	bmi	.done				;
	beq	.action				;
	subq.w	#1,mtn_delay(a1)		;
	bra	.done				;

.action	move.l	b_sintab(pc),a4			;

	move.w	mtn_direction(a1),d0		;
	sub.w	d0,mtn_angle(a1)		;
	move.w	mtn_angle(a1),d0		;
	and.w	#$07fe,d0			;
	move.w	(a4,d0.w),d2			; sin angle = x (factor 256)
	move.w	d2,v_mtnunitx(a5)		;
	mulu	mtn_length(a1),d2		;

	move.w	d2,d7				;
	asr.w	#7,d7				; 256:128=2px
	asr.w	#5,d2				; 256:32=8px
	add.w	d7,d2				; 2px + 8px = 10px
	add.w	mtn_x(a1),d2			;
	
	add.w	#512,a4				;
	move.w	(a4,d0.w),d3			; cos angle = y (factor 256)
	move.w	d3,v_mtnunity(a5)		;
	mulu	mtn_length(a1),d3		;

	move.w	d3,d7				;
	asr.w	#7,d7				; 256:128=2px
	asr.w	#5,d3				; 256:32=8px
	add.w	d7,d3				; 2px + 8px = 10px
	add.w	mtn_y(a1),d3			;

	movem.w	mtn_x(a1),d0/d1			; x1 y1
	bsr	clip				; important: call twice
	bsr	clip				; 

	add.w	mtn_bitplane(a1),a0		;
	bsr	.mtndrawline			;
	sub.w	mtn_bitplane(a1),a0		;

	subq.w	#1,mtn_duration(a1)		;
	bne	.running			;

	move.l	mtn_data(a1),a4			;
	movem.w	(a4)+,d6/d7			; "abbrechen" len and new dir
	move.l	a4,mtn_data(a1)			;

	movem.w	v_mtnunitx(a5),d2/d3		;
	mulu	d6,d2				;
	move.w	d2,d4				;
	asr.w	#7,d4				; 256:128=2px
	asr.w	#5,d2				; 256:32=8px
	add.w	d4,d2				; 2px + 8px = 10px
	movem.w	mtn_x(a1),d0/d1			;
	add.w	d0,d2				;
	move.w	d2,mtn_x(a1)			; new x1
	mulu	d6,d3				;
	move.w	d3,d4				;
	asr.w	#7,d4				; 256:128=2px
	asr.w	#5,d3				; 256:32=8px
	add.w	d4,d3				; 2px + 8px = 10px
	add.w	d1,d3				;
	move.w	d3,mtn_y(a1)			; new y1
	
	movem.w	d0-d3,(a2)			; store extra line
	addq.w	#8,a2				;

	sub.w	d6,mtn_length(a1)		; shorten
	move.w	d7,mtn_direction(a1)		;
	move.w	#512/delta,mtn_duration(a1)	;

	tst.w	d7				;
	bne	.running			;
	not.w	mtn_delay(a1)			; $0000 -> $ffff (negative = done)
.running
.done	add.w	#sizeofmtn,a1			; next mtn
	dbf	d5,.loop			;

.waitraster
	move.l	$04-$52(a6),d0			; wait until outside of visible area
	and.l	#$0001ff00,d0			;
	cmp.l	#302<<8,d0			; line to wait for
	blo	.waitraster			;

	move.l	v_bitplane(a5),a0		;
	add.w	#center,a0			;
	lea	v_mtnlines(a5),a1		; draw extra lines
.extra	cmp.l	a1,a2				;
	beq	.done2				;
	movem.w	(a1)+,d0-d3			;
	bsr	.mtndrawline			;
	bra	.extra				;

.mtndrawline
	moveq	#4,d7				; note: moveq clears d7's upper word
	move.l	d7,a4				; octant code
	move.w	d0,d7				; x1
	and.w	#$000f,d7			;
	ror.w	#4,d7				; startbit of line
	or.w	#$0bca,d7			;
	swap	d7				;
	sub.w	d0,d2				; x2-x1
	bpl	.rightwards			;
	addq.w	#1,a4				;
	neg.w	d2				;
.rightwards
	sub.w	d1,d3				; y2-y1
	bpl	.upwards			;
	addq.w	#2,a4				; bset #1
	neg.w	d3				; d3=y
.upwards
	move.w	d3,d6				;
	sub.w	d2,d6				; d6=y-x
	bmi	.nsteep				; steepness <1
	exg	d2,d3				; swap x and y
	subq.w	#4,a4				; bclr #2
	neg.w	d6				;
.nsteep	lsl.w	#6,d2				; 64 = 1<<6
	add.w	#64+2,d2			; +2 required (width)
	move.w	d6,d4				; d2=y-x
	add.w	d3,d4				; d2=2y-x
	bpl	.nosign				;
	addq.w	#8,a4				; sign of 2y-x (in oct code)
.nosign	lsl.w	#2,d3				; d3=4y
	lsl.w	#2,d6				; d6=4y-4x
	swap	d3				;
	move.w	d6,d3				;
	clr.w	d7				;
	move.b	.octs(pc,a4.w),d7		; octant
	move.w	d1,d6				;
	asl.w	#5,d1				; *32
	asl.w	#3,d6				; *8
	add.w	d6,d1				; = *40 (pwidth)

	lea	(a0,d1.w),a4			;
	asr.w	#3,d0				;
	add.w	d0,a4				;

	move.l	a6,a3				;
	btst	#14-8,$02-$52(a6)		;
.waitblitter2
	btst	#14-8,$02-$52(a6)		;
	bne	.waitblitter2			;
	move.l	d3,$62-$52(a6)			; 4y, 4y-4x	BLTB/AMOD
	move.l	d7,$40-$52(a6)			;		BLTCON 0,1
	move.l	a4,$48-$52(a6)			;
	move.w	d4,(a3)+			; 2y-x		BLTAPTL
	move.l	a4,(a3)+			; set starting address
	move.w	d2,(a3)				; start
.done2	rts					;

.octs	dc.b	0*4+1,2*4+1,1*4+1,3*4+1
	dc.b	4*4+1,5*4+1,6*4+1,7*4+1
	dc.b	0*4+65,2*4+65,1*4+65,3*4+65
	dc.b	4*4+65,5*4+65,6*4+65,7*4+65


*------	APPLY MTX -------------------------------------------------------------*

z	equ 0

; a2 = 2d data destination
; a3 = 3d data source
; d7 num vertices
applymtx
	lea	v_matrix(a5),a0			;
.loop	move.l	a0,a1				;
	movem.w	(a3)+,d1/d2			; x y
	
	if z
	moveq	#0,d0				; z
	endif

	move.w	d1,d4				;
	muls	(a1)+,d4			;
	move.w	d2,d5				;
	muls	(a1)+,d5			;
	add.l	d5,d4				;

	if z
	move.w	d0,d5				;
	muls	(a1)+,d5			;
	add.l	d5,d4				;
	else
	addq.w	#2,a1				;
	endif

	move.w	d1,d5				;
	muls	(a1)+,d5			;
	move.w	d2,d6				;
	muls	(a1)+,d6			;
	add.l	d6,d5				;
	
	if z
	move.w	d0,d6				;
	muls	(a1)+,d6			;
	add.l	d6,d5				;
	else
	addq.w	#2,a1				;
	endif
	
	muls	(a1)+,d1			;
	muls	(a1)+,d2			;
	
	if z
	muls	(a1)+,d0			;
	add.l	d1,d0				;
	add.l	d2,d0				;
	asr.l	#8,d0				;
;	move.w	d0,d6				; (no need for z value)
	else
	addq.w	#2,a1				;
	move.l	d1,d0				;
	add.l	d2,d0				;
	asr.l	#8,d0				;
	endif

	add.w	#$00e2,d0			; distance
	divs	d0,d4				;
	divs	d0,d5				;

	move.b	d4,(a2)+			; x
	move.b	d5,(a2)+			; y

	dbf	d7,.loop			;
	rts					;


*------	MTX -------------------------------------------------------------------*

mtx	move.l	b_sintab(pc),a0			; sin
	lea	512(a0),a1			; cos

	move.w	#$07fe,d5			;
	move.w	v_a(a5),d6			;
	and.w	d5,d6				;
	move.w	(a0,d6.w),d0			; sin a
	move.w	(a1,d6.w),d3			; cos a

	move.w	v_b(a5),d6			;
	and.w	d5,d6				;
	move.w	(a0,d6.w),d1			; sin b
	move.w	(a1,d6.w),d4			; cos b

	move.w	v_c(a5),d6			;
	and.w	d5,d6				;
	move.w	(a0,d6.w),d2			; sin c
	move.w	(a1,d6.w),d5			; cos c

	lea	v_matrix(a5),a0			;
	move.w	d0,d6				;
	muls	d1,d6				;
	asr.l	#8,d6				;
	move.w	d6,a1				;
	move.w	d3,d7				;
	muls	d2,d7				;
	asr.l	#8,d7				;
	move.w	d7,a4				;
	muls	d5,d6				;
	asr.l	#8,d6				;
	sub.w	d7,d6				;
	move.w	d6,6(a0)			;
	move.w	d3,d7				;
	muls	d5,d7				;
	asr.l	#8,d7				;
	move.w	d7,a3				;
	move.w	a1,d6				;
	muls	d2,d6				;
	asr.l	#8,d6				;
	add.w	d7,d6				;
	move.w	d6,8(a0)			;
	move.w	a3,d6				;
	muls	d1,d6				;
	asr.l	#8,d6				;
	move.w	d0,d7				;
	muls	d2,d7				;
	asr.l	#8,d7				;
	add.w	d7,d6				;
	move.w	d6,12(a0)			;
	move.w	a4,d6				;
	muls	d1,d6				;
	asr.l	#8,d6				;
	move.w	d0,d7				;
	muls	d5,d7				;
	asr.l	#8,d7				;
	sub.w	d7,d6				;
	move.w	d6,14(a0)			;
	muls	d4,d5				;
	asr.l	#8,d5				;
	move.w	d5,(a0)				;
	muls	d4,d2				;
	asr.l	#8,d2				;
	move.w	d2,2(a0)			;
	muls	d4,d0				;
	asr.l	#8,d0				;
	move.w	d0,10(a0)			;
	muls	d4,d3				;
	asr.l	#8,d3				;
	move.w	d3,16(a0)			;
	neg.w	d1				;
	move.w	d1,4(a0)			;
	rts					;


*------ SET SPRITE POINTERS ---------------------------------------------------*

; a0: clist d0: sprite data
setspritepointers
	moveq	#8-1,d7				; 8 sprite pointers
.loop	swap	d0				; sprite0
	move.w	d0,(a0)				;
	swap	d0				;
	move.w	d0,4(a0)			;
	addq.l	#8,a0				; next pointer
	dbf	d7,.loop			;
	rts					;


*------ SPRITE DATA -----------------------------------------------------------*

spritedata
	dc.w	$1905,$1a00	; 1px high sprite in the top-leftmost valid position
	dc.w	$0000,$0000	; blank pixel data
	dc.w	$0000,$0000	; end of sprite
spritedataend


*------	PLAYER ----------------------------------------------------------------*

cmds	dc.w	0 ; cmd_wait (must be zero)
	dc.w	cmdactorstart-cmds
	dc.w	cmdactorstop-cmds
	dc.w	cmdsrf-cmds
	dc.w	cmdmtncolora-cmds
	dc.w	cmdmtncolorb-cmds
	dc.w	cmdmtn-cmds
	dc.w	cmdcosmicrain-cmds
	dc.w	cmdtxt-cmds
	dc.w	cmdquit-cmds
	dc.w	cmdsrfexpand-cmds
	dc.w	cmdsrfshrink-cmds
	dc.w	cmdsrfcolor0-cmds
	dc.w	cmdsrfcolor1-cmds
	dc.w	cmdsrfcolor2-cmds
	dc.w	cmdlms-cmds
	dc.w	cmdladd-cmds

play	tst.b	v_doquit(a5)		;
	bne	.quit			; don't mess up with precalc
	tst.b	v_wait(a5)		;
	beq	.donotwait		;
	subq.b	#1,v_wait(a5)		;
.quit	rts				;
.donotwait
	move.l	v_cmdspointer(a5),a0	;
loop	move.b	(a0)+,d0		;
	bne	.notcmdwait		;
	move.b	(a0)+,v_wait(a5)	; cmd_wait duration
	move.l	a0,v_cmdspointer(a5)	;
	if numbers
	addq.b	#1,v_waitcount(a5)	; sync helper
	endif
	rts				;

.notcmdwait
	ext.w	d0			;
	lea	cmds(pc),a1		;
	add.w	(a1,d0.w),a1		;
	jsr	(a1)			; execute cmd
	bra	loop			;
	
cmdactorstart
	move.b	(a0)+,d1		; actor
	move.l	v_actors(a5),d2		;
	bset	d1,d2			;
	bra	set			;

cmdactorstop
	moveq	#0,d1			;
	move.b	(a0)+,d1		; actor
	move.l	v_actors(a5),d2		;
	bclr	d1,d2			;
set	move.l	d2,v_actors(a5)		;
	rts				;

cmdsrf	move.l	b_clist1(pc),$80(a6)	;
;	STARTACTOR actor_srf		; special case because of background
	rts				;

cmdmtncolora
	move.w	#mtncolora-clist2+2,d2	;
reuse	moveq	#0,d1			;
	move.b	(a0)+,d1		; value (upper byte)
	asl.w	#8,d1			;
	move.b	(a0)+,d1		; value (lower byte)
	move.l	b_clist2(pc),a1		;
	move.w	d1,(a1,d2.w)		;
	rts				;

cmdmtncolorb
	move.w	#mtncolorb-clist2+2,d2	;
	bra	reuse			;

cmdsrfcolor1
	moveq	#srfcolor1-clist1+2,d2	;
reuse2	moveq	#0,d1			;
	move.b	(a0)+,d1		; value (upper byte)
	asl.w	#8,d1			;
	move.b	(a0)+,d1		; value (lower byte)
	move.l	b_clist1(pc),a1		;
	move.w	d1,(a1,d2.w)		;
	rts				;

cmdsrfcolor2
	move.w	#srfcolor2-clist1+2,d2	;
	bra	reuse2			;

cmdsrfcolor0
	move.w	#srfcolor0-clist1+2,d2	;
	bra	reuse2			;

cmdmtn	move.l	b_clist2(pc),$80(a6)	;
	STARTACTOR actor_mtn		;
	rts				;

cmdcosmicrain
	tst.w	v_cosmicsindex(a5)	; flip cosmic rain
	beq	.show			;
	move.w	#-2,v_cosmicsidxd(a5)	;
	clr.w	v_cosmicsidxs(a5)	;
	bra	.go			;	
.show	move.w	#2,v_cosmicsidxd(a5)	;
	move.w	#cosmicsend-cosmics-2,v_cosmicsidxs(a5) ;
.go	rts				;

cmdtxt	move.l	b_clist4(pc),$80(a6)	;
	STARTACTOR actor_txt		;
	rts				;

cmdquit	st	v_doquit(a5)		;
	rts				;

cmdsrfexpand
	clr.w	v_instwdindex(a5)	;
	STARTACTOR actor_srfexpand
	rts				;
	
cmdsrfshrink
	move.w	#instwidendata2end-instwidendata2,v_instwdindex(a5) ;
	STARTACTOR actor_srfshrink
	rts				;

cmdlms	move.l	b_clist3(pc),$80(a6)	;
	STARTACTOR actor_lms		;
	rts				;

cmdladd	moveq	#0,d1			;
	move.b	(a0)+,d1		; y
	add.w	d1,d1			; 2 (4) lines at once
	add.w	d1,d1			;

	move.w	d1,d2			;
	asl.w	#5,d1			; *32
	asl.w	#3,d2			; *8
	add.w	d2,d1			; = *40 (pwidth)

	moveq	#0,d2			;
	move.b	(a0)+,d2		; x
	move.l	v_db1b2b(a5),a2		; source (buffer)
	add.w	d2,a2			;
	move.l	v_db1a2a(a5),a3		; destination (bitplane)
	add.w	d2,a3			;
	add.w	#26*pwidth,a3		; top padding

	move.w	(a2,d1.w),(a3,d1.w)	; line 1
	move.w	2*pwidth(a2,d1.w),2*pwidth(a3,d1.w) ; line 2
	rts				;

; commands
cmdoffset	equ	2		; word offset (cmds jump table)

cmd_wait	equ 	0*cmdoffset
cmd_actor_start	equ	1*cmdoffset
cmd_actor_stop	equ	2*cmdoffset
cmd_srf		equ	3*cmdoffset
cmd_mtncolora	equ	4*cmdoffset	; change color above
cmd_mtncolorb	equ	5*cmdoffset	; change color below
cmd_mtn		equ	6*cmdoffset
cmd_cosmicrain	equ	7*cmdoffset
cmd_txt		equ	8*cmdoffset
cmd_quit	equ	9*cmdoffset
cmd_srfexpand	equ	10*cmdoffset
cmd_srfshrink	equ	11*cmdoffset
cmd_srfcolor0	equ	12*cmdoffset
cmd_srfcolor1	equ	13*cmdoffset
cmd_srfcolor2	equ	14*cmdoffset
cmd_lms		equ	15*cmdoffset
cmd		equ	16*cmdoffset	; short for cmd_ladd

ld		equ	2	; ladd delay

playcmds
	dc.b	cmd_actor_start,actor_prepare_4fx
	dc.b	cmd_wait,50

	dc.b	cmd,0,18, cmd_wait,ld
	dc.b	cmd,0,20, cmd,1,24, cmd_wait,ld

	dc.b	cmd,0,22, cmd,2,14, cmd,1,22, cmd_wait,ld	; 0 done 2 start
	dc.b	cmd,2,16, cmd,1,20, cmd_wait,ld
	dc.b	cmd,2,18, cmd,1,18, cmd_wait,ld
	dc.b	cmd,2,20, cmd,1,16, cmd,3,26, cmd_wait,ld	; 3 start
	dc.b	cmd,2,22, cmd,3,24, cmd_wait,ld
	dc.b	cmd,2,24, cmd,3,22, cmd,4,14, cmd_wait,ld	; 2 done 4 start

	dc.b	cmd,4,16, cmd,3,20, cmd_wait,ld
	dc.b	cmd,4,18, cmd,3,18, cmd_wait,ld
	dc.b	cmd,4,20, cmd,5,26, cmd,3,16, cmd_wait,ld	; 5 start
	dc.b	cmd,4,22, cmd,5,24, cmd,3,14, cmd_wait,ld
	dc.b	cmd,4,24, cmd,5,22, cmd_wait,ld
	dc.b	cmd,4,26, cmd,5,20, cmd,6,12, cmd_wait,ld	; 4 done 6 start

	dc.b	cmd,6,14, cmd,5,18, cmd_wait,ld
	dc.b	cmd,6,16, cmd,5,16, cmd_wait,ld
	dc.b	cmd,6,18, cmd,5,14, cmd_wait,ld
	dc.b	cmd,6,20, cmd,5,12, cmd,7,28, cmd_wait,ld	; 7 start
	dc.b	cmd,6,22, cmd,7,26, cmd_wait,ld
	dc.b	cmd,6,24, cmd,7,24, cmd_wait,ld
	dc.b	cmd,6,26, cmd,7,22, cmd_wait,ld
	dc.b	cmd,6,28, cmd,7,20, cmd,8,10, cmd_wait,ld	; 6 done 8 start

	dc.b	cmd,8,12, cmd,7,18, cmd_wait,ld
	dc.b	cmd,8,14, cmd,7,16, cmd_wait,ld
	dc.b	cmd,8,16, cmd,7,14, cmd_wait,ld
	dc.b	cmd,8,18, cmd,7,12, cmd_wait,ld
	dc.b	cmd,8,20, cmd,9,30, cmd_wait,ld			; 9 start
	dc.b	cmd,8,22, cmd,9,28, cmd_wait,ld
	dc.b	cmd,8,24, cmd,9,26, cmd_wait,ld
	dc.b	cmd,8,26, cmd,9,24, cmd_wait,ld
	dc.b	cmd,8,28, cmd,9,22, cmd,10,10, cmd_wait,ld	; 8 done 10 start

	dc.b	cmd,10,12, cmd,9,20, cmd_wait,ld
	dc.b	cmd,10,14, cmd,9,18, cmd_wait,ld
	dc.b	cmd,10,16, cmd,9,16, cmd_wait,ld
	dc.b	cmd,10,18, cmd,9,14, cmd_wait,ld
	dc.b	cmd,10,20, cmd,9,12, cmd,11,30, cmd_wait,ld	; 11 start
	dc.b	cmd,10,22, cmd,9,10, cmd,11,28, cmd_wait,ld
	dc.b	cmd,10,24, cmd,11,26, cmd_wait,ld
	dc.b	cmd,10,26, cmd,11,24, cmd_wait,ld
	dc.b	cmd,10,28, cmd,11,22, cmd_wait,ld
	dc.b	cmd,10,30, cmd,11,20, cmd,12,8, cmd_wait,ld	; 10 done 12 start

	dc.b	cmd,12,10, cmd,11,18, cmd_wait,ld
	dc.b	cmd,12,12, cmd,11,16, cmd_wait,ld
	dc.b	cmd,12,14, cmd,11,14, cmd_wait,ld
	dc.b	cmd,12,16, cmd,11,12, cmd_wait,ld
	dc.b	cmd,12,18, cmd,11,10, cmd_wait,ld
	dc.b	cmd,12,20, cmd,11,8, cmd,13,30, cmd_wait,ld	; 13 start
	dc.b	cmd,12,22, cmd,13,28, cmd_wait,ld
	dc.b	cmd,12,24, cmd,13,26, cmd_wait,ld
	dc.b	cmd,12,26, cmd,13,24, cmd_wait,ld
	dc.b	cmd,12,28, cmd,13,22, cmd_wait,ld
	dc.b	cmd,12,30, cmd,13,20, cmd,14,10, cmd_wait,ld	; 12 done 14 start

	dc.b	cmd,14,12, cmd,13,18, cmd_wait,ld
	dc.b	cmd,14,14, cmd,13,16, cmd_wait,ld
	dc.b	cmd,14,16, cmd,13,14, cmd_wait,ld
	dc.b	cmd,14,18, cmd,13,12, cmd_wait,ld
	dc.b	cmd,14,20, cmd,13,10, cmd,15,30, cmd_wait,ld	; 15 start
	dc.b	cmd,14,22, cmd,15,28, cmd_wait,ld
	dc.b	cmd,14,24, cmd,15,26, cmd_wait,ld
	dc.b	cmd,14,26, cmd,15,24, cmd_wait,ld
	dc.b	cmd,14,28, cmd,15,22, cmd_wait,ld
	dc.b	cmd,14,30, cmd,15,20, cmd_wait,ld
	dc.b	cmd,14,32, cmd,15,18, cmd,16,8, cmd_wait,ld	; 14 done 16 start

	dc.b	cmd,16,10, cmd,15,16, cmd_wait,ld
	dc.b	cmd,16,12, cmd,15,14, cmd_wait,ld
	dc.b	cmd,16,14, cmd,15,12, cmd_wait,ld
	dc.b	cmd,16,16, cmd,15,10, cmd_wait,ld
	dc.b	cmd,16,18, cmd,15,8, cmd_wait,ld
	dc.b	cmd,16,20, cmd,17,28, cmd_wait,ld		; 17 start
	dc.b	cmd,16,22, cmd,17,26, cmd_wait,ld
	dc.b	cmd,16,24, cmd,17,24, cmd_wait,ld
	dc.b	cmd,16,26, cmd,17,22, cmd_wait,ld
	dc.b	cmd,16,28, cmd,17,20, cmd,18,8, cmd_wait,ld	; 16 done 18 start

	dc.b	cmd,18,10, cmd,17,18, cmd_wait,ld
	dc.b	cmd,18,12, cmd,17,16, cmd_wait,ld
	dc.b	cmd,18,14, cmd,17,14, cmd_wait,ld
	dc.b	cmd,18,16, cmd,17,12, cmd_wait,ld
	dc.b	cmd,18,18, cmd,17,10, cmd_wait,ld
	dc.b	cmd,18,20, cmd,19,28, cmd_wait,ld		; 19 start
	dc.b	cmd,18,22, cmd,19,26, cmd_wait,ld
	dc.b	cmd,18,24, cmd,19,24, cmd_wait,ld
	dc.b	cmd,18,26, cmd,19,22, cmd_wait,ld
	dc.b	cmd,18,28, cmd,19,20, cmd,20,6, cmd_wait,ld	; 18 done 20 start

	dc.b	cmd,20,8, cmd,19,18, cmd_wait,ld
	dc.b	cmd,20,10, cmd,19,16, cmd_wait,ld
	dc.b	cmd,20,12, cmd,19,14, cmd_wait,ld
	dc.b	cmd,20,14, cmd,19,12, cmd_wait,ld
	dc.b	cmd,20,16, cmd,19,10, cmd_wait,ld
	dc.b	cmd,20,18, cmd,19,8, cmd_wait,ld
	dc.b	cmd,20,20, cmd,21,32, cmd_wait,ld		; 21 start
	dc.b	cmd,20,22, cmd,21,30, cmd_wait,ld
	dc.b	cmd,20,24, cmd,21,28, cmd_wait,ld
	dc.b	cmd,20,26, cmd,21,26, cmd_wait,ld
	dc.b	cmd,20,28, cmd,21,24, cmd_wait,ld
	dc.b	cmd,20,30, cmd,21,22, cmd,22,6, cmd_wait,ld	; 20 done 22 start

	dc.b	cmd,22,8, cmd,21,20, cmd_wait,ld
	dc.b	cmd,22,10, cmd,21,18, cmd_wait,ld
	dc.b	cmd,22,12, cmd,21,16, cmd_wait,ld
	dc.b	cmd,22,14, cmd,21,14, cmd_wait,ld
	dc.b	cmd,22,16, cmd,21,12, cmd_wait,ld
	dc.b	cmd,22,18, cmd,21,10, cmd_wait,ld
	dc.b	cmd,22,20, cmd,21,8, cmd,23,32, cmd_wait,ld
	dc.b	cmd,22,22, cmd,21,6, cmd,23,30, cmd_wait,ld	; 23 start
	dc.b	cmd,22,24, cmd,23,28, cmd_wait,ld
	dc.b	cmd,22,26, cmd,23,26, cmd_wait,ld
	dc.b	cmd,22,28, cmd,23,24, cmd_wait,ld
	dc.b	cmd,22,30, cmd,23,22, cmd_wait,ld
	dc.b	cmd,22,32, cmd,23,20, cmd,24,6, cmd_wait,ld	; 22 done 24 start

	dc.b	cmd,24,8, cmd,23,18, cmd_wait,ld
	dc.b	cmd,24,10, cmd,23,16, cmd_wait,ld
	dc.b	cmd,24,12, cmd,23,14, cmd_wait,ld
	dc.b	cmd,24,14, cmd,23,12, cmd_wait,ld
	dc.b	cmd,24,16, cmd,23,10, cmd_wait,ld
	dc.b	cmd,24,18, cmd,23,8, cmd_wait,ld
	dc.b	cmd,24,20, cmd,23,6, cmd,25,30, cmd_wait,ld	; 25 start
	dc.b	cmd,24,22, cmd,25,28, cmd_wait,ld
	dc.b	cmd,24,24, cmd,25,26, cmd_wait,ld
	dc.b	cmd,24,26, cmd,25,24, cmd_wait,ld
	dc.b	cmd,24,28, cmd,25,22, cmd_wait,ld
	dc.b	cmd,24,30, cmd,25,20, cmd_wait,ld
	dc.b	cmd,24,32, cmd,25,18, cmd,26,6, cmd_wait,ld	; 24 done 26 start

	dc.b	cmd,26,8, cmd,25,16, cmd_wait,ld
	dc.b	cmd,26,10, cmd,25,14, cmd_wait,ld
	dc.b	cmd,26,12, cmd,25,12, cmd_wait,ld
	dc.b	cmd,26,14, cmd,25,10, cmd_wait,ld
	dc.b	cmd,26,16, cmd,25,8, cmd_wait,ld
	dc.b	cmd,26,18, cmd,25,6, cmd_wait,ld
	dc.b	cmd,26,20, cmd,27,32, cmd_wait,ld		; 27 start
	dc.b	cmd,26,22, cmd,27,30, cmd_wait,ld
	dc.b	cmd,26,24, cmd,27,28, cmd_wait,ld
	dc.b	cmd,26,26, cmd,27,26, cmd_wait,ld
	dc.b	cmd,26,28, cmd,27,24, cmd_wait,ld
	dc.b	cmd,26,30, cmd,27,22, cmd_wait,ld
	dc.b	cmd,26,32, cmd,27,20, cmd,28,8, cmd_wait,ld	; 26 done 28 start
	
	dc.b	cmd,28,10, cmd,27,18, cmd_wait,ld
	dc.b	cmd,28,12, cmd,27,16, cmd_wait,ld
	dc.b	cmd,28,14, cmd,27,14, cmd_wait,ld
	dc.b	cmd,28,16, cmd,27,12, cmd_wait,ld
	dc.b	cmd,28,18, cmd,27,10, cmd_wait,ld
	dc.b	cmd,28,20, cmd,27,8, cmd,29,32, cmd_wait,ld	; 29 start
	dc.b	cmd,28,22, cmd,29,30, cmd_wait,ld
	dc.b	cmd,28,24, cmd,29,28, cmd_wait,ld
	dc.b	cmd,28,26, cmd,29,26, cmd_wait,ld
	dc.b	cmd,28,28, cmd,29,24, cmd_wait,ld
	dc.b	cmd,28,30, cmd,29,22, cmd_wait,ld
	dc.b	cmd,28,32, cmd,29,20, cmd,30,8, cmd_wait,ld	; 28 done 30 start

	dc.b	cmd,30,10, cmd,29,18, cmd_wait,ld
	dc.b	cmd,30,12, cmd,29,16, cmd_wait,ld
	dc.b	cmd,30,14, cmd,29,14, cmd_wait,ld
	dc.b	cmd,30,16, cmd,29,12, cmd_wait,ld
	dc.b	cmd,30,18, cmd,29,10, cmd_wait,ld
	dc.b	cmd,30,20, cmd,29,8, cmd,31,30, cmd_wait,ld	; 31 start
	dc.b	cmd,30,22, cmd,31,28, cmd_wait,ld
	dc.b	cmd,30,24, cmd,31,26, cmd_wait,ld
	dc.b	cmd,30,26, cmd,31,24, cmd_wait,ld
	dc.b	cmd,30,28, cmd,31,22, cmd_wait,ld
	dc.b	cmd,30,30, cmd,31,20, cmd,32,8, cmd_wait,ld	; 30 done 32 start

	dc.b	cmd,32,10, cmd,31,18, cmd_wait,ld
	dc.b	cmd,32,12, cmd,31,16, cmd_wait,ld
	dc.b	cmd,32,14, cmd,31,14, cmd_wait,ld
	dc.b	cmd,32,16, cmd,31,12, cmd_wait,ld
	dc.b	cmd,32,18, cmd,31,10, cmd_wait,ld
	dc.b	cmd,32,20, cmd,31,8, cmd,33,28, cmd_wait,ld
	dc.b	cmd,32,22, cmd,33,26, cmd_wait,ld
	dc.b	cmd,32,24, cmd,33,24, cmd_wait,ld
	dc.b	cmd,32,26, cmd,33,22, cmd_wait,ld
	dc.b	cmd,32,28, cmd,33,20, cmd,34,18, cmd_wait,ld	; 32 done 34 start

	dc.b	cmd,34,20, cmd,33,18, cmd_wait,ld

	dc.b	cmd,34,28, cmd_wait,ld ; (34 skips vom 20 to 28!)

	dc.b	cmd,35,20, cmd_wait,ld
	dc.b	cmd,35,18, cmd_wait,ld
	dc.b	cmd,36,18, cmd_wait,ld
	dc.b	cmd,36,20, cmd_wait,ld
	dc.b	cmd,37,20, cmd_wait,ld
	dc.b	cmd,37,18, cmd_wait,ld
	dc.b	cmd,38,18, cmd_wait,ld
	dc.b	cmd,38,20, cmd_wait,ld
	dc.b	cmd,39,20, cmd_wait,ld
	dc.b	cmd,39,18, cmd,40,6, cmd_wait,ld

	dc.b	cmd,40,8, cmd_wait,ld
	dc.b	cmd,40,10, cmd_wait,ld
	dc.b	cmd,40,12, cmd_wait,ld
	dc.b	cmd,40,14, cmd_wait,ld
	dc.b	cmd,40,16, cmd_wait,ld
	dc.b	cmd,40,18, cmd_wait,ld
	dc.b	cmd,40,20, cmd_wait,ld
	dc.b	cmd,40,22, cmd_wait,ld
	dc.b	cmd,40,24, cmd_wait,ld
	dc.b	cmd,40,26, cmd_wait,ld
	dc.b	cmd,40,28, cmd_wait,ld
	dc.b	cmd,40,30, cmd_wait,ld
	dc.b	cmd,40,32

	dc.b	cmd_wait,64
	dc.b	cmd_actor_start,actor_ladd_fading

	dc.b	cmd_wait,64
	dc.b	cmd_actor_start,actor_ladd_fading

	dc.b	cmd_wait,249
	dc.b	cmd_actor_stop,actor_ladd
	dc.b	cmd_lms

	dc.b	cmd_wait,50 ; empty screen for 1 second
	dc.b	cmd_actor_start,actor_lmsup

	dc.b	cmd_wait,50
	dc.b	cmd_actor_start,actor_lsp

	dc.b	cmd_wait,250
	dc.b	cmd_wait,50

	dc.b	cmd_wait,130
	dc.b	cmd_cosmicrain
	dc.b	cmd_wait,230
	dc.b	cmd_cosmicrain
	dc.b	cmd_wait,130
	dc.b	cmd_cosmicrain
	dc.b	cmd_wait,130
	dc.b	cmd_cosmicrain
	dc.b	cmd_wait,130
	dc.b	cmd_cosmicrain
	dc.b	cmd_wait,130
	dc.b	cmd_cosmicrain
	dc.b	cmd_wait,40
	dc.b	cmd_cosmicrain

	dc.b	cmd_actor_start,actor_lmsdown
	dc.b	cmd_wait,100

	dc.b	cmd_actor_stop,actor_lms
	dc.b	cmd_txt

	dc.b	cmd_wait,250
	dc.b	cmd_wait,250
	dc.b	cmd_wait,250
	dc.b	cmd_wait,250
	dc.b	cmd_wait,250
	dc.b	cmd_wait,200

	dc.b	cmd_srf ; clist will become active next frame
	dc.b	cmd_wait,0
	dc.b	cmd_actor_stop,actor_txt
	dc.b	cmd_actor_start,actor_srf
	dc.b	cmd_wait,0
	dc.b	cmd_srfcolor0,$00,$00

	dc.b	cmd_wait,250
	dc.b	cmd_srfexpand, cmd_wait,100-20 ; 100
	dc.b	cmd_srfshrink, cmd_wait,200+20 ; 200
	dc.b	cmd_srfexpand, cmd_wait,100-25 ; 100
	dc.b	cmd_srfshrink, cmd_wait,50+25

	dc.b	cmd_wait,230
	dc.b	cmd_srfexpand, cmd_wait,100-5
	dc.b	cmd_srfshrink, cmd_wait,223+5

	dc.b	cmd_srfcolor1,$0f,$36, cmd_wait,0
	dc.b	cmd_srfcolor1,$0e,$36, cmd_wait,0
	dc.b	cmd_srfcolor1,$0d,$36, cmd_wait,0
	dc.b	cmd_srfcolor1,$0c,$25, cmd_wait,0
	dc.b	cmd_srfcolor1,$0b,$25, cmd_wait,0
	dc.b	cmd_srfcolor1,$0a,$25, cmd_wait,0
	dc.b	cmd_srfcolor1,$09,$14, cmd_wait,0
	dc.b	cmd_srfcolor1,$08,$14, cmd_wait,0
	dc.b	cmd_srfcolor1,$07,$14, cmd_wait,0
	dc.b	cmd_srfcolor1,$06,$13, cmd_wait,0
	dc.b	cmd_srfcolor1,$05,$03
	dc.b	cmd_srfcolor2,$0a,$af, cmd_wait,0
	dc.b	cmd_srfcolor1,$04,$03
	dc.b	cmd_srfcolor2,$09,$9e, cmd_wait,0
	dc.b	cmd_srfcolor1,$03,$02
	dc.b	cmd_srfcolor2,$09,$9d, cmd_wait,0
	dc.b	cmd_srfcolor1,$02,$02
	dc.b	cmd_srfcolor2,$08,$8c, cmd_wait,0
	dc.b	cmd_srfcolor1,$01,$02
	dc.b	cmd_srfcolor2,$08,$8b, cmd_wait,0
	dc.b	cmd_srfcolor2,$07,$7a, cmd_wait,0
	dc.b	cmd_srfcolor2,$06,$69, cmd_wait,0
	dc.b	cmd_srfcolor2,$06,$68, cmd_wait,0
	dc.b	cmd_srfcolor2,$05,$57, cmd_wait,0
	dc.b	cmd_srfcolor2,$04,$46, cmd_wait,0
	dc.b	cmd_srfcolor2,$03,$35, cmd_wait,0
	dc.b	cmd_srfcolor2,$02,$24, cmd_wait,0
	dc.b	cmd_srfcolor2,$01,$12, cmd_wait,0
	dc.b	cmd_srfcolor2,$01,$02
	
	dc.b	cmd_wait,70

	dc.b	cmd_actor_stop,actor_srf
	dc.b	cmd_srfcolor0,$01,$02
	dc.b	cmd_mtn

	rept 7	; duration mtn
	dc.b	cmd_wait,255
	endr
	dc.b	cmd_wait,196

neon	equ	1
	dc.b	cmd_mtncolorb,mtnc2d>>8,mtnc2d&$ff
	dc.b	cmd_wait,1+neon
	dc.b	cmd_mtncolorb,mtnc2dd>>8,mtnc2dd&$ff
	dc.b	cmd_wait,1+neon
	dc.b	cmd_mtncolorb,mtnc2>>8,mtnc2&$ff
	dc.b	cmd_mtncolora,mtnc1>>8,mtnc1&$ff
	dc.b	cmd_wait,0+neon
	dc.b	cmd_mtncolorb,mtnc2d>>8,mtnc2d&$ff
	dc.b	cmd_mtncolora,mtnc1d>>8,mtnc1d&$ff
	dc.b	cmd_wait,1+neon
	dc.b	cmd_mtncolorb,mtnc0>>8,mtnc0&$ff
	dc.b	cmd_wait,0+neon
	dc.b	cmd_mtncolora,mtnc1dd>>8,mtnc1dd&$ff
	dc.b	cmd_wait,1+neon
	dc.b	cmd_mtncolora,mtnc0>>8,mtnc0&$ff
	dc.b	cmd_wait,1+neon
	dc.b	cmd_mtncolora,mtnc1>>8,mtnc1&$ff
	dc.b	cmd_wait,0+neon
	dc.b	cmd_mtncolora,mtnc0>>8,mtnc0&$ff
	dc.b	cmd_wait,2+neon
	dc.b	cmd_mtncolora,mtnc1>>8,mtnc1&$ff
	dc.b	cmd_wait,0+neon
	dc.b	cmd_mtncolorb,mtnc2>>8,mtnc2&$ff
	dc.b	cmd_wait,0+neon
	dc.b	cmd_mtncolorb,mtnc2d>>8,mtnc2d&$ff
	dc.b	cmd_mtncolora,mtnc1d>>8,mtnc1d&$ff
	dc.b	cmd_wait,1+neon
	dc.b	cmd_mtncolora,mtnc1dd>>8,mtnc1dd&$ff
	dc.b	cmd_wait,0+neon
	dc.b	cmd_mtncolorb,mtnc2dd>>8,mtnc2dd&$ff
	dc.b	cmd_mtncolora,mtnc0>>8,mtnc0&$ff
	dc.b	cmd_wait,1+neon
	dc.b	cmd_mtncolorb,mtnc0>>8,mtnc0&$ff
	dc.b	cmd_wait,4+neon
	dc.b	cmd_mtncolorb,mtnc2>>8,mtnc2&$ff
	dc.b	cmd_wait,0+neon
	dc.b	cmd_mtncolorb,mtnc2d>>8,mtnc2d&$ff
	dc.b	cmd_wait,0+neon
	dc.b	cmd_mtncolorb,mtnc2dd>>8,mtnc2dd&$ff
	dc.b	cmd_wait,0+neon
	dc.b	cmd_mtncolorb,mtnc0>>8,mtnc0&$ff

	dc.b	cmd_wait,100 ; empty screen
	dc.b	cmd_quit
	dc.b	cmd_wait,255 ; you never know ;-)

	even


*------	COPPER INSTRUCTION LIST 1 SRF -----------------------------------------*

clist1	dc.w	$1007,$fffe ; chance for player to alter clist in time
clist1bpl
	dc.w	$00e0,0,$00e2,0
	dc.w	$00e4,0,$00e6,0

spritepointersclist1
	dc.w	$0120,0,$0122,0
	dc.w	$0124,0,$0126,0
	dc.w	$0128,0,$012a,0
	dc.w	$012c,0,$012e,0
	dc.w	$0130,0,$0132,0
	dc.w	$0134,0,$0136,0
	dc.w	$0138,0,$013a,0
	dc.w	$013c,0,$013e,0

	dc.w	$008e,$2c81
	dc.w	$0090,$2cc1
	dc.w	$0092,$0038
	dc.w	$0094,$00d0

	dc.w	$0100,$2200

	dc.w	$0102,$0000
	dc.w	$0104,$0000 ; sprites have no priority

	dc.w	$0108,$0000
	dc.w	$010a,$0000

srfcolor0
	dc.w	$0180,mtnc0 ; will be set to $0000 and back to mtnc0 again
srfcolor1
	dc.w	$0182,mtnc1
srfcolor2
	dc.w	$0184,mtnc2
	dc.w	$0186,mtnc0

	dc.w	$ffff,$fffe
clist1end


*------	COPPER INSTRUCTION LIST 2 MTN -----------------------------------------*

clist2	dc.w	$1007,$fffe ; chance for player to alter clist in time
clist2bpl
	dc.w	$00e0,0,$00e2,0
	dc.w	$00e4,0,$00e6,0
	dc.w	$00e8,0,$00ea,0

spritepointersclist2
	dc.w	$0120,0,$0122,0
	dc.w	$0124,0,$0126,0
	dc.w	$0128,0,$012a,0
	dc.w	$012c,0,$012e,0
	dc.w	$0130,0,$0132,0
	dc.w	$0134,0,$0136,0
	dc.w	$0138,0,$013a,0
	dc.w	$013c,0,$013e,0

	dc.w	$008e,$2c81
	dc.w	$0090,$2cc1
	dc.w	$0092,$0038
	dc.w	$0094,$00d0

	dc.w	$0102,$0000
	dc.w	$0104,$0000 ; sprites have no priority

	dc.w	$0108,$0000
	dc.w	$010a,$0000

	dc.w	$0100,$3200

mtnc0	equ	$0102
mtnc1	equ	$0f36
mtnc2	equ	$0aaf

mtnc1d	equ	$0d14	; dark
mtnc1dd	equ	$0b02	; dark dark

mtnc2d	equ	$088d	; dark
mtnc2dd	equ	$066b	; dark dark

	dc.w	$0180,mtnc0
mtncolora
	dc.w	$0182,mtnc1
	dc.w	$0184,mtnc1
	dc.w	$0186,mtnc1
	dc.w	$0188,mtnc2
	dc.w	$018a,mtnc2
	dc.w	$018c,mtnc1
	dc.w	$018e,mtnc1
	
	dc.w	$9e07,$fffe
mtncolorb
	dc.w	$0182,mtnc2

	dc.w	$ffff,$fffe
clist2end


*------	COPPER INSTRUCTION LIST 3 LMS -----------------------------------------*

clist3	dc.w	$1007,$fffe ; chance for player to alter clist in time
clist3bpl
	dc.w	$00e0,0,$00e2,0
	dc.w	$00e4,0,$00e6,0

spritepointersclist3
	dc.w	$0120,0,$0122,0
	dc.w	$0124,0,$0126,0
	dc.w	$0128,0,$012a,0
	dc.w	$012c,0,$012e,0
	dc.w	$0130,0,$0132,0
	dc.w	$0134,0,$0136,0
	dc.w	$0138,0,$013a,0
	dc.w	$013c,0,$013e,0

	dc.w	$008e,$2c81
	dc.w	$0090,$2cc1
	dc.w	$0092,$0038
	dc.w	$0094,$00d0

	dc.w	$0100,$2200

	dc.w	$0108,$0000
	dc.w	$010a,$0000

	dc.w	$0180,mtnc0
	dc.w	$0182,mtnc1
clist3cols
	dc.w	$0184,mtnc2
	dc.w	$0186,mtnc2

clist3rl
	ds.w	numrasterlines*6

	dc.w	$ffff,$fffe
clist3end


*------	COPPER INSTRUCTION LIST 4 TXT -----------------------------------------*

clist4	dc.w	$1007,$fffe ; chance for player to alter clist in time
clist4bpl
	dc.w	$00e0,0,$00e2,0
	dc.w	$00e4,0,$00e6,0
	dc.w	$00e8,0,$00ea,0
	dc.w	$00ec,0,$00ee,0
clist4bpl4
	dc.w	$00f0,0,$00f2,0

spritepointersclist4
	dc.w	$0120,0,$0122,0
	dc.w	$0124,0,$0126,0
	dc.w	$0128,0,$012a,0
	dc.w	$012c,0,$012e,0
	dc.w	$0130,0,$0132,0
	dc.w	$0134,0,$0136,0
	dc.w	$0138,0,$013a,0
	dc.w	$013c,0,$013e,0

	dc.w	$008e,$2c81
	dc.w	$0090,$2cc1
	dc.w	$0092,$0038
	dc.w	$0094,$00d0

	dc.w	$0100,$5200
	dc.w	$0102,$0000

	dc.w	$0108,$0000
	dc.w	$010a,$0000

	dc.w	$0180,mtnc0
	dc.w	$0182,mtnc0
	dc.w	$0184,mtnc0
	dc.w	$0186,mtnc0
	dc.w	$0188,mtnc0
	dc.w	$018a,mtnc0
	dc.w	$018c,mtnc0
	dc.w	$018e,mtnc0

	dc.w	$0190,mtnc0
	dc.w	$0192,mtnc0
	dc.w	$0194,mtnc0
	dc.w	$0196,mtnc0
	dc.w	$0198,mtnc0
	dc.w	$019a,mtnc0
	dc.w	$019c,mtnc0
	dc.w	$019e,mtnc0

	dc.w	$ab07,$fffe
	dc.w	$0108,-2*pwidth
	dc.w	$010a,-2*pwidth

	dc.w	$ffdf,$fffe
	dc.w	$2b07,$fffe
	dc.w	$0100,$0200 ; hide trash on line $2c

	dc.w	$ffff,$fffe
clist4end


*------	COPPER INSTRUCTION LIST 5 LADD ----------------------------------------*

clist5	dc.w	$1007,$fffe ; chance for player to alter clist in time
clist5bpl
	dc.w	$00e0,0,$00e2,0
spritepointersclist5
	dc.w	$0120,0,$0122,0
	dc.w	$0124,0,$0126,0
	dc.w	$0128,0,$012a,0
	dc.w	$012c,0,$012e,0
	dc.w	$0130,0,$0132,0
	dc.w	$0134,0,$0136,0
	dc.w	$0138,0,$013a,0
	dc.w	$013c,0,$013e,0

	dc.w	$008e,$2c81
	dc.w	$0090,$2cc1
	dc.w	$0092,$0038
	dc.w	$0094,$00d0

	dc.w	$0100,$1200
	dc.w	$0102,$0000

	dc.w	$0108,$0000
	dc.w	$010a,$0000

	dc.w	$0180,$0000
	dc.w	$0182,$05fa

line	equ	$eb	; MCCOY
	dc.b	line,$07,$ff,$fe

clist5bpl2
	dc.w	$00e0,0,$00e2,0
	dc.w	$00e4,0,$00e6,0
	dc.w	$00e8,0,$00ea,0
	dc.w	$00ec,0,$00ee,0

	dc.w	$0100,$c200	; 4 hires bitplanes
	dc.w	$0102,$00dd

laddcl1	dc.w	$0182,0
	dc.w	$0184,0
	dc.w	$0186,0
	dc.w	$0188,0
	dc.w	$018a,0
	dc.w	$018c,0
	dc.w	$018e,0
	dc.w	$0190,0
	dc.w	$0192,0
	dc.w	$0194,0
	dc.w	$0196,0
	dc.w	$0198,0
	dc.w	$019a,0
	dc.w	$019c,0
	dc.w	$019e,0

	dc.b	(line+13)&$ff,$07,$ff,$fe ; gap
	dc.w	$0100,$9200	; 1 hires bitplane
	dc.w	$0182,$0000
	dc.w	$0108,-laddpwidth
	dc.w	$010a,-laddpwidth

	dc.w	$fb07,$fffe	; SPREADING...
	dc.w	$0100,$c200	; 4 hires bitplanes
	dc.w	$0108,$0000
	dc.w	$010a,$0000

laddcl2	dc.w	$0182,0
	dc.w	$0184,0
	dc.w	$0186,0
	dc.w	$0188,0
	dc.w	$018a,0
	dc.w	$018c,0
	dc.w	$018e,0
	dc.w	$0190,0
	dc.w	$0192,0
	dc.w	$0194,0
	dc.w	$0196,0
	dc.w	$0198,0
	dc.w	$019a,0
	dc.w	$019c,0
	dc.w	$019e,0

	dc.w	$ffdf,$fffe
	dc.w	$0407,$fffe
	dc.w	$0100,$1200
	dc.w	$0182,$0000

	dc.w	$ffff,$fffe
clist5end


*------	CLEAR SCREEN ----------------------------------------------------------*

cpuclslines	equ	240
cpuclschunk	equ	10

cls	move.l	v_db1b2b(a5),a4			;
	bsr	waitblitter			;

	move.w	#$0000,$66(a6)			; modulo D
	move.l	#$01000000,$40(a6)		; bltcon0 bltcon1
	move.l	a4,$54(a6)			; destination D
	move.w	#((pheight*2)-cpuclslines)<<6+pwidth>>1,$58(a6) ; bltsize and start

	add.w	#2*psize,a4			; end of bitplanes
	moveq	#(cpuclslines/cpuclschunk)-1,d7	; 24 * 10 lines
	movem.l	v_zero(a5),d0-d6/a0-a2		; 10 registers * 4 bytes = 40 (1 line)
.cpucls	
	rept cpuclschunk
	movem.l	d0-d6/a0-a2,-(a4)		; clears 1 line (pwidth)
	endr
	dbf	d7,.cpucls			;

	if timing
	move.w	#$0fff,$180(a6)			; white should be a few lines only
	endif
	bsr	waitblitter			;
	if timing
	move.w	#$0000,$180(a6)			;
	endif

	rts					;


*------	MEMORY MANAGEMENT -----------------------------------------------------*

BESTMEMORY	equ	0
MEMF_CHIP	equ	1<<1
MEMF_CLEAR	equ	1<<16

clist1size	equ	clist1end-clist1
clist2size	equ	clist2end-clist2
clist3size	equ	clist3end-clist3
clist4size	equ	clist4end-clist4
clist5size	equ	clist5end-clist5
lspbanksize	equ	lspbankend-lspbank
spritedatasize	equ	spritedataend-spritedata
laddbitmapssize	equ	laddbitmapsend-laddbitmaps

; note: MEMF_CLEAR for extra safety
memtable
b_clist1	dc.l	0,MEMF_CHIP+MEMF_CLEAR,clist1size
b_clist2	dc.l	0,MEMF_CHIP+MEMF_CLEAR,clist2size
b_clist3	dc.l	0,MEMF_CHIP+MEMF_CLEAR,clist3size
b_clist4	dc.l	0,MEMF_CHIP+MEMF_CLEAR,clist4size
b_clist5	dc.l	0,MEMF_CHIP+MEMF_CLEAR,clist5size
b_lspbank	dc.l	0,MEMF_CHIP+MEMF_CLEAR,lspbanksize
b_spritedata	dc.l	0,MEMF_CHIP+MEMF_CLEAR,spritedatasize
b_laddbitmaps	dc.l	0,MEMF_CHIP+MEMF_CLEAR,laddbitmapssize

memtable2
b_bitplanes	dc.l	0,MEMF_CHIP+MEMF_CLEAR,safetyzone+psize+4*psize
b_ringplanes	dc.l	0,MEMF_CHIP+MEMF_CLEAR,numringplanes*ringplaneheight*pwidth
b_vars		dc.l	0,BESTMEMORY+MEMF_CLEAR,sizeofvars
b_sintab	dc.l	0,BESTMEMORY+MEMF_CLEAR,2048+512
b_lmsvertices	dc.l	0,BESTMEMORY+MEMF_CLEAR,numvertices*2*2 ; x,y (words)
b_lmsanimation	dc.l	0,MEMF_CHIP+MEMF_CLEAR,numvertices*2*numanimsteps ; x,y (49152 bytes)

;b_testoutofmem	dc.l	0,MEMF_CHIP,600000
memtableend

entrysize 	equ	12 ; one entry in the memtable is 12 bytes large (3 longwords)
entries		equ	(memtableend-memtable)/entrysize
entrieschip	equ	(memtable2-memtable)/entrysize

allocandinit
	lea	clist1(pc),a1			;
	move.l	AbsExecBase.w,a6		;
	jsr	TypeOfMem(a6)			;
	btst	#1,d0				; chipmem?
	beq	.notchipmem			;

	lea	clist1(pc),a0			; mark data that is in chipmen already
	lea	b_clist1(pc),a1			;
	move.l	a0,(a1)				;

	lea	clist2(pc),a0			;
	lea	b_clist2(pc),a1			;
	move.l	a0,(a1)				;

	lea	clist3(pc),a0			;
	lea	b_clist3(pc),a1			;
	move.l	a0,(a1)				;

	lea	clist4(pc),a0			;
	lea	b_clist4(pc),a1			;
	move.l	a0,(a1)				;

	lea	clist5(pc),a0			;
	lea	b_clist5(pc),a1			;
	move.l	a0,(a1)				;

	lea	base(pc),a0			;
	add.l	#lspbank-base,a0		;
	lea	b_lspbank(pc),a1		;
	move.l	a0,(a1)				;

	lea	spritedata(pc),a0		;
	lea	b_spritedata(pc),a1		;
	move.l	a0,(a1)				;
	
	lea	laddbitmaps(pc),a0		;
	lea	b_laddbitmaps(pc),a1		;
	move.l	a0,(a1)				;
.notchipmem
	lea	memtable(pc),a5			;
	moveq	#entries-1,d7			;
.loop	tst.l	(a5)				; not to be allocated?
	bne	.noalloc			;
	move.l	8(a5),d0			; bytesize
	move.l	4(a5),d1			; requirements
	move.l	AbsExecBase.w,a6		;
	jsr	AllocMem(a6)			;
	move.l	d0,(a5)				;
	beq	.printerrorandfreemem		; out of memory
.noalloc	
	add.w	#entrysize,a5			; next entry
	dbf	d7,.loop			;
	bsr	init				;
	moveq	#0,d0				; ok, all entries allocated
	rts					;

.printerrorandfreemem
	bsr	printoutofmemory		;
dealloc	move.l	AbsExecBase.w,a6		;
	jsr	TypeOfMem(a6)			;
	lea	memtable(pc),a5			;
	moveq	#entries-1,d7			;
	btst	#1,d0				; chipmem?
	beq	.loop				; we are not in chipmem so free all entries
	lea	memtable2(pc),a5		;
	moveq	#entries-entrieschip-1,d7	;
.loop	tst.l	(a5)				; end of memtable?
	beq	.done				;
	move.l	(a5),a1				; address of memory block
	move.l	8(a5),d0			; bytesize
	move.l	AbsExecBase.w,a6		;
	jsr	FreeMem(a6)			;
	add.w	#entrysize,a5			;
	dbf	d7,.loop			;
.done	moveq	#-1,d0				; alloc error
	rts					;

init	lea	base(pc),a0			; copy lspbank to chip memory
	add.l	#lspbank-base,a0		;
	move.l	b_lspbank(pc),a1		;
	move.l	#lspbanksize,d0			;
.copylspbank
	move.b	(a0)+,(a1)+			;
	subq.l	#1,d0				;
	bne	.copylspbank			;

	move.l	b_vars(pc),a5			;

	move.l	b_bitplanes(pc),a0		;
	add.w	#safetyzone,a0			;
	move.l	a0,v_bitplane(a5)		; static not double buffered bitplane
	add.w	#psize,a0			;
	lea	v_db1a2a(a5),a1			;
	move.l	a0,(a1)+			;
	add.w	#2*psize,a0			;
	move.l	a0,(a1)				; v_db1b2b

	move.l	b_lmsanimation(pc),v_txtbuffer(a5) ; txt buffers

	lea	base(pc),a1			; init punks
	move.l	a1,d1				;
	lea	punks(pc),a0			;
	move.w	#(punksend-punks)/4-1,d7	;
.ploop	add.l	d1,(a0)+			;
	dbf	d7,.ploop			;

	lea	base(pc),a1			;
	move.l	a1,d1				;
	lea	instactions(pc),a0		;
	move.w	#numinstactions-1,d7		;
.iloop	tst.l	(a0)				;
	beq	.void				;
	add.l	d1,(a0)				;
.void	addq.w	#6,a0				;
	dbf	d7,.iloop			;

	lea	mtns(pc),a0			;
	lea	v_mtns(a5),a1			;
	moveq	#nummtns-1,d7			;
.mloop	move.l	(a0)+,a2			;
	moveq	#8-1,d6				; init mtn struct with 7 word data values
.minit	move.w	(a2)+,(a1)+			;
	dbf	d6,.minit			;
	move.l	a2,(a1)+			; mtn_data
	dbf	d7,.mloop			;

	lea	clist1(pc),a0			; copy clist 1 to chip memory
	move.l	b_clist1(pc),a1			;
	move.w	#clist1size-1,d7		;
.copyc1	move.b	(a0)+,(a1)+			;
	dbf	d7,.copyc1			;

	lea	clist2(pc),a0			; copy clist 2 to chip memory
	move.l	b_clist2(pc),a1			;
	move.w	#clist2size-1,d7		;
.copyc2	move.b	(a0)+,(a1)+			;
	dbf	d7,.copyc2			;

	lea	clist3rl(pc),a1			; init clist3
	move.w	#numrasterlines-1,d7		;
	moveq	#$2b,d0				;
.looprl	move.b	d0,(a1)+			;
	move.b	#$e1,(a1)+			; wait on the right side
	move.l	#$fffe0102,(a1)+		;
	move.w	#$0077,(a1)+			; cosmic rain invisible by default
	move.l	#$01820000,(a1)+		;
	addq.b	#1,d0				; next line
	dbf	d7,.looprl			;

	lea	clist3(pc),a0			; copy clist 3 to chip memory
	move.l	b_clist3(pc),a1			;
	move.w	#clist3size-1,d7		;
.copyc3	move.b	(a0)+,(a1)+			;
	dbf	d7,.copyc3			;

	lea	clist4(pc),a0			; copy clist 4 to chip memory
	move.l	b_clist4(pc),a1			;
	move.w	#clist4size-1,d7		;
.copyc4	move.b	(a0)+,(a1)+			;
	dbf	d7,.copyc4			;

	lea	clist5(pc),a0			; copy clist 5 to chip memory
	move.l	b_clist5(pc),a1			;
	move.w	#clist5size-1,d7		;
.copyc5	move.b	(a0)+,(a1)+			;
	dbf	d7,.copyc5			;

	lea	spritedata(pc),a0		; copy sprite data to chip memory
	move.l	b_spritedata(pc),a1		;
	move.w	#spritedatasize-1,d7		;
.copysd	move.b	(a0)+,(a1)+			;
	dbf	d7,.copysd			;

	lea	laddbitmaps(pc),a0		; copy Ladd bitmaps to chip memory
	move.l	b_laddbitmaps(pc),a1		;
	move.w	#laddbitmapssize-1,d7		;
.copylb	move.b	(a0)+,(a1)+			;
	dbf	d7,.copylb			;

	move.l	b_spritedata(pc),d0		;
	move.l	b_clist1(pc),a0			; sprites clist 1
	add.w	#spritepointersclist1+2-clist1,a0 ;
	bsr	setspritepointers		;

	move.l	b_clist2(pc),a0			; sprites clist 2
	add.w	#spritepointersclist2+2-clist2,a0 ;
	bsr	setspritepointers		;

	move.l	b_clist3(pc),a0			; sprites clist 3
	add.w	#spritepointersclist3+2-clist3,a0 ;
	bsr	setspritepointers		;

	move.l	b_clist4(pc),a0			; sprites clist 4
	add.w	#spritepointersclist4+2-clist4,a0 ;
	bsr	setspritepointers		;

	move.l	b_clist5(pc),a0			; sprites clist 5
	add.w	#spritepointersclist5+2-clist5,a0 ;
	bsr	setspritepointers		;

	move.l	b_clist4(pc),a1			; bitplane pointers clist 4
	add.w	#clist4bpl+2-clist4,a1		;
	move.l	b_ringplanes(pc),a0		;
	moveq	#4-1,d7				; 4 ringsplanes
.initrp	move.l	a0,d0				;
	move.w	d0,4(a1)			;
	swap	d0				;
	move.w	d0,(a1)				;
	addq.w	#8,a1				;
	add.w	#ringplaneheight*pwidth,a0	; next ringplane
	dbf	d7,.initrp			;

	move.l	b_clist5(pc),a1			; bitplane pointers clist 5
	add.w	#clist5bpl+2-clist5,a1		;
	move.l	v_db1a2a(a5),d0			;
	move.w	d0,4(a1)			;
	swap	d0				;
	move.w	d0,(a1)				;

	move.l	b_clist5(pc),a1			; bitplane pointers clist 5 Ladd
	add.w	#clist5bpl2+2-clist5,a1		;
	move.l	b_laddbitmaps(pc),a0		;
	moveq	#4-1,d7				; 4 ringsplanes
.initlp	move.l	a0,d0				;
	move.w	d0,4(a1)			;
	swap	d0				;
	move.w	d0,(a1)				;
	addq.w	#8,a1				;
	add.w	#22*laddpwidth,a0		;
	dbf	d7,.initlp			;

	bsr	generatesintab			; sintab

	moveq	#0,d7				; generate byte mirror
.mirror	moveq	#0,d6				; resulting mirrored byte value
	moveq	#7,d0				; highest bit
	moveq	#0,d1				; lowest bit
.miloop	btst	d0,d7				;
	beq	.nset				;
	bset	d1,d6				;
.nset	addq.b	#1,d1				;
	subq.b	#1,d0				;
	bpl	.miloop				;	
	move.b	d6,v_txtmirror(a5,d7.w)		;
	addq.b	#1,d7				; = addq.w #1,d7, cmp.w	#256,d7
	bne	.mirror				; not yet $00 (= $100)

	lea	playcmds(pc),a0			; init player
	move.l	a0,v_cmdspointer(a5)		;
	STARTACTOR actor_player			;

	move.w	#1*4,v_rayindexadd1(a5)		; important: multiple of 4
	move.w	#%10,v_raycolor1a(a5)		; current/previous color is 11 xor 10 = 01 (red)
	move.w	#%01,v_raycolor1b(a5)		; (red)
	lea	v_rays+(2*sizeofray)(a5),a0	; ray 3
	move.l	a0,v_rayprevious1(a5)		;

	move.w	#100*4,v_rayindex2(a5)		; important: multiple of 4
	move.w	#1*4,v_rayindexadd2(a5)		;
	move.w	#%11,v_raycolor2a(a5)		; previous color is 01 xor 11 = 10
	move.w	#%10,v_raycolor2b(a5)		;
	lea	v_rays+(0*sizeofray)(a5),a0	; ray 1
	move.l	a0,v_rayprevious2(a5)		;

	move.w	#200*4,v_rayindex3(a5)		; important: multiple of 4
	move.w	#1*4,v_rayindexadd3(a5)		;
	move.w	#%01,v_raycolor3a(a5)		; previous color is 10 xor 01
	move.w	#%11,v_raycolor3b(a5)		;
	lea	v_rays+(1*sizeofray)(a5),a0	; ray 2
	move.l	a0,v_rayprevious3(a5)		;

	if instruments                  	;
	move.w	#$ffff,v_instlow(a5)		;
	endif					;

	move.w	#instbrakedataend-instbrakedata,v_instbdindex(a5) ;

	move.l	#instnop<<16+instnop,v_inst0(a5) ;
	move.l	#instnop<<16+instnop,v_inst2(a5) ;

	bsr	lmsinit				;
	bsr	generateladd			;

	move.w	#laddcl1-clist5+2,v_laddcbank(a5) ;
	STARTACTOR actor_ladd			; start with Ladd effect
	rts					;


*------	GENERATE SINE TABLE ---------------------------------------------------*

; sine table, 1024 angle steps, factor 256

generatesintab
	lea	.sinb(pc),a0			;
	move.l	b_sintab(pc),a1			;
	move.w	#246-1,d7			;
.gensin	moveq	#0,d0				;
	move.b	(a0)+,d0			;
	move.w	d0,(a1)+			;
	dbf	d7,.gensin			;

	move.l	a1,a0				; used for cos
	moveq	#10+11-1,d7			; 10 values for sin, 11 values for cos
.fill256
	move.w	#$0100,(a1)+			;
	dbf	d7,.fill256			;

	move.w	#245-1,d7			; cos
.gencos	move.w	-(a0),(a1)+			;
	dbf	d7,.gencos			;

	move.w	#512-1,d7			;
	move.l	b_sintab(pc),a0			;
.genneg	move.w	(a0)+,d0			;
	neg.w	d0				;
	move.w	d0,(a1)+			;
	dbf	d7,.genneg			;

	move.l	b_sintab(pc),a0			;
	move.w	#256-1,d7			;
.gensin2
	move.w	(a0)+,(a1)+			;
	dbf	d7,.gensin2			;
	rts					;

.sinb	dc.b	$00,$02,$03,$05,$06,$08,$09,$0b
	dc.b	$0d,$0e,$10,$11,$13,$14,$16,$18
	dc.b	$19,$1b,$1c,$1e,$1f,$21,$22,$24
	dc.b	$26,$27,$29,$2a,$2c,$2d,$2f,$30
	dc.b	$32,$33,$35,$37,$38,$3a,$3b,$3d
	dc.b	$3e,$40,$41,$43,$44,$46,$47,$49
	dc.b	$4a,$4c,$4d,$4f,$50,$52,$53,$55
	dc.b	$56,$58,$59,$5b,$5c,$5e,$5f,$61
	dc.b	$62,$63,$65,$66,$68,$69,$6b,$6c
	dc.b	$6d,$6f,$70,$72,$73,$75,$76,$77
	dc.b	$79,$7a,$7b,$7d,$7e,$80,$81,$82
	dc.b	$84,$85,$86,$88,$89,$8a,$8c,$8d
	dc.b	$8e,$90,$91,$92,$93,$95,$96,$97
	dc.b	$99,$9a,$9b,$9c,$9e,$9f,$a0,$a1
	dc.b	$a2,$a4,$a5,$a6,$a7,$a8,$aa,$ab
	dc.b	$ac,$ad,$ae,$af,$b1,$b2,$b3,$b4
	dc.b	$b5,$b6,$b7,$b8,$b9,$ba,$bc,$bd
	dc.b	$be,$bf,$c0,$c1,$c2,$c3,$c4,$c5
	dc.b	$c6,$c7,$c8,$c9,$ca,$cb,$cc,$cd
	dc.b	$ce,$cf,$cf,$d0,$d1,$d2,$d3,$d4
	dc.b	$d5,$d6,$d7,$d7,$d8,$d9,$da,$db
	dc.b	$dc,$dc,$dd,$de,$df,$e0,$e0,$e1
	dc.b	$e2,$e3,$e3,$e4,$e5,$e5,$e6,$e7
	dc.b	$e7,$e8,$e9,$e9,$ea,$eb,$eb,$ec
	dc.b	$ed,$ed,$ee,$ee,$ef,$ef,$f0,$f1
	dc.b	$f1,$f2,$f2,$f3,$f3,$f4,$f4,$f5
	dc.b	$f5,$f5,$f6,$f6,$f7,$f7,$f8,$f8
	dc.b	$f8,$f9,$f9,$f9,$fa,$fa,$fa,$fb
	dc.b	$fb,$fb,$fc,$fc,$fc,$fd,$fd,$fd
	dc.b	$fd,$fd,$fe,$fe,$fe,$fe,$fe,$ff
	dc.b	$ff,$ff,$ff,$ff,$ff,$ff		; 246 values 

	even


*------ GENERATE TEXT ---------------------------------------------------------*

generatetext
	move.l	v_txtbuffer(a5),a1		;
	move.l	a1,v_txtbuffertemp(a5)		;

	move.w	#numgentxtlines-1,d7		;
	lea	v_txtlines(a5),a2		;
.init	move.l	a1,(a2)				; init empty lines
	addq.w	#sizeoftxt,a2			;
	dbf	d7,.init			;

	move.l	a1,a0				;
	add.w	#1*lineheight*txtpwidth,a0	; "4FX "
	lea	lineheight*txtpwidth(a0),a1	; "FX 4"
	lea	lineheight*txtpwidth(a1),a2	; "X 4F"
	lea	lineheight*txtpwidth(a2),a3	; " 4FX"
	
	moveq	#num4fxlines/4-1,d7		;
	lea	v_txtlines+(txtlinepadding*sizeoftxt)(a5),a4 ;
.4fx	move.l	a0,(a4)				;
	addq.w	#sizeoftxt,a4			;
	move.l	a1,(a4)				;
	addq.w	#sizeoftxt,a4			;
	move.l	a2,(a4)				;
	addq.w	#sizeoftxt,a4			;
	move.l	a3,(a4)				;
	addq.w	#sizeoftxt,a4			;
.valid	dbf	d7,.4fx				;

	lea	v_txtlines+((2*txtlinepadding+num4fxlines-6)*sizeoftxt)(a5),a4 ;
	move.l	a4,d5				;
	add.l	#4*sizeoftxt,d5			; this is the four "4FX" text lines
	
.line	move.l	v_txtbuffertemp(a5),a1		;
	cmp.l	d5,a4				; 4FX?
	bls	.skip4fx			;
	move.l	a1,(a4)+			; txt_data
	bra	.continue			;
.skip4fx
	addq.l	#4,a4				; skip 
.continue
	moveq	#0,d6				; width (in bytes) counter
	moveq	#1,d4				;
	ror.b	d4,d4				;
.repeat	move.l	a0,a3				; don't try this at home kids
	move.l	v_txtpointer(a5),a0		; repeat line text
.char	moveq	#0,d0				;
	move.b	(a0)+,d0			;
	beq	.repeat				;
	bmi	.done				; negative (-1) = end of text
	sub.b	#"=",d0				;
	move.b	prop(pc,d0.w),d1		;
	asl.w	#5,d0				; *32 (size of 1 char)
	lea	font(pc,d0.w),a2		;

	moveq	#15,d3				; bit 15
.col	movem.l	a1-a2,-(a7)			;
	moveq	#lineheight-1,d7		;
.row	move.w	(a2),d0				;
	btst.l	d3,d0				;
	beq	.noset				;
	or.b	d4,(a1)				; draw pixel
.noset	addq.w	#2,a2				; next char row
	add.w	#txtpwidth,a1			; next buffer row
	dbf	d7,.row				;
	movem.l	(a7)+,a1-a2			;
	subq.w	#1,d3				; next bit to test in char col

	ror.b	#1,d4				; advance xpos
	bcc	.x				;
	addq.w	#1,a1				; next byte
	addq.w	#1,d6				; width (in bytes) counter
	cmp.w	#txtpwidth,d6			;
	beq	.next				;
.x	subq.b	#1,d1				; one column of char is drawn
	bne	.col				;
	bra	.char				;

.next	clr.w	(a4)+				; clear/skip txt_xpos
	move.l	a3,v_txtpointer(a5)		;

	lea	textgap(pc),a0			;
	cmp.l	a3,a0				;
	bne	.nogap				;
	add.w	#txtlinepadding*sizeoftxt,a4	;
.nogap	
	if timing
	move.w	a0,$180(a6)			;
	endif
	
	add.l	#lineheight*txtpwidth,v_txtbuffertemp(a5) ;
	tst.b	v_doquit(a5)			;
	beq	.line				;
.done	rts					;

; includes 3px padding
prop	dc.b	11		; = 4
	dc.b	11		; > -
	dc.b	5		; ? .
	dc.b	11		; @ space
	dc.b	11,11,11,11	; ABCD
	dc.b	11,11,11,11	; EFGH
	dc.b	5,10,10,10	; IJKL
	dc.b	11,11,11,11	; MNOP
	dc.b	11,11,11,11	; QRST
	dc.b	11,11,11,11	; UVWX
	dc.b	11,11		; YZ


*------ FONT ------------------------------------------------------------------*

font	dc.b	%11000011,0	; ? = 4
	dc.b	%11000011,0
	dc.b	%11000011,0
	dc.b	%11000011,0
	dc.b	%11111111,0
	dc.b	%11111111,0
	dc.b	%00000011,0
	dc.b	%00000011,0
	dc.b	%00000011,0
	dc.b	%00000011,0
	ds.w	6

	dc.b	%00000000,0	; > = -
	dc.b	%00000000,0
	dc.b	%00000000,0
	dc.b	%00000000,0
	dc.b	%11111111,0
	dc.b	%11111111,0
	dc.b	%00000000,0
	dc.b	%00000000,0
	dc.b	%00000000,0
	dc.b	%00000000,0
	ds.w	6

	dc.b	%00000000,0	; ? = .
	dc.b	%00000000,0
	dc.b	%00000000,0
	dc.b	%00000000,0
	dc.b	%00000000,0
	dc.b	%00000000,0
	dc.b	%00000000,0
	dc.b	%00000000,0
	dc.b	%11000000,0
	dc.b	%11000000,0
	ds.w	6

	ds.w	16		; @ = space
	
	dc.b	%11111111,0	; A
	dc.b	%11111111,0
	dc.b	%00000011,0
	dc.b	%00000011,0
	dc.b	%11111111,0
	dc.b	%11111111,0
	dc.b	%11000011,0
	dc.b	%11000011,0
	dc.b	%11000011,0
	dc.b	%11000011,0
	ds.w	6

	dc.b	%11111100,0	; B
	dc.b	%11111100,0
	dc.b	%00001100,0
	dc.b	%00001100,0
	dc.b	%11111111,0
	dc.b	%11111111,0
	dc.b	%00000011,0
	dc.b	%00000011,0
	dc.b	%11111111,0
	dc.b	%11111111,0
	ds.w	6

	dc.b	%11111111,0	; C
	dc.b	%11111111,0
	dc.b	%11000000,0
	dc.b	%11000000,0
	dc.b	%11000000,0
	dc.b	%11000000,0
	dc.b	%11000000,0
	dc.b	%11000000,0
	dc.b	%11111111,0
	dc.b	%11111111,0
	ds.w	6

	dc.b	%11111111,0	; D
	dc.b	%11111111,0
	dc.b	%00000011,0
	dc.b	%00000011,0
	dc.b	%11000011,0
	dc.b	%11000011,0
	dc.b	%11000011,0
	dc.b	%11000011,0
	dc.b	%11111111,0
	dc.b	%11111111,0
	ds.w	6

	dc.b	%11111111,0	; E
	dc.b	%11111111,0
	dc.b	%11000000,0
	dc.b	%11000000,0
	dc.b	%11111111,0
	dc.b	%11111111,0
	dc.b	%11000000,0
	dc.b	%11000000,0
	dc.b	%11111111,0
	dc.b	%11111111,0
	ds.w	6

	dc.b	%11111111,0	; F
	dc.b	%11111111,0
	dc.b	%11000000,0
	dc.b	%11000000,0
	dc.b	%11111111,0
	dc.b	%11111111,0
	dc.b	%11000000,0
	dc.b	%11000000,0
	dc.b	%11000000,0
	dc.b	%11000000,0
	ds.w	6

	dc.b	%11111111,0	; G
	dc.b	%11111111,0
	dc.b	%11000000,0
	dc.b	%11000000,0
	dc.b	%11001111,0
	dc.b	%11001111,0
	dc.b	%11000011,0
	dc.b	%11000011,0
	dc.b	%11111111,0
	dc.b	%11111111,0
	ds.w	6

	dc.b	%11000011,0	; H
	dc.b	%11000011,0
	dc.b	%11000011,0
	dc.b	%11000011,0
	dc.b	%11111111,0
	dc.b	%11111111,0
	dc.b	%11000011,0
	dc.b	%11000011,0
	dc.b	%11000011,0
	dc.b	%11000011,0
	ds.w	6

	dc.b	%11000000,0	; I
	dc.b	%11000000,0
	dc.b	%11000000,0
	dc.b	%11000000,0
	dc.b	%11000000,0
	dc.b	%11000000,0
	dc.b	%11000000,0
	dc.b	%11000000,0
	dc.b	%11000000,0
	dc.b	%11000000,0
	ds.w	6

	dc.b	%00000011,0	; J
	dc.b	%00000011,0
	dc.b	%00000011,0
	dc.b	%00000011,0
	dc.b	%00000011,0
	dc.b	%00000011,0
	dc.b	%00000011,0
	dc.b	%00000011,0
	dc.b	%11111111,0
	dc.b	%11111111,0
	ds.w	6

	dc.b	%11000011,0	; K
	dc.b	%11000011,0
	dc.b	%11000011,0
	dc.b	%11000011,0
	dc.b	%11111111,0
	dc.b	%11111111,0
	dc.b	%11001100,0
	dc.b	%11001100,0
	dc.b	%11001100,0
	dc.b	%11001100,0
	ds.w	6

	dc.b	%11000000,0	; L
	dc.b	%11000000,0
	dc.b	%11000000,0
	dc.b	%11000000,0
	dc.b	%11000000,0
	dc.b	%11000000,0
	dc.b	%11000000,0
	dc.b	%11000000,0
	dc.b	%11111111,0
	dc.b	%11111111,0
	ds.w	6

	dc.b	%11111111,0	; M
	dc.b	%11111111,0
	dc.b	%11110011,0
	dc.b	%11110011,0
	dc.b	%11000011,0
	dc.b	%11000011,0
	dc.b	%11000011,0
	dc.b	%11000011,0
	dc.b	%11000011,0
	dc.b	%11000011,0
	ds.w	6

	dc.b	%11111111,0	; N
	dc.b	%11111111,0
	dc.b	%11000011,0
	dc.b	%11000011,0
	dc.b	%11000011,0
	dc.b	%11000011,0
	dc.b	%11000011,0
	dc.b	%11000011,0
	dc.b	%11000011,0
	dc.b	%11000011,0
	ds.w	6

	dc.b	%11111111,0	; O
	dc.b	%11111111,0
	dc.b	%11000011,0
	dc.b	%11000011,0
	dc.b	%11000011,0
	dc.b	%11000011,0
	dc.b	%00000011,0
	dc.b	%00000011,0
	dc.b	%11111111,0
	dc.b	%11111111,0
	ds.w	6

	dc.b	%11111111,0	; P
	dc.b	%11111111,0
	dc.b	%00000011,0
	dc.b	%00000011,0
	dc.b	%11111111,0
	dc.b	%11111111,0
	dc.b	%11000000,0
	dc.b	%11000000,0
	dc.b	%11000000,0
	dc.b	%11000000,0
	ds.w	6

	dc.b	%11111111,0	; Q
	dc.b	%11111111,0
	dc.b	%11000011,0
	dc.b	%11000011,0
	dc.b	%11000011,0
	dc.b	%11000011,0
	dc.b	%00001111,0
	dc.b	%00001111,0
	dc.b	%11111111,0
	dc.b	%11111111,0
	ds.w	6

	dc.b	%11111111,0	; R
	dc.b	%11111111,0
	dc.b	%11000011,0
	dc.b	%11000011,0
	dc.b	%11001111,0
	dc.b	%11001111,0
	dc.b	%11001100,0
	dc.b	%11001100,0
	dc.b	%11001100,0
	dc.b	%11001100,0
	ds.w	6

	dc.b	%11111111,0	; S
	dc.b	%11111111,0
	dc.b	%11000000,0
	dc.b	%11000000,0
	dc.b	%11111111,0
	dc.b	%11111111,0
	dc.b	%00000011,0
	dc.b	%00000011,0
	dc.b	%11111111,0
	dc.b	%11111111,0
	ds.w	6

	dc.b	%11111111,0	; T
	dc.b	%11111111,0
	dc.b	%00000011,0
	dc.b	%00000011,0
	dc.b	%00000011,0
	dc.b	%00000011,0
	dc.b	%00000011,0
	dc.b	%00000011,0
	dc.b	%00000011,0
	dc.b	%00000011,0
	ds.w	6

	dc.b	%11000011,0	; U
	dc.b	%11000011,0
	dc.b	%11000011,0
	dc.b	%11000011,0
	dc.b	%11000011,0
	dc.b	%11000011,0
	dc.b	%11000011,0
	dc.b	%11000011,0
	dc.b	%11111111,0
	dc.b	%11111111,0
	ds.w	6

	dc.b	%11000011,0	; V
	dc.b	%11000011,0
	dc.b	%11000011,0
	dc.b	%11000011,0
	dc.b	%11000011,0
	dc.b	%11000011,0
	dc.b	%11001111,0
	dc.b	%11001111,0
	dc.b	%11111100,0
	dc.b	%11111100,0
	ds.w	6

	dc.b	%11000011,0	; W
	dc.b	%11000011,0
	dc.b	%11000011,0
	dc.b	%11000011,0
	dc.b	%11000011,0
	dc.b	%11000011,0
	dc.b	%11110011,0
	dc.b	%11110011,0
	dc.b	%11111111,0
	dc.b	%11111111,0
	ds.w	6

	dc.b	%00110011,0	; X
	dc.b	%00110011,0
	dc.b	%00110011,0
	dc.b	%00110011,0
	dc.b	%11111111,0
	dc.b	%11111111,0
	dc.b	%11001100,0
	dc.b	%11001100,0
	dc.b	%11001100,0
	dc.b	%11001100,0
	ds.w	6

	dc.b	%11000011,0	; Y
	dc.b	%11000011,0
	dc.b	%11000011,0
	dc.b	%11000011,0
	dc.b	%11111111,0
	dc.b	%11111111,0
	dc.b	%00011000,0
	dc.b	%00011000,0
	dc.b	%00011000,0
	dc.b	%00011000,0
	ds.w	6

	dc.b	%11111111,0	; Z
	dc.b	%11111111,0
	dc.b	%00000011,0
	dc.b	%00000011,0
	dc.b	%11111111,0
	dc.b	%11111111,0
	dc.b	%11000000,0
	dc.b	%11000000,0
	dc.b	%11111111,0
	dc.b	%11111111,0
	ds.w	6


*------ TEXT ------------------------------------------------------------------*

; 49152 / 494 (lineheight*txtpwidth)= max 99 lines

text	dc.b	"@",0

	dc.b	"=FX@",0
	dc.b	"FX@=",0
	dc.b	"X@=F",0
	dc.b	"@=FX",0

	dc.b	"ABYSS@",0
	dc.b	"ABYSS>CONNECTION@",0
	dc.b	"AFWD@",0
	dc.b	"ALCATRAZ@",0
	dc.b	"ALTAIR@",0
	dc.b	"AMIGABILL@",0
	dc.b	"ANDROMEDA@",0
	dc.b	"ARTSTATE@",0
	dc.b	"ATTENTIONWHORE@",0
	dc.b	"BATMAN@GROUP@",0
	dc.b	"BINARY@",0
	dc.b	"DEKADENCE@",0
	dc.b	"DEMOZOO@",0
	dc.b	"DESIRE@",0
	dc.b	"ECHTZEIT@",0
	dc.b	"FIVE@FINGER@PUNCH@",0
	dc.b	"GHOSTOWN@",0
	dc.b	"ISTARI@",0
	dc.b	"KESTRA@BITWORLD@",0
	dc.b	"LEMMY@",0
	dc.b	"LEMON?@",0
	dc.b	"LOONIES@",0
	dc.b	"MCCOY@",0
	dc.b	"MELON@",0
	dc.b	"NAH>KOLOR@",0
	dc.b	"NEW@GENERATION@CREW@",0
	dc.b	"OXYGENE@",0
	dc.b	"POUET@",0
	dc.b	"REBELS@",0
	dc.b	"RESISTANCE@",0
	dc.b	"SCA@",0
	dc.b	"SCOOPEX@",0
	dc.b	"SLIPSTREAM@",0
	dc.b	"SOFTWARE@FAILURE@",0
	dc.b	"SPACEBALLS@",0
	dc.b	"SPECTROX@",0
	dc.b	"TEK@",0
	dc.b	"THE@TWITCH@ELITE@",0
	dc.b	"TPOLM@",0
	dc.b	"TRSI@",0
	dc.b	"UP@ROUGH@",0
	dc.b	"VISION@FACTORY@",0
	dc.b	"VOID@",0
	dc.b	"ZODIAC@",0
	dc.b	"ZYMOSIS@",0
textgap	dc.b	"MUSIC@",0
	dc.b	"VIRGILL@",0
	dc.b	"@",0
	dc.b	"@",0
	dc.b	"INSPIRATION@",0
	dc.b	"MNEMOTRON@",0
	dc.b	"@",0
	dc.b	"@",0
	dc.b	"CODE@AND@GFX@",0
	dc.b	"DEPECHE@",0
	dc.b	-1 ; end of text

	even


*------	PRINT OUT OF MEMORY ---------------------------------------------------*

printoutofmemory
	lea	.dos(pc),a1			;
	move.l	AbsExecBase.w,a6		;
	jsr	OldOpenLibrary(a6)		;
	move.l	d0,a6				;
	beq	.error				;
	jsr	Output(a6)			;
	move.l	d0,d1				;
	beq	.error				;
	moveq	#.textend-.text,d3		; length
	lea	.text(pc),a1			;
	move.l	a1,d2				;
	jsr	Write(a6)			;
	tst.l	d0				;
	beq	.error				;
	move.l	a6,a1				;
	move.l	AbsExecBase.w,a6		;
	jsr	CloseLibrary(a6)		;
.error	moveq	#0,d0				;
	rts					;

.dos	dc.b	"dos.library",0
.text	dc.b	"Error: Could not allocate enough memory",10
.textend
	even


;*****************************************************************
;
;	Light Speed Player v1.13 (modified)
;	Fastest Amiga MOD player ever :)
;	Written By Arnaud Carré (aka Leonard / OXYGENE)
;	https://github.com/arnaud-carre/LSPlayer
;	X: @leonard_coder
;
;	--------How to use--------- 
;
;	bsr LSP_MusicDriver_CIA_Start : Init LSP player code and install CIA interrupt
;		a2: VBR (CPU Vector Base Register) ( use 0 if 68000 )
;
;	bsr LSP_MusicDriver_CIA_Stop : Stop LSP music replay
;*****************************************************************

LSP_MusicDriver_CIA_Start
	lea	.irqVector(pc),a3
	lea	$78(a2),a2
	move.l	a2,(a3)
	lea	.LSPDmaCon+1(pc),a2		; DMACON byte patch address
	bsr	LSP_MusicInit			; init the LSP player ( whatever fast or insane version )

	lea	.pMusicBPM(pc),a2
	move.l	a0,(a2)				; store music BPM pointer
	move.w	(a0),d0				; start BPM
	lea	.curBpm(pc),a2
	move.w	d0,(a2)
	bsr	.LSP_IrqInstall
	rts

.LSPDmaCon	dc.w	$8000
.irqVector	dc.l	0
.ciaClock	dc.l	0
.curBpm		dc.w	0
.pMusicBPM	dc.l	0

; d0: music BPM
.LSP_IrqInstall
	move.w	#(1<<13),$dff09a		; disable CIA interrupt
	lea	.LSP_MainIrq(pc),a0
	move.l	.irqVector(pc),a4
	move.l	a0,(a4)

	lea	$bfd000,a0
	move.b 	#$7f,$d00(a0)
	move.b 	#$10,$e00(a0)
	move.b 	#$10,$f00(a0)
	move.l	#1773447,d1			; PAL clock

	lea	.ciaClock(pc),a4
	move.l	d1,(a4)
	divu.w	d0,d1
	move.b	d1,$400(a0)
	lsr.w 	#8,d1
	move.b	d1,$500(a0)
	move.b	#$83,$d00(a0)
	move.b	#$11,$e00(a0)
			
	move.b	#496&255,$600(a0)		; set timer B to 496 (to set DMACON)
	move.b	#496>>8,$700(a0)

	move.w 	#(1<<13),$dff09c		; clear any req CIA
	move.w 	#$a000,$dff09a			; CIA interrupt enabled
	rts

.LSP_MainIrq
	btst.b	#0,$bfdd00
	beq	.skipa

	movem.l	d0-a6,-(a7)
	lea	custom,a6
	CHECKACTOR actor_lsp
	beq	.mute
	
	if numbers
	tst.w	v_number(a5)
	bne	.done
	move.w	v_frame(a5),v_number(a5)
	clr.w	v_frame(a5)
.done
	endif
	
	bsr	LSP_MusicPlayTick		; LSP main music driver tick

	; check if BMP changed in the middle of the music
.mute	move.l	.pMusicBPM(pc),a0
	move.w	(a0),d0				; current music BPM
	cmp.w	.curBpm(pc),d0
	beq	.noChg
	lea	.curBpm(pc),a2			
	move.w	d0,(a2)				; current BPM
	move.l	.ciaClock(pc),d1
	divu.w	d0,d1
	move.b	d1,$bfd400
	lsr.w 	#8,d1
	move.b	d1,$bfd500			

.noChg	lea	.LSP_DmaconIrq(pc),a0
	move.l	.irqVector(pc),a1
	move.l	a0,(a1)
	move.b	#$19,$bfdf00			; start timer B, one shot

	movem.l	(a7)+,d0-a6
.skipa	move.w	#$2000,$dff09c
	nop
	rte

.LSP_DmaconIrq
	btst.b	#1,$bfdd00
	beq	.skipb
	move.w	.LSPDmaCon(pc),$dff096
	pea	(a0)
	move.l	.irqVector(pc),a0
	pea	.LSP_MainIrq(pc)
	move.l	(a7)+,(a0)
	move.l	(a7)+,a0
.skipb	move.w	#$2000,$dff09c
	nop
	rte

LSP_MusicDriver_CIA_Stop
	move.b	#$7f,$bfdd00
	move.w	#$2000,$9a(a6)
	move.w	#$2000,$9c(a6)
	move.w	#$000f,$96(a6)
	rts


;------------------------------------------------------------------
;
;	LSP_MusicInit
;
;		In:	a0: LSP music data (any memory)
;			a1: LSP sound bank (chip memory)
;			a2: DMAcon patch
;		Out:a0: music BPM pointer (16bits)
;			d0: music len in tick count
;
;------------------------------------------------------------------
LSP_MusicInit
	lea	base(pc),a0			; a0: music data (any mem) + 10
	add.l	#lspmusic-base,a0		;
	move.l	b_lspbank(pc),d1		; a1: sound bank data (chip mem)

	lea	LSP_State(pc),a3
	move.l	a2,m_dmaconPatch(a3)
	
	move.l	a0,a4				; relocation flag ad
	addq.w	#2,a0				; skip relocation flag
	move.w	(a0)+,m_currentBpm(a3)		; default BPM
	move.w	(a0)+,m_escCodeRewind(a3)
	move.w	(a0)+,m_escCodeSetBpm(a3)
	move.w	(a0)+,m_escCodeGetPos(a3)
	move.l	(a0)+,-(a7)			; music len in frame ticks
	move.w	(a0)+,d0			; instrument count
	lea	-12(a0),a2			; LSP data has -12 offset on instrument tab (win 2 cycles in insane player)
	move.l	a2,m_lspInstruments(a3)		; instrument tab addr (minus 4)
	subq.w	#1,d0
	move.l	a0,a1				; keep relocated flag
.relocLoop
	tst.b	(a4)				; relocation guard
	bne	.relocated
	add.l	d1,(a0)
	add.l	d1,6(a0)
.relocated
	lea	12(a0),a0
	dbf	d0,.relocLoop
	move.w	(a0)+,d0			; codes table size
	move.l	a0,m_codeTableAddr(a3)		; code table
	add.w	d0,d0
	add.w	d0,a0

	; read sequence timing infos (if any)
	move.w	(a0)+,m_seqCount(a3)
	beq	.noSeq
	move.l	a0,m_seqTable(a3)
	clr.w	m_currentSeq(a3)
	move.w	m_seqCount(a3),d0
	moveq	#0,d1
	move.w	d0,d1
	lsl.w	#3,d1				; 8 bytes per entry
	add.w	#12,d1				; add 3 last 32bits (word stream size, byte stream loop, word stream loop)
	add.l	a0,d1				; word stream data address
	subq.w	#1,d0
.seqRel	tst.b	(a4)
	bne	.skipRel
	add.l	d1,(a0)
	add.l	d1,4(a0)
.skipRel
	addq.w	#8,a0
	dbf	d0,.seqRel

.noSeq	movem.l	(a0)+,d0-d2			; word stream size, byte stream loop point, word stream loop point
	st	(a4)				; mark this music score as "relocated"
	move.l	a0,m_wordStream(a3)
	lea	(a0,d0.l),a1			; byte stream
	move.l	a1,m_byteStream(a3)
	add.l	d2,a0
	add.l	d1,a1
	move.l	a0,m_wordStreamLoop(a3)
	move.l	a1,m_byteStreamLoop(a3)
	lea	m_currentBpm(a3),a0
	move.l	(a7)+,d0			; music len in frame ticks
	rts


;------------------------------------------------------------------
;
;	LSP_MusicPlayTick
;
;		In:	a6: must be $dff000
;			Scratched regs: d0/d1/d2/a0/a1/a2/a3/a4/a5
;		Out:None
;
;------------------------------------------------------------------
LSP_MusicPlayTick
	lea	LSP_State(pc),a1
	move.l	m_byteStream(a1),a0
	move.l	m_codeTableAddr(a1),a2
.process
	moveq	#0,d0
.cloop	move.b	(a0)+,d0
	beq	.cextended
	add.w	d0,d0
	move.w	(a2,d0.w),d0			; code
	beq	.noInst
.cmdExec
	add.b	d0,d0
	bcc	.noVd
	move.b	(a0)+,$d9(a6)
.noVd	add.b	d0,d0
	bcc	.noVc
	move.b	(a0)+,$c9(a6)
.noVc	add.b	d0,d0
	bcc	.noVb
	move.b	(a0)+,$b9(a6)
.noVb	add.b	d0,d0
	bcc	.noVa
	move.b	(a0)+,$a9(a6)
.noVa	move.l	a0,(a1)+			; store byte stream ptr
	move.l	(a1),a0				; word stream
	tst.b	d0
	beq	.noPa
	add.b	d0,d0
	bcc	.noPd
	move.w	(a0)+,$d6(a6)
.noPd	add.b	d0,d0
	bcc	.noPc
	move.w	(a0)+,$c6(a6)
.noPc	add.b	d0,d0
	bcc	.noPb
	move.w	(a0)+,$b6(a6)
.noPb	add.b	d0,d0
	bcc	.noPa
	move.w	(a0)+,$a6(a6)
.noPa	tst.w	d0
	beq	.noInst

	moveq	#0,d1
	move.l	m_lspInstruments-4(a1),a2	; instrument table
	lea	.resetv+12(pc),a4
	lea	$d0(a6),a5
	moveq	#4-1,d2
.vloop	add.w	d0,d0
	bcs	.setIns
	add.w	d0,d0
	bcc	.skip
	move.l	(a4),a3
	move.l	(a3)+,(a5)
	move.w	(a3)+,4(a5)
	bra	.skip
.setIns	;add.w	(a0)+,a2

	move.w  (a0)+,d3                ; d3 is unused here
	add.w   d3,a2                   ;
	move.l	b_vars(pc),a3		; a3 is unused here

	cmp.w   #3,d2			;
	bne     .i2			;
	move.w  d3,v_inst3(a3)		;
	bra     .done			;
.i2	cmp.w   #2,d2			;
	bne     .i1			;
	move.w	d3,v_inst2(a3)		;
	bra     .done			;
.i1	cmp.w   #1,d2			;
	bne     .i0			;
	move.w  d3,v_inst1(a3)		;
	bra     .done			;
.i0	tst.w   d2			;
	bne	.done			;
	move.w	d3,v_inst0(a3)		;

.done	
	if instruments
	cmp.w	v_instlow(a3),d3	;
	bgt	.greater		;
	move.w  d3,v_instlow(a3)	;
.greater
	cmp.w	v_insthigh(a3),d3	;
	blt	.less			;
	move.w	d3,v_insthigh(a3)	;
.less
	endif

	add.w	d0,d0
	bcc	.noReset
	bset	d2,d1
	move.w	d1,$96(a6)
.noReset
	move.l	(a2)+,(a5)
	move.w	(a2)+,4(a5)
	move.l	a2,(a4)
.skip	subq.w	#4,a4
	sub.w	#$10,a5
	dbf	d2,.vloop

	move.l	m_dmaconPatch-4(a1),a3	; set dmacon value
	move.b	d1,(a3)

.noInst	move.l	a0,(a1)			; store word stream (or byte stream if coming from early out)
	rts

.cextended
	add.w	#$100,d0
	move.b	(a0)+,d0
	beq	.cextended
	add.w	d0,d0
	move.w	(a2,d0.w),d0		; code
	cmp.w	m_escCodeRewind(a1),d0
	beq	.r_rewind
	cmp.w	m_escCodeSetBpm(a1),d0
	beq	.r_chgbpm
	cmp.w	m_escCodeGetPos(a1),d0
	bne	.cmdExec
.r_setPos
	move.b	(a0)+,(m_currentSeq+1)(a1)
	bra	.process

.r_rewind	
	move.l	m_byteStreamLoop(a1),a0
	move.l	m_wordStreamLoop(a1),m_wordStream(a1)
	bra	.process

.r_chgbpm
	move.b	(a0)+,(m_currentBpm+1)(a1)	; BPM
	bra	.process

.resetv	dc.l	0,0,0,0

	rsreset	
m_byteStream		rs.l	1	;  0 byte stream
m_wordStream		rs.l	1	;  4 word stream
m_dmaconPatch		rs.l	1	;  8 dmacon
m_codeTableAddr		rs.l	1	; 12 code table addr
m_escCodeRewind		rs.w	1	; 16 rewind special escape code
m_escCodeSetBpm		rs.w	1	; 18 set BPM escape code
m_lspInstruments	rs.l	1	; 20 LSP instruments table addr
m_relocDone		rs.w	1	; 24 reloc done flag
m_currentBpm		rs.w	1	; 26 current BPM
m_byteStreamLoop	rs.l	1	; 28 byte stream loop point
m_wordStreamLoop	rs.l	1	; 32 word stream loop point
m_seqCount		rs.w	1
m_seqTable		rs.l	1
m_currentSeq		rs.w	1
m_escCodeGetPos		rs.w	1
sizeof_LSPVars		rs.w	0

LSP_State	ds.b	sizeof_LSPVars
	even


*------	LADD COLOR FADING -----------------------------------------------------*

laddfading
	CHECKACTOR actor_ladd_fading
	beq	.done

	move.w	v_laddcolindex(a5),d0	;
	btst	#0,d0			; every second frame only
	bne	.skip			;
	move.l	b_clist5(pc),a1		;
	add.w	v_laddcbank(a5),a1	;
	lea	laddcols(pc,d0.w),a0	;
	moveq	#numladdcolors-1,d7	;
.loop	move.w	(a0),(a1)		;
	add.w	#laddcol1end-laddcol1,a0 ;
	addq.w	#4,a1			; next color in clist
	dbf	d7,.loop		;

.skip	addq.w	#1,v_laddcolindex(a5)	;
	cmp.w	#laddcol1end-laddcol1,v_laddcolindex(a5) ;
	bne	.nf			;
	STOPACTOR actor_ladd_fading
	clr.w	v_laddcolindex(a5)	; init second line
	move.w	#laddcl2-clist5+2,v_laddcbank(a5) ;
.nf	
.done	rts				;

numladdcolors	equ	15

laddcols
laddcol1
	dc.w	$0000,$0100,$0100,$0100,$0100,$0100,$0100,$0100,$0100,$0100,$0100,$0100,$0100,$0100,$0100,$0100
laddcol1end
	dc.w	$0000,$0100,$0100,$0100,$0100,$0100,$0100,$0100,$0100,$0200,$0200,$0200,$0200,$0200,$0200,$0200
	dc.w	$0000,$0100,$0100,$0100,$0100,$0100,$0200,$0200,$0200,$0200,$0200,$0300,$0300,$0300,$0300,$0300
	dc.w	$0000,$0100,$0100,$0100,$0100,$0200,$0200,$0200,$0300,$0300,$0300,$0300,$0400,$0400,$0400,$0400
	dc.w	$0000,$0100,$0100,$0100,$0200,$0200,$0200,$0300,$0300,$0300,$0400,$0400,$0400,$0500,$0500,$0500
	dc.w	$0000,$0100,$0100,$0200,$0200,$0200,$0300,$0300,$0400,$0400,$0400,$0500,$0500,$0600,$0600,$0600
	dc.w	$0000,$0100,$0100,$0200,$0200,$0300,$0300,$0400,$0400,$0500,$0500,$0600,$0600,$0600,$0700,$0700
	dc.w	$0000,$0100,$0100,$0200,$0300,$0300,$0400,$0400,$0500,$0500,$0600,$0600,$0700,$0700,$0800,$0800
	dc.w	$0000,$0100,$0200,$0200,$0300,$0300,$0400,$0500,$0500,$0600,$0600,$0700,$0800,$0800,$0900,$0900
	dc.w	$0000,$0100,$0200,$0200,$0300,$0400,$0400,$0500,$0600,$0600,$0700,$0800,$0800,$0900,$0a00,$0a00
	dc.w	$0000,$0100,$0200,$0300,$0300,$0400,$0500,$0600,$0600,$0700,$0800,$0800,$0900,$0a00,$0b00,$0b00
	dc.w	$0000,$0100,$0200,$0300,$0400,$0400,$0500,$0600,$0700,$0800,$0800,$0900,$0a00,$0b00,$0c00,$0c00
	dc.w	$0000,$0100,$0200,$0300,$0400,$0500,$0600,$0600,$0700,$0800,$0900,$0a00,$0b00,$0c00,$0d00,$0d00
	dc.w	$0000,$0100,$0200,$0300,$0400,$0500,$0600,$0700,$0800,$0900,$0a00,$0b00,$0c00,$0d00,$0d00,$0e00
	dc.w	$0000,$0100,$0200,$0300,$0400,$0500,$0600,$0700,$0800,$0900,$0a00,$0b00,$0c00,$0d00,$0e00,$0f00


*------	PUNKS AND MISC DATA ---------------------------------------------------*

punks
lmsfns	dc.l	lmsforward-base
	dc.l	lmspush-base
	dc.l	lmspop-base
	dc.l	lmsleft-base
	dc.l	lmsright-base

mtns	dc.l	mtn_s-base
	dc.l	mtn_p1-base
	dc.l	mtn_r-base
	dc.l	mtn_e-base
	dc.l	mtn_a-base
	dc.l	mtn_d-base
	dc.l	mtn_p2-base
	dc.l	mtn_o-base
	dc.l	mtn_i-base
	dc.l	mtn_n-base
	dc.l	mtn_t-base
punksend

instactions
	dc.l	0				; $fe02
	ds.w	1
	dc.l	0				; $fe08
	ds.w	1
	dc.l	0				; $fe0e
	ds.w	1
	dc.l	0				; $fe14
	ds.w	1
	dc.l	0				; $fe1a
	ds.w	1
	dc.l	0				; $fe20
	ds.w	1
	dc.l	0				; $fe26
	ds.w	1
	dc.l	0				; $fe2c
	ds.w	1
	dc.l	0				; $fe32
	ds.w	1
	dc.l	0				; $fe38
	ds.w	1
	dc.l	0				; $fe3e
	ds.w	1
	dc.l	0				; $fe44
	ds.w	1
	dc.l	0				; $fe4a
	ds.w	1
	dc.l	0				; $fe50
	ds.w	1
	dc.l	0				; $fe56
	ds.w	1
	dc.l	0				; $fe5c
	ds.w	1
	dc.l	0				; $fe62
	ds.w	1
	dc.l	0				; $fe68
	ds.w	1
	dc.l	0				; $fe6e
	ds.w	1
	dc.l	0				; $fe74
	ds.w	1
	dc.l	0				; $fe7a
	ds.w	1
	dc.l	0				; $fe80
	ds.w	1
	dc.l	0				; $fe86
	ds.w	1
	dc.l	0				; $fe8c
	ds.w	1
	dc.l	0				; $fe92
	ds.w	1
	dc.l	0				; $fe98
	ds.w	1
	dc.l	0				; $fe9e
	ds.w	1
	dc.l	0				; $fea4
	ds.w	1
	dc.l	0				; $feaa
	ds.w	1
	dc.l	0				; $feb0
	ds.w	1
	dc.l	0				; $feb6
	ds.w	1
	dc.l	0				; $febc
	ds.w	1
	dc.l	0				; $fec2
	ds.w	1
	dc.l	0				; $fec8
	ds.w	1
	dc.l	0				; $fece
	ds.w	1
	dc.l	0				; $fed4
	ds.w	1
	dc.l	0				; $feda
	ds.w	1
	dc.l	0				; $fee0
	ds.w	1
	dc.l	0				; $fee6
	ds.w	1
	dc.l	0				; $feec
	ds.w	1
	dc.l	0				; $fef2
	ds.w	1
	dc.l	0				; $fef8
	ds.w	1
	dc.l	0				; $fefe
	ds.w	1
	dc.l	0				; $ff04
	ds.w	1
	dc.l	0				; $ff0a
	ds.w	1
	dc.l	0				; $ff10
	ds.w	1
	dc.l	0				; $ff16
	ds.w	1
	dc.l	0				; $ff1c
	ds.w	1
	dc.l	0				; $ff22
	ds.w	1
	dc.l	0				; $ff28
	ds.w	1
	dc.l	0				; $ff2e
	ds.w	1
	dc.l	0				; $ff34
	ds.w	1
	dc.l	0				; $ff3a
	ds.w	1
	dc.l	0				; $ff40
	ds.w	1
	dc.l	0				; $ff46
	ds.w	1
	dc.l	0				; $ff4c
	ds.w	1
	dc.l	0				; $ff52
	ds.w	1
	dc.l	0				; $ff58
	ds.w	1
	dc.l	0				; $ff5e
	ds.w	1
	dc.l	0				; $ff64
	ds.w	1
	dc.l	0				; $ff6a
	ds.w	1
	dc.l	0				; $ff70
	ds.w	1
	dc.l	0				; $ff76
	ds.w	1
	dc.l	0				; $ff7c
	ds.w	1
	dc.l	0				; $ff82
	ds.w	1
	dc.l	0				; $ff88
	ds.w	1
	dc.l	0				; $ff8e
	ds.w	1
	dc.l	0				; $ff94
	ds.w	1
	dc.l	ajump-base			; $ff9a
	ds.w	1
	dc.l	0				; $ffa0
	ds.w	1
	dc.l	ajump-base			; $ffa6
	ds.w	1
	dc.l	0				; $ffac
	ds.w	1
	dc.l	0				; $ffb2
	ds.w	1
	dc.l	0				; $ffb8
	ds.w	1
	dc.l	0				; $ffbe
	ds.w	1
	dc.l	0				; $ffc4
	ds.w	1
	dc.l	0				; $ffca
	ds.w	1
	dc.l	0				; $ffd0
	ds.w	1
	dc.l	0				; $ffd6
	ds.w	1
	dc.l	0				; $ffdc
	ds.w	1
	dc.l	0				; $ffe2
	ds.w	1
	dc.l	0				; $ffe8
	ds.w	1
	dc.l	0				; $ffee
	ds.w	1
	dc.l	0				; $fff4
	ds.w	1
	dc.l	astep40-base			; $fffa
	ds.w	1
	dc.l	0				; $0000
	ds.w	1
	dc.l	astep40-base			; $0006
	ds.w	1
	dc.l	astep40-base			; $000c
	ds.w	1
	dc.l	0				; $0012
	ds.w	1
	dc.l	0				; $0018
	ds.w	1
	dc.l	astep40-base			; $001e
	ds.w	1
	dc.l	0				; $0024
	ds.w	1
	dc.l	0				; $002a
	ds.w	1
	dc.l	aneg_step40-base		; $0030
	ds.w	1
	dc.l	aneg_step40-base		; $0036
	ds.w	1
	dc.l	aneg_step40-base		; $003c
	ds.w	1
	dc.l	0				; $0042
	ds.w	1
	dc.l	astep40-base			; $0048
	ds.w	1
	dc.l	0				; $004e
	ds.w	1
	dc.l	astep40-base			; $0054
	ds.w	1
	dc.l	0				; $005a
	ds.w	1
	dc.l	astep40-base			; $0060
	ds.w	1
	dc.l	0				; $0066
	ds.w	1
	dc.l	aneg_step40-base		; $006c
	ds.w	1
	dc.l	0				; $0072
	ds.w	1
	dc.l	0				; $0078
	ds.w	1
	dc.l	0				; $007e
	ds.w	1
	dc.l	0				; $0084
	ds.w	1
	dc.l	0				; $008a
	ds.w	1
	dc.l	0				; $0090
	ds.w	1
	dc.l	0				; $0096
	ds.w	1
	dc.l	0				; $009c
	ds.w	1
	dc.l	0				; $00a2
	ds.w	1
	dc.l	0				; $00a8
	ds.w	1
	dc.l	0				; $00ae
	ds.w	1
	dc.l	0				; $00b4
	ds.w	1
	dc.l	0				; $00ba
	ds.w	1
	dc.l	0				; $00c0
	ds.w	1
	dc.l	0				; $00c6
	ds.w	1
	dc.l	aneg_step40-base		; $00cc
	ds.w	1
	dc.l	0				; $00d2
	ds.w	1
	dc.l	ajump-base			; $00d8
	ds.w	1
	dc.l	0				; $00de
	ds.w	1
	dc.l	0				; $00e4
	ds.w	1
	dc.l	0				; $00ea
	ds.w	1
	dc.l	0				; $00f0
	ds.w	1
	dc.l	0				; $00f6
	ds.w	1
	dc.l	0				; $00fc
	ds.w	1
	dc.l	0				; $0102
	ds.w	1
	dc.l	astep40-base			; $0108
	ds.w	1
	dc.l	0				; $010e
	ds.w	1
	dc.l	0				; $0114
	ds.w	1
	dc.l	0				; $011a
	ds.w	1
	dc.l	0				; $0120
	ds.w	1
	dc.l	0				; $0126
	ds.w	1
	dc.l	0				; $012c
	ds.w	1
	dc.l	0				; $0132
	ds.w	1
	dc.l	0				; $0138
	ds.w	1
	dc.l	0				; $013e
	ds.w	1
	dc.l	ajump-base			; $0144
	ds.w	1
	dc.l	0				; $014a
	ds.w	1
	dc.l	ajump-base			; $0150
	ds.w	1
	dc.l	0				; $0156
	ds.w	1
	dc.l	0				; $015c
	ds.w	1
	dc.l	0				; $0162
	ds.w	1
	dc.l	0				; $0168
	ds.w	1
	dc.l	0				; $016e
	ds.w	1
	dc.l	ajump-base			; $0174
	ds.w	1
	dc.l	0				; $017a
	ds.w	1
	dc.l	ajump-base			; $0180
	ds.w	1
	dc.l	0				; $0186
	ds.w	1
	dc.l	0				; $018c
	ds.w	1
	dc.l	0				; $0192
	ds.w	1
	dc.l	0				; $0198
	ds.w	1
	dc.l	0				; $019e
	ds.w	1
	dc.l	0				; $01a4
	ds.w	1
	dc.l	0				; $01aa
	ds.w	1
	dc.l	0				; $01b0
	ds.w	1
	dc.l	0				; $01b6
	ds.w	1
	dc.l	0				; $01bc
	ds.w	1
	dc.l	0				; $01c2
	ds.w	1
	dc.l	0				; $01c8
	ds.w	1
	dc.l	0				; $01ce
	ds.w	1
	dc.l	0				; $01d4
	ds.w	1
	dc.l	0				; $01da
	ds.w	1
	dc.l	0				; $01e0
	ds.w	1
	dc.l	0				; $01e6
	ds.w	1
	dc.l	0				; $01ec
	ds.w	1
	dc.l	0				; $01f2
	ds.w	1
	dc.l	0				; $01f8
	ds.w	1
	dc.l	0				; $01fe
	ds.w	1
	dc.l	0				; $0204
	ds.w	1
	dc.l	0				; $020a
	ds.w	1
	dc.l	0				; $0210
	ds.w	1

	dc.l	anop-base			; special instrument/offset: $0216
	ds.w	1
instactionsend

astep40	clr.w	v_instbdindex(a5)		; start it
	move.w	#lmscolorlast-lmscolors,v_lmscolindex(a5) ; lms flashing color
anop	rts					; reusing this rts for anop

ajump	cmp.w	#$001e,v_inst1(a5)		; trigger is instrument $00cc and $001e together
	bne	.done				;
	move.w	#4*256,d1			; jump
	tst.w	v_instneg(a5)			;
	beq	.noneg				;
	neg.w	d1				;
.noneg	add.w	d1,v_rayindex1(a5)		;
	add.w	d1,v_rayindex2(a5)		;
	add.w	d1,v_rayindex3(a5)		;

	CHECKACTOR actor_lmsup
	bne	.done				;

	asr.w	#4,d1				;
	add.w	d1,v_lmsanimindex(a5)		;
.done	rts					;

aneg_step40
	bsr	aneg				;
	bra	astep40				;

aneg	not.w	v_instneg(a5)			;
	rts					;

instb	move.w	v_instbdindex(a5),d2		;
	move.w	instbrakedata(pc,d2.w),d1	;
	tst.w	v_instneg(a5)			;
	beq	.noneg				;
	neg.w	d1				;
.noneg	add.w	d1,v_rayindex1(a5)		;
	add.w	d1,v_rayindex2(a5)		;
	add.w	d1,v_rayindex3(a5)		;

	CHECKACTOR actor_lmsup
	bne	.nolms				;

	asr.w	#4,d1				;
	add.w	d1,v_lmsanimindex(a5)		;

.nolms	cmp.w	#instbrakedataend-instbrakedata,d2 ;
	beq	.more				:
	addq.w	#2,v_instbdindex(a5)		;
.more	rts					;

instbrakedata
;	dc.w	89*4,82*4,75*4,69*4,63*4
;	dc.w	58*4,52*4
	dc.w	47*4,43*4,39*4,35*4,31*4,27*4
	dc.w	24*4,21*4,19*4,16*4,14*4,12*4,10*4,9*4
	dc.w	7*4,6*4,5*4,4*4,3*4,2*4,2*4,1*4,1*4,1*4
instbrakedataend
	dc.w	0*4

srfexpandshrink
	CHECKACTOR actor_srfexpand
	beq	.shrink				;
	move.w	v_instwdindex(a5),d3		;
	moveq	#0,d1				;
	move.b	instwidendata2(pc,d3.w),d1	;
	moveq	#0,d2				;
	lea	instwidendata3(pc),a4		;
	move.b	(a4,d3.w),d2			;
	tst.w	v_instneg(a5)			;
	beq	.noneg				;
	sub.w	d1,v_rayindex2(a5)		;
	sub.w	d2,v_rayindex1(a5)		;
	bra	.next				;
.noneg	add.w	d1,v_rayindex2(a5)		;
	add.w	d2,v_rayindex3(a5)		;
.next	addq.w	#1,v_instwdindex(a5)		;
	cmp.w	#instwidendata2end-instwidendata2,d3 ;
	bne	.running			;
	STOPACTOR actor_srfexpand
.running	

.shrink	
	CHECKACTOR actor_srfshrink
	beq	.noshrink			;
	move.w	v_instwdindex(a5),d3		;
	moveq	#0,d1				;
	move.b	instwidendata2(pc,d3.w),d1	;
	moveq	#0,d2				;
	move.b	instwidendata3(pc,d3.w),d2	;
	tst.w	v_instneg(a5)			;
	beq	.noneg2				;
	add.w	d1,v_rayindex2(a5)		;
	add.w	d2,v_rayindex1(a5)		;
	bra	.next2				;
.noneg2	sub.w	d1,v_rayindex2(a5)		;
	sub.w	d2,v_rayindex3(a5)		;
.next2	subq.w	#1,v_instwdindex(a5)		;
	bpl	.running2			;
	STOPACTOR actor_srfshrink
.running2	
.noshrink
	rts					;

instwidendata2
	dc.b	32,32,32,32,28,32,28,28		; multiple of 4 (see sinelab)
	dc.b	28,28,24,28,28,24,24,24
	dc.b	24,24,20,24,20,20,20,20
	dc.b	20,20,16,16,20,16,16,16
	dc.b	12,16,12,12,12,12,12,12
	dc.b	12,8,8,8,8,8,8,8
	dc.b	4,4,8,4,4,0,4,0
	dc.b	4,0
instwidendata2end
	dc.b	0

instwidendata3
	dc.b	64,64,64,60,60,60,60,56
	dc.b	56,56,52,52,52,52,48,48
	dc.b	48,44,44,44,44,40,44,36
	dc.b	40,36,36,36,32,36,28,32
	dc.b	28,28,28,28,24,24,24,20
	dc.b	20,20,20,16,16,16,12,12
	dc.b	12,12,8,8,8,8,4,4
	dc.b	0,4
	dc.b	0

	even


*------	LADD BITMAPS ----------------------------------------------------------*

laddbitmaps
	incbin	"laddbitmaps"
laddbitmapsend
	even

*------	MUSIC -----------------------------------------------------------------*

lspbank	incbin	"schmoddermann-cut.lsbank"
lspbankend
	even	; very important

lspmusic
	incbin	"schmoddermann-cut.lsmusic",10 ; skip header (10 bytes)
	even
