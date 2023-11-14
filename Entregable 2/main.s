.segment "HEADER"
  ; .byte "NES", $1A      ; iNES header identifier
  .byte $4E, $45, $53, $1A
  .byte 2               ; 2x 16KB PRG code
  .byte 1               ; 1x  8KB CHR data
  .byte $01, $00        ; mapper 0, vertical mirroring

.segment "VECTORS"
  ;; When an NMI happens (once per frame if enabled) the label nmi:
  .addr nmi
  ;; When the processor first turns on or is reset, it will jump to the label reset:
  .addr reset
  ;; External interrupt IRQ (unused)
  .addr 0

; "nes" linker config requires a STARTUP section, even if it's empty
.segment "STARTUP"

; Main code segment for the program
.segment "CODE"

reset:
  sei		; disable IRQs
  cld		; disable decimal mode
  ldx #$40
  stx $4017	; disable APU frame IRQ
  ldx #$ff 	; Set up stack
  txs		;  .
  inx		; now X = 0
  stx $2000	; disable NMI
  stx $2001 	; disable rendering
  stx $4010 	; disable DMC IRQs

;; first wait for vblank to make sure PPU is ready
vblankwait1:
  bit $2002
  bpl vblankwait1

clear_memory:
  lda #$00
  sta $0000, x
  sta $0100, x
  sta $0300, x
  sta $0400, x
  sta $0500, x
  sta $0600, x
  sta $0700, x
  lda #$fe
  sta $0200, x
  inx
  bne clear_memory

;; second wait for vblank, PPU is ready after this
vblankwait2:
  bit $2002
  bpl vblankwait2

main:
load_palettes:
  lda $2002
  lda #$3f
  sta $2006
  lda #$00
  sta $2006
  ldx #$00

@loop:
  lda palettes, x
  sta $2007
  inx
  cpx #$20
  bne @loop



LoadSprites:
  ldx #$00
LoadSpritesLoop:
  lda sprites, x
  sta $0200, x
  inx
  cpx #$d0
  bne LoadSpritesLoop

LoadBackground:
  lda $2002
  lda #$20
  sta $2006
  lda #$00
  sta $2006
  ldx #$00
LoadBackgroundLoop1:
  lda background, x
  sta $2007
  inx 
  bne LoadBackgroundLoop1

  ldx #$00
LoadBackgroundLoop2:
  lda background + 256, x
  sta $2007
  inx 
  bne LoadBackgroundLoop2

  ldx #$00
LoadBackgroundLoop3:
  lda background + 512, x
  sta $2007
  inx 
  bne LoadBackgroundLoop3

  ldx #$00
LoadBackgroundLoop4:
  lda background + 768, x
  sta $2007
  inx 
  bne LoadBackgroundLoop4

enable_rendering:
  lda #%10000000	; Enable NMI
  sta $2000
  lda #%00010000	; Enable Sprites
  sta $2001

forever:
  jmp forever
  
nmi:
  ldx #$00 	; Set SPR-RAM address to 0
  stx $2003
  lda #$02
  sta $4014
  lda #$00
  sta $2005
  
  lda #%10010000
  sta $2000
  lda #%00011110
  sta $2001

  rti



palettes:
  ; Background Palette
  .byte $0f, $07, $37, $39
  .byte $0f, $00, $30, $37
  .byte $0f, $0c, $1c, $07
  .byte $0f, $00, $00, $00

  ; Sprite Palette
  .byte $0f, $0f, $21, $31
  .byte $0f, $00, $00, $00
  .byte $0f, $00, $00, $00
  .byte $0f, $2d, $3d, $30

sprites:
  ;      Y ,tile, attr, X
  .byte $88, $00, $00, $20 
  .byte $88, $01, $00, $28
  .byte $90, $10, $00, $20
  .byte $90, $11, $00, $28

  .byte $78, $02, $00, $30
  .byte $78, $02, $40, $38
  .byte $80, $12, $00, $30
  .byte $80, $13, $00, $38

  .byte $78, $04, $00, $50
  .byte $78, $05, $00, $58
  .byte $80, $14, $00, $50
  .byte $80, $15, $00, $58

  .byte $68, $0b, $00, $40
  .byte $68, $0c, $00, $48
  .byte $70, $1b, $00, $40
  .byte $70, $1c, $00, $48

  .byte $98, $07, $00, $40
  .byte $98, $08, $00, $48
  .byte $98, $09, $00, $50
  .byte $a0, $17, $00, $40
  .byte $a0, $18, $00, $48
  .byte $a0, $19, $00, $50

  .byte $88, $06, $03, $60
  .byte $88, $06, $43, $68
  .byte $90, $16, $03, $60
  .byte $90, $16, $43, $68

  .byte $88, $06, $03, $90
  .byte $88, $06, $43, $98 
  .byte $90, $1a, $03, $90
  .byte $90, $1a, $43, $98

  ;inverterted sprites
  .byte $68, $0b, $40, $b8
  .byte $68, $0c, $40, $b0
  .byte $70, $1b, $40, $b8
  .byte $70, $1c, $40, $b0

  .byte $78, $04, $40, $a8
  .byte $78, $05, $40, $a0
  .byte $80, $14, $40, $a8
  .byte $80, $15, $40, $a0

  .byte $78, $02, $40, $c8
  .byte $78, $02, $00, $c0
  .byte $80, $12, $40, $c8
  .byte $80, $13, $40, $c0

  .byte $88, $00, $40, $d8
  .byte $88, $01, $40, $d0
  .byte $90, $10, $40, $d8
  .byte $90, $11, $40, $d0

  .byte $98, $07, $40, $b8
  .byte $98, $08, $40, $b0
  .byte $98, $09, $40, $a8
  .byte $a0, $17, $40, $b8
  .byte $a0, $18, $40, $b0
  .byte $a0, $19, $40, $a8



background:
	.byte $2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$00
	.byte $01,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a
	.byte $2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$00
	.byte $01,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a
	.byte $2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$02,$2b
	.byte $2b,$03,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a
	.byte $2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$04,$05
	.byte $05,$06,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a
	.byte $2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a
	.byte $2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a
	.byte $07,$07,$07,$07,$07,$2a,$07,$07,$07,$07,$07,$07,$2a,$07,$07,$07
	.byte $07,$07,$07,$2a,$07,$07,$07,$07,$07,$07,$2a,$07,$07,$07,$07,$07
	.byte $2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2b,$2b,$2b,$2b,$2b,$2b,$2b,$2b
	.byte $2b,$2b,$2b,$2b,$2b,$2b,$2b,$2b,$2a,$28,$28,$28,$28,$28,$28,$28
	.byte $2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a
	.byte $2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a
	.byte $2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a
	.byte $2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a
	.byte $2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a
	.byte $2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a
	.byte $2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a
	.byte $2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a
	.byte $2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a
	.byte $2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a
	.byte $2a,$2a,$08,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$09
	.byte $08,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$09,$2a,$2a
	.byte $2a,$2a,$0d,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$0e
	.byte $0d,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$0e,$2a,$2a
	.byte $2a,$2a,$0d,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$0e
	.byte $0d,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$0e,$2a,$2a
	.byte $2a,$2a,$0d,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$0e
	.byte $0d,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$0e,$2a,$2a
	.byte $2a,$2a,$0d,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$0e
	.byte $0d,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$0e,$2a,$2a
	.byte $2a,$2a,$0d,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$0e
	.byte $0d,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$0e,$2a,$2a
	.byte $2a,$2a,$0d,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$0e
	.byte $0d,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$0e,$2a,$2a
	.byte $2a,$2a,$0d,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$0e
	.byte $0d,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$16,$0e,$18,$17
	.byte $2a,$2a,$0d,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$0e
	.byte $0d,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$2b,$2b,$2b,$2b
	.byte $2a,$2a,$0a,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0b
	.byte $0a,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$2b,$2b,$2b,$2b
	.byte $2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a
	.byte $2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2b,$2b,$2b,$2b
	.byte $2a,$2a,$10,$11,$11,$11,$11,$11,$11,$11,$11,$11,$12,$2a,$2a,$2a
	.byte $2a,$2a,$2a,$2a,$2a,$2a,$2a,$19,$1a,$1a,$1a,$1b,$2b,$2b,$2b,$2b
	.byte $2a,$2a,$13,$2b,$2b,$2b,$2b,$2c,$2b,$2b,$2b,$2b,$14,$2a,$2a,$2a
	.byte $2a,$2a,$2a,$2a,$2a,$2a,$2a,$1c,$29,$29,$29,$1d,$2b,$2b,$2b,$2b
	.byte $2a,$2a,$15,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$2a,$15,$2a,$2a,$2a
	.byte $2a,$2a,$2a,$2a,$2a,$2a,$2a,$1e,$2a,$2a,$2a,$1f,$2b,$2b,$2b,$2b
	.byte $20,$21,$22,$23,$20,$21,$22,$23,$20,$21,$22,$23,$20,$21,$22,$23
	.byte $20,$21,$22,$23,$20,$21,$22,$23,$20,$21,$22,$23,$20,$21,$22,$23
	.byte $24,$2b,$2b,$25,$24,$2d,$2d,$25,$24,$2b,$2b,$25,$24,$2b,$2b,$25
	.byte $24,$2b,$2b,$25,$24,$2b,$2b,$25,$24,$2b,$2b,$25,$24,$2b,$2b,$25
	.byte $24,$2b,$2b,$25,$24,$2d,$2d,$25,$24,$2b,$2b,$25,$24,$2b,$2b,$25
	.byte $24,$2b,$2b,$25,$24,$2b,$2b,$25,$24,$2b,$2b,$25,$24,$2b,$2b,$25
	.byte $26,$2b,$2b,$27,$26,$2b,$2d,$27,$26,$2b,$2b,$27,$26,$2b,$2b,$27
	.byte $26,$2b,$2b,$27,$26,$2b,$2b,$27,$26,$2b,$2b,$27,$26,$2b,$2b,$27
	.byte $55,$55,$55,$55,$55,$55,$55,$55,$05,$05,$05,$05,$05,$05,$05,$05
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$44,$55,$55,$55,$55,$55,$55,$11
	.byte $44,$55,$55,$55,$55,$55,$55,$11,$04,$05,$05,$05,$05,$05,$05,$15
	.byte $a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$0a,$0a,$0a,$0a,$0a,$0a,$0a,$0a

; Character memory
.segment "CHARS"
.incbin "matadero.chr"
