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


.segment "ZEROPAGE"
player_x:  .res 1  ; .res 1 means reserve one byte of space  
player_y:  .res 1
jump_counter:   .res 1
is_grounded: .res 1
frame: .res 1
sprite_id: .res 1
sprite_bytes: .res 1
direction: .res 1
walk_cycle_step: .res 1

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

set_initial_values:
  lda #$bf
  sta player_y
  lda #$20
  sta player_x
  lda #$10
  sta sprite_bytes
  lda #$01
  sta direction

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

GoToLoadSprites:
  jsr LoadSprites

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

  inc frame
  lda #$04
  cmp frame
  bpl NotYet
  inc walk_cycle_step
  lda #$04
  cmp walk_cycle_step
  bne InRange
  lda #$00
  sta walk_cycle_step
  InRange:
  lda #$00
  sta frame
  NotYet:
  

LatchController:
  lda #$01
  sta $4016
  lda #$00
  sta $4016

ReadP1A:
  lda #0
  sta is_grounded
  lda $4016       ; player 1
  and #%00000001  ; look at bit 0
  ; beq ReadP1ADone ; branch if button is not pressed
  bne DoJump      ; branch if pressed
  jmp DoNotJump
                  ; here goes instructions if the button is pressed (in this case, jump)
DoJump:
  lda jump_counter
  cmp #30
  bcs DoNotJump
  inc jump_counter
  dec player_y
  dec player_y
  dec player_y
  lda #$30
  sta sprite_id
  lda #$10
  sta sprite_bytes
  lda #$01
  cmp direction
  beq LoadJump
  lda #$78
  sta sprite_id
  LoadJump:
    jsr LoadSprites
    jmp ReadP1ADone

DoNotJump:  
  lda #$FF
  sta jump_counter
  ; lda player_y
  ; cmp #$bf ; check if colliding with floor 
  jsr CheckPlatform3
  jsr CheckPlatform1
  jsr CheckPlatform2
  jsr CheckFloor

  lda #0
  cmp is_grounded
  beq NotOnFloor 
  jsr TouchFloor ; stop falling
  jmp ReadP1ADone
  NotOnFloor:
  inc player_y
  inc player_y
  jsr LoadSprites
  jmp ReadP1ADone


ReadP1ADone:      ; finish handling this button
                  ; do the same for the other 7 buttons 
ReadP1B:
  lda $4016
  and #%00000001 
  beq ReadP1BDone 
  lda #$40
  sta sprite_id
  lda #$01
  cmp direction
  beq LoadAttack
  lda #$b8
  sta sprite_id
  LoadAttack:
    lda #$18
    sta sprite_bytes
    jsr LoadSprites
ReadP1BDone:

ReadP1Start:
  lda $4016
  and #%00000001
  beq ReadP1StartDone
ReadP1StartDone:

ReadP1Select:
  lda $4016
  and #%00000001
  beq ReadP1SelectDone
ReadP1SelectDone:

ReadP1Up:
  lda $4016
  and #%00000001
  beq ReadP1UpDone
ReadP1UpDone:

ReadP1Down:
  lda $4016
  and #%00000001
  beq ReadP1DownDone
ReadP1DownDone:

ReadP1Left:
  lda $4016
  and #%00000001
  beq ReadP1LeftDone

  lda player_x
  clc
  cmp #$01  ; check if colliding with left wall
  bcc ReadP1LeftDone
  dec player_x
  dec player_x
  lda #$00
  cmp jump_counter
  beq OnLandL
  lda #$ff
  cmp jump_counter
  bne InAirL
  OnLandL:
  ldx walk_cycle_step
  lda walk_cycle_left, x
  sta sprite_id
  lda #$10
  sta sprite_bytes
  InAirL:
    lda #$00
    sta direction
    jsr LoadSprites

ReadP1LeftDone:

ReadP1Right:
  lda $4016
  and #%00000001
  beq ReadP1RightDone

  lda player_x
  clc
  cmp #$F1  ; check if colliding with right wall
  bcs ReadP1RightDone
  inc player_x
  inc player_x
  lda #$00
  cmp jump_counter
  beq OnLandR
  lda #$ff
  cmp jump_counter
  bne InAirR
  OnLandR:
  ldx walk_cycle_step
  lda walk_cycle_right, x
  sta sprite_id
  lda #$10
  sta sprite_bytes
  InAirR:
    lda #$01
    sta direction
    jsr LoadSprites

ReadP1RightDone:
  rti

CheckFloor:
  lda player_y
  cmp #$c1 ; "bottom edge" (if I don't add this, it causes overflow errors)
  bpl CheckFloorEnd
  cmp #$bf ; "top edge" (the actual floor)
  bmi CheckFloorEnd
  lda #1
  sta is_grounded
  CheckFloorEnd:
  rts

CheckPlatform1:
  lda player_x
  cmp #$04 ; left edge
  bmi CheckPlatform1End
  cmp #$68 ; right edge
  bpl CheckPlatform1End 
  lda player_y
  cmp #$AB ; bottom edge
  bpl CheckPlatform1End
  cmp #$A9 ; top edge
  bmi CheckPlatform1End
  lda #1
  sta is_grounded
CheckPlatform1End:
  rts

CheckPlatform2:
  lda player_x
  cmp #$AA ; left edge
  bmi CheckPlatform2End
  cmp #$E0 ; right edge
  bpl CheckPlatform2End 
  lda player_y
  cmp #$AB ; bottom edge
  bpl CheckPlatform2End
  cmp #$A9 ; top edge
  bmi CheckPlatform2End
  lda #1
  sta is_grounded
CheckPlatform2End:
  rts

CheckPlatform3:
  lda player_x
  cmp #$D1 ; left edge
  bmi CheckPlatform3End
  cmp #$FF ; right edge
  bpl CheckPlatform3End 
  lda player_y
  cmp #$95 ; bottom edge
  bpl CheckPlatform3End
  cmp #$92 ; top edge
  bmi CheckPlatform3End
  lda #1
  sta is_grounded
CheckPlatform3End:
  rts

TouchFloor: 
  lda #0
  sta jump_counter
  lda #$00
  sta sprite_id
  ; ldx walk_cycle_step
  ; lda walk_cycle_right, x
  ; sta sprite_id
  lda #$10
  sta sprite_bytes
  lda #$01
  cmp direction
  beq LoadLanding
  lda #$a8
  sta sprite_id
  ; ldx walk_cycle_step
  ; lda walk_cycle_left, x
  ; sta sprite_id
  LoadLanding:
    jsr LoadSprites
  rts

LoadSprites:
  ldx #$00
  ldy sprite_id
LoadSpritesLoop:
  lda player_y
  clc
  adc sprites, y
  sta $0200, x
  inx
  iny
  lda sprites, y
  sta $0200, x
  inx
  iny
  lda sprites, y
  sta $0200, x
  inx
  iny
  lda player_x
  clc
  adc sprites, y
  sta $0200, x
  inx
  iny
  cpx sprite_bytes ;ADD 4 FOR EACH SPRITE
  bne LoadSpritesLoop
  lda #$fe
  sta $0200, x
  inx
  sta $0200, X
  inx
  sta $0200, X
  inx
  sta $0200, x
  inx
  sta $0200, x
  inx
  sta $0200, X
  inx
  sta $0200, X
  inx
  sta $0200, x
  rts

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
  .byte $00, $00, $00, $00 ;default right $00
  .byte $00, $01, $00, $08
  .byte $08, $10, $00, $00
  .byte $08, $11, $00, $08

  .byte $00, $02, $00, $00 ;walk 1 right $10
  .byte $00, $02, $40, $08
  .byte $08, $12, $00, $00
  .byte $08, $13, $00, $08

  .byte $00, $04, $00, $00 ;walk 2 right $20
  .byte $00, $05, $00, $08
  .byte $08, $14, $00, $00
  .byte $08, $15, $00, $08

  .byte $00, $0b, $00, $00 ;jump right $30
  .byte $00, $0c, $00, $08
  .byte $08, $1b, $00, $00
  .byte $08, $1c, $00, $08

  .byte $00, $07, $00, $00 ;cute right $40
  .byte $00, $08, $00, $08
  .byte $00, $09, $00, $10
  .byte $08, $17, $00, $00
  .byte $08, $18, $00, $08
  .byte $08, $19, $00, $10

  .byte $00, $06, $03, $00 ;damage $58
  .byte $00, $06, $43, $08
  .byte $08, $16, $03, $00
  .byte $08, $16, $43, $08

  .byte $00, $06, $03, $00 ;death $68
  .byte $00, $06, $43, $08
  .byte $08, $1a, $03, $00
  .byte $08, $1a, $43, $08

  ; inverterted sprites
  .byte $00, $0b, $40, $08 ;jump left $78
  .byte $00, $0c, $40, $00
  .byte $08, $1b, $40, $08
  .byte $08, $1c, $40, $00

  .byte $00, $04, $40, $08 ;walk 2 left $88
  .byte $00, $05, $40, $00
  .byte $08, $14, $40, $08
  .byte $08, $15, $40, $00

  .byte $00, $02, $40, $08 ;walk 1 left $98
  .byte $00, $02, $00, $00
  .byte $08, $12, $40, $08
  .byte $08, $13, $40, $00

  .byte $00, $00, $40, $08 ;default left $a8
  .byte $00, $01, $40, $00
  .byte $08, $10, $40, $08
  .byte $08, $11, $40, $00

  .byte $00, $07, $40, $08 ;cute left $b8
  .byte $00, $08, $40, $00
  .byte $00, $09, $40, $f8
  .byte $08, $17, $40, $08
  .byte $08, $18, $40, $00
  .byte $08, $19, $40, $f8


walk_cycle_right:
  .byte $00, $10, $00, $20

walk_cycle_left:
  .byte $a8, $98, $a8, $88

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
