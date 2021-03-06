;==============================================================
; WLA-DX banking setup
;==============================================================

.memorymap
defaultslot 0
slotsize $4000
slot 0 $0000
slot 1 $4000
slot 2 $8000
.endme

.rombankmap
bankstotal 16
banksize $4000
banks 16
.endro

;==============================================================
; SMS rom header
;==============================================================

.smstag

;==============================================================
; SMS defines
;==============================================================

.define VDPControl $bf
.define VDPData $be
.define VDPScanline $7e
.define PSGPort $7f
.define IOPortA $DC
.define VRAMRead $0000
.define VRAMWrite $4000
.define CRAMWrite $c000
.define MapperSlot0 $fffd
.define MapperSlot1 $fffe
.define MapperSlot2 $ffff
.define TileSize 32
.define TileMapSize 1536
.define TilePaletteSize 16
.define VDPPaletteSize 32
.define BankSize_ 16384
.define Width 256
.define Height 192
.define HalfWidth (Width / 2)
.define HalfHeight (Height / 2)

;==============================================================
; Program defines
;==============================================================

.define BorderColor 16

.define RotoColumnCount 24
.define RotoLineCount 24

.define PM7LineCount 12
.define PM7Fov 0.85

.define PartOriginY 0
.define PartInitialBeatWait 1
.define PartEndBeat 4

.define VBCount 16
.define VBOriginX HalfWidth
.define VBOriginY 64
.define VBOriginYShadow 64
.define VBOriginZ (-64)

.define MusicVBlankPerBeat 48

;==============================================================
; Utility macros
;==============================================================

.macro PlaySampleU ; c65 (436)
    ex af, af'
    exx
    
    rld
    and $0f
    or $90
    out (PSGPort), a
    inc hl
    
    exx    
    ex af, af'
.endm

.macro PlaySampleL ; c58 (443)
    ex af, af'
    exx
    
    ld a, (hl)
    and $0f
    or $90
    out (PSGPort), a
    bit 7, h
    call nz, ParticlesChangePCMBank

    exx    
    ex af, af'
.endm

.macro WaitVBlank args playSmp ; c11
    xor a 
    ld (HadVBlank), a
---:
    .ifeq playSmp 1
        PlaySampleL
        .repeat 18
            inc iy
            dec iy
        .endr
        PlaySampleU
        .repeat 16
            inc iy
            dec iy
        .endr
    .endif
    ld a, (HadVBlank)
    or a  ; update flags
    jp p, ---
.endm

.macro SetVDPAddress args addr  ; c36
; sets the VDP address
    ld a, <addr
    out (VDPControl),a
    ld a, >addr
    out (VDPControl),a
.endm

.macro CopyToVDP
; copies data to the VDP
; parameters: hl = data address, de = data length
; affects: a, hl, bc, de
    ld c, VDPData
-:  outi
    dec de
    ld a,d
    or e
    jp nz,-
.endm

.macro CopyToVDPPCM args slot_
; copies data to the VDP
; parameters: hl = data address, de = data length
; affects: a, hl, bc, de
    ld b, 8
    ld c, VDPData
-:  outi

    jp nz, +
    ld a, 6
    ld (MapperSlot1), a
    PlaySampleL
    PlaySampleU  
    ld a, slot_
    ld (MapperSlot1), a    
    ld b, 8
+:

    dec de
    ld a,d
    or e
    jp nz,-
.endm

.macro PlaySampleSkew args skew
    ex af, af'
        ; this macro is 27 cycles when not playing
        ; one sample every 325 cycles (320 + 5 from jr not jumping)
    add a, (skew + 27) / 325 * 256
    jr nc, ++++
    PlaySample
++++:
    ex af, af'
.endm

.macro AddAToHL
    add a, l
    ld l, a
    adc a, h
    sub l
    ld h, a
.endm

.macro AddAToDE
    add a, e
    ld e, a
    adc a, d
    sub e
    ld d, a
.endm

.macro AddAToBC
    add a, c
    ld c, a
    adc a, b
    sub c
    ld b, a
.endm

.macro AddAToIX
    add a, ixl
    ld ixl, a
    adc a, ixh
    sub ixl
    ld ixh, a
.endm

.macro NegateHL
    xor a
    sub l
    ld l,a
    sbc a,a
    sub h
    ld h,a
.endm

.macro NegateDE
    xor a
    sub e
    ld e,a
    sbc a,a
    sub d
    ld d,a
.endm

.macro NegateBC
    xor a
    sub c
    ld c,a
    sbc a,a
    sub b
    ld b,a
.endm

.macro NegateIX
    xor a
    sub ixl
    ld ixl,a
    sbc a,a
    sub ixh
    ld ixh,a
.endm

.macro NegateIY
    xor a
    sub iyl
    ld iyl,a
    sbc a,a
    sub iyh
    ld iyh,a
.endm

.macro SignExtendAToHL ; c20
	ld l,a
	rlca		; or rla
	sbc a,a
	ld h,a
.endm

.macro SignExtendAToBC ; c20
	ld c,a
	rlca		; or rla
	sbc a,a
	ld b,a
.endm    

; 256 step cosinus
; input in c and output in a
.macro GetCosC
    ld b, >CosLUT
    ld a, (bc)
.endm

; 256 step sinus
; input in c and output in a
.macro GetSinC
    ld b, >SinLUT
    ld a, (bc)
.endm
    
.macro HLFakeCall args 1
    ld hl, +
    jp \1
+:
.endm

.macro HLFakeRet
    jp (hl)
.endm

.macro memcpy args dest, src, len
    ld de, dest
    ld hl, src
    ld bc, len
    ldir
.endm

.macro memset args dest, val, len
    ld hl, dest
    ld de, dest + 1
    ld bc, len - 1
    ld a, val
    ld (hl), a
    ldir
.endm

;==============================================================
; RAM variables
;==============================================================

; Global variables
.enum $c000 export
    XScroll           dsb 192
    LocalPalette      dsb 32
    FadeinPalette     dsb 32

    CurBeatIdx        dw
    CurFrameIdx       dw
    SPSave            dw
    MusicPtr          dw 
    RandSeed          db
    MusicBank         db
    MusicFrameWait    db
    CurEffect         db
    HadVBlank         db  
    BeatCounter       db
    UseXScroll        db  
    IntroRobCurFrame  db  
    IntroDissCurFrame db  
.ende

; RotoZoom / PseudoMode7
.enum $c200 export
    RotoRAMBakeIncs   dsb 256 ; align 256
    RotoRAMBakeIncsEnd .
    RotoPrecalcData   dsb 256 ; align 256
    PM7SAT            dsb 256 ; align 256
    PM7TM             dsb PM7LineCount * 64 + 64    
    PM7TMEnd          .

    RotoRot           dw ; (8.8 fixed point)
    RotoScl           dw ; (8.8 fixed point)
    RotoX             dw ; (8.8 fixed point)
    RotoY             dw ; (8.8 fixed point)
    RobAX             dw ; (8.8 fixed point)
    RobAY             dw ; (8.8 fixed point)
    RobBX             dw ; (8.8 fixed point)
    RobBY             dw ; (8.8 fixed point)
    RotoTX            db
    RotoTY            db
    PM7CurLine        db
    RotoCurStep       dw
    RotoNextStepBeat  db
    RotoLoadedOtherCol db
    
    ; keep this last
    RAMCode           .
.ende

; 2D particles
.enum $c200 export
    PartXH            dsb 256
    PartXL            dsb 256
    PartYH            dsb 256
    PartYL            dsb 256
    PartXA            dsb 256 ; (4.4 fixed point)
    PartYA            dsb 256 ; (4.4 fixed point)
    PartLife          dsb 256 ; (4.4 fixed point)
    PartLifeA         dsb 256 ; (4.4 fixed point)
    PartSAT           dsb 256
    PartCount         db
    PartMode          db
    PartInitialBeat   db
.ende

; VectorBalls
.enum $c200 export
    VBX               dsb 256
    VBY               dsb 256
    VBZ               dsb 256
    VBSAT             dsb 256
    VBRX              db
    VBRY              db
    VBRZ              db
    VBSinCos          dsb 6
    VBFactors         dsb 9
    VBCurStep         dw
    VBCurStepBeat     db
.ende

;==============================================================
; Init code
;==============================================================

.bank 2 slot 2
.org $0000
interrupt:
    push af
    push hl

    in a, (VDPControl) ;ack vdp int
    and $80
    ld (HadVBlank), a
    jp m, +

    ; line interrupt X scroll
    
    in a, (VDPScanline)
    ld l, a
    ld h, >XScroll
    ld a, (hl)
    
    out (VDPControl), a
    ld a, $88
    out (VDPControl), a

    pop hl
    pop af
    ei
    ret
+:
    ; for X scroll

    ld a, (UseXScroll)
    or a
    jp z, MusicUpdate
    ld a, (XScroll)
    out (VDPControl), a
    ld a, $88
    out (VDPControl), a

    jp MusicUpdate    

init_tab: ; table must exist within this bank
    .db $00, $00, $01, $02

main:
    di              ; disable interrupts
    im 1            ; interrupt mode 1
    
    ; set up stack

    ld sp, $dff0

    ; clear RAM

    memset $c000, 0, $2000

    ; this maps the first 48K of ROM to $0000-$BFFF
    memcpy $fffc, init_tab, $0004
   
    ; set up VDP registers

    ld hl,VDPInitData
    ld b,VDPInitDataEnd-VDPInitData
    ld c,VDPControl
    otir

    ; clear VRAM

    ; 1. set VRAM write address to $0000
    SetVDPAddress $0000 | VRAMWrite
    ; 2. output 16KB of zeroes
    ld bc, $4000     ; counter for 16KB of VRAM
-:  xor a
    out (VDPData), a ; output to VRAM address, which is auto-incremented after each write
    dec bc
    ld a, b
    or c
    jr nz, -

    call SetDummySpriteTable
    
    call EnsureFifty    
    
    call UploadPalette
    
    in a, (VDPControl) ; ack any previous int
    
    ; turn screen on
    ld a, %1100010
;          ||||||`- Zoomed sprites -> 16x16 pixels
;          |||||`-- Doubled sprites -> 2 tiles per sprite, 8x16
;          ||||`--- Mega Drive mode 5 enable
;          |||`---- 30 row/240 line mode
;          ||`----- 28 row/224 line mode
;          |`------ VBlank interrupts
;          `------- Enable display
    out (VDPControl), a
    ld a, $81
    out (VDPControl), a
    
    call MusicInit
    
    ei

Reinit:

    ; Effect init
    call CurEffectInit
    
;==============================================================
; Main loop
;==============================================================

MainLoop:
p0:

    ; frame index advance
    ld a, (CurFrameIdx)
    inc a
    ld (CurFrameIdx), a
   
p1:
    call CurEffectUpdate
    jp MainLoop
    
;==============================================================
; Utility functions
;==============================================================

; 16*8 unsigned multiplication
; The following routine multiplies de by a and places the result in ahl
; (which means a is the most significant byte of the product, l the least significant and h the intermediate one...)
MultiplyUnsignedAByDE:
    ld c, 0
    ld h, c
    ld l, h

    add a,a  ; optimised 1st iteration
    jp nc, +
    ld h,d
    ld l,e
+:
    .repeat 7
        add hl,hl
        rla
        jp nc, +
        add hl,de
        adc a,c
    +:
    .endr

    ret

; 16*8 fixed point signed multiplication
; input in a/de, output in hl
FPMultiplySignedAByDE:
    ld c, 0
    ld h,c
    ld l,c

    add a,a  ; optimised 1st iteration
    jp nc, @Positive
    neg
    
    .repeat 7
        add hl,hl
        rla
        jp nc, +
        add hl,de
        adc a,c
    +:
    .endr
    
    ld l, h
    ld h, a
    NegateHL
   
    ret
    
@Positive:
   
    .repeat 7
        add hl,hl
        rla
        jp nc, +
        add hl,de
        adc a,c
    +:
    .endr

    ld l, h
    ld h, a

    ret

; 16*16 unsigned multiplication
; This routine performs the operation BCHL=BC*DE
MultiplyBCByDE:
    ld hl,0

    .repeat 16
        add hl,hl
        rl c
        rl b
        jp nc, +
        add hl,de
        jp nc, +
        inc bc  ; This instruction (with the jump) is like an "ADC DE,0"
    +:
    .endr

    ret
    

; Multiply unsigned 8-bit values
; In:  Multiply H with E
; Out: HL = result
MultiplyUnsignedHByE:
    ld d,0
    ld l,d
    
    sla h		; optimised 1st iteration
    jr nc, +
    ld l,e
+:
    
    .repeat 7
        add hl,hl
        jr nc, +
        add hl,de
    +:
    .endr
    ret    

; Square Table 8-bit * 8-bit Signed
; Input: B = Multiplier, C = Multiplicand (both in range -128..127)
; Output: HL = Product
; Thrashes: AF
FPMultiplySignedBByC:
    push bc
    push de

    ld  h, >SSqrLUT
    ld  l,b
    ld  a,b
    ld  e,(hl)
    inc h
    ld  d,(hl)      ; DE = a^2
    ld  l,c
    ld  b,(hl)
    dec h
    ld  c,(hl)      ; BC = b^2
    add a,l     ; let's try (a + b)
    jp  po, @Plus     ; jump if no overflow

    sub l
    sub l
    ld  l,a
    ld  a,(hl)
    inc h
    ld  h,(hl)
    ld  l,a     ; HL = (a - b)^2
    ex  de,hl
    add hl,bc
    sbc hl,de       ; HL = a^2 + b^2 - (a - b)^2

    ;sra h       ; uncomment to get real product
    ;rr  l

    pop de
    pop bc
    
    ret

@Plus:
    ld  l,a
    ld  a,(hl)
    inc h
    ld  h,(hl)
    ld  l,a     ; HL = (a + b)^2
    or  a
    sbc hl,bc
    or  a
    sbc hl,de       ; HL = (a + b)^2 - a^2 - b^2

    ;sra h       ; uncomment to get real product
    ;rr  l

    pop de
    pop bc
    
    ret
    
.macro FPMultiplySignedBByC_Faster
    push de

    ld  h, >SSqrLUT
    ld  l,b
    ld  a,b
    ld  e,(hl)
    inc h
    ld  d,(hl)      ; DE = a^2
    ld  l,c
    ld  b,(hl)
    dec h
    ld  c,(hl)      ; BC = b^2
    add a,l     ; let's try (a + b)
    jp  po, +++     ; jump if no overflow

    sub l
    sub l
    ld  l,a
    ld  a,(hl)
    inc h
    ld  h,(hl)
    ld  l,a     ; HL = (a - b)^2
    ex  de,hl
    add hl,bc
    sbc hl,de       ; HL = a^2 + b^2 - (a - b)^2

    jp ++++

+++:
    ld  l,a
    ld  a,(hl)
    inc h
    ld  h,(hl)
    ld  l,a     ; HL = (a + b)^2
    or  a
    sbc hl,bc
    or  a
    sbc hl,de       ; HL = (a + b)^2 - a^2 - b^2

++++:

    ;sra h       ; uncomment to get real product
    ;rr  l
    
    pop de
.endm    
    
NullSub:
    ret
    
CurEffectInit:
    ld h, >EffectsSequence
    ld a, (CurEffect)
    add a, a
    add a, a
    inc a
    add a, a
    ld l, a
    ld e, (hl)
    inc l
    ld d, (hl)
    ex de, hl
    jp (hl)
    
CurEffectFinalize:
    ld h, >EffectsSequence
    ld a, (CurEffect)
    add a, a
    add a, a
    inc a
    inc a
    add a, a
    ld l, a
    ld e, (hl)
    inc l
    ld d, (hl)
    ex de, hl
    jp (hl)
    
CurEffectUpdate:
    ld h, >EffectsSequence
    ld a, (CurEffect)
    add a, a
    add a, a
    add a, a
    ld l, a
    ld e, (hl)
    inc l
    ld d, (hl)
    ex de, hl
    jp (hl)

NextEffect_JP:
    call CurEffectFinalize
    ld a, (CurEffect)
    inc a
    cp (EffectsSequenceEnd-EffectsSequence) / 8
    jp nz, ++
    dec a
++:
    ld (CurEffect), a
    jp Reinit
    

UploadPalette:
    ld c, VDPData
    SetVDPAddress $0000 | CRAMWrite
    ld hl, LocalPalette
    .repeat VDPPaletteSize / 2
        outi
        inc iy
        outi
        dec iy
    .endr
    
    ret

UploadPalettePCM:
    ld c, VDPData
    SetVDPAddress $0000 | CRAMWrite
    ld hl, LocalPalette
    .repeat VDPPaletteSize / 4
        outi
        inc iy
        outi
        dec iy
    .endr
    PlaySampleL
    .repeat VDPPaletteSize / 4
        outi
        inc iy
        outi
        dec iy
    .endr
    PlaySampleU
    
    ret

    
SetDummySpriteTable:
    ; dummy Sprite table
    SetVDPAddress $3f00 | VRAMWrite
    ld a, $d0
    out (VDPData), a
    
    ret
    
ClearTileMap:
    ; TODO: improve me
    
    SetVDPAddress $3800 | VRAMWrite
    ld bc, TileMapSize
-:  xor a
    out (VDPData), a
    dec bc
    ld a, b
    or c
    jr nz, -
    
    SetVDPAddress $3000 | VRAMWrite
    ld bc, TileMapSize
-:  xor a
    out (VDPData), a
    dec bc
    ld a, b
    or c
    jr nz, -

    ret
    
; Fast RND
;
; An 8-bit pseudo-random number generator,
; using a similar method to the Spectrum ROM,
; - without the overhead of the Spectrum ROM.
;
; R = random number seed
; an integer in the range [1, 256]
;
; R -> (33*R) mod 257
;
; S = R - 1
; an 8-bit unsigned integer
.macro random ; c81
    push bc
    ld a, (RandSeed)
    ld b, a 

    rrca ; multiply by 32
    rrca
    rrca
    xor $1f

    add a, b
    sbc a, 255 ; carry

    ld (RandSeed), a
    pop bc
.endm 

DetectTVType:
; Returns a=0 for NTSC, a=1 for PAL
-:
    in a, (VDPScanline)
    cp 192
    jp nz, -
    
    ld b, 0
    ld c, a
-:
    in a, (VDPScanline)
    cp c
    jp z, -
    inc b
    ld c, a
    cp 0
    jp nz, -    

    ld a, b
    cp 100
    ld a, 0
    jr c, +
    inc a
+:

    ret
    
SATOuti:
    .repeat 256
        outi
    .endr
    
    ret

SATOutiPCM: ; c106
    .repeat 16
        outi
    .endr

    .repeat 5
        .repeat 23
            outi
        .endr
        PlaySampleL
        .repeat 22
            outi
        .endr
        PlaySampleU
    .endr
   
    .repeat 15
        outi
    .endr
    
    ret

    
;==============================================================
; Music code
;==============================================================

.macro MusicGetByte
    ld a, (hl)
    inc hl
    bit 7, h
    jp z, +++
    ld c, a
    ld hl, $4000
    ld a, (MusicBank)
    inc a
    ld (MusicBank), a
    ld (MapperSlot1), a
    ld a, c
+++:
.endm
    
MusicInit:
    ; init PSG
    ld hl,PSGInitData
    ld bc,(PSGInitDataEnd-PSGInitData)<<8 + PSGPort
    otir

    ld hl, $4000
    ld (MusicPtr), hl
    ld a, 4
    ld (MusicBank), a
    ld a, 1
    ld (MusicFrameWait), a
    
    ret

MusicUpdate:
    push bc
    push de
    
    ; beats management
    ld a, (BeatCounter)
    or a
    jp nz, +
    ld hl, CurBeatIdx
    inc (hl)
    ld a, MusicVBlankPerBeat
+:    
    dec a
    ld (BeatCounter), a
    
    ; context switch
    ld a, (MapperSlot1)
    ld b, a
    ld a, (MusicBank)
    ld (MapperSlot1), a     
    ld hl, (MusicPtr)
    
    ld a, (MusicFrameWait)
    or a
    jp z, @Return ; termination is MusicFrameWait = 0
    dec a
    ld (MusicFrameWait), a
    jp nz, @Return
-:    
    MusicGetByte
    bit 4, a ; "Wait" command ?
    jp z, +
    ; "PSG" command
    MusicGetByte
    out (PSGPort), a
    jp -
+:
    ld c, 0
    cp $66 ; "Termination" command
    jr z, +
    inc c
    cp $63 ; "Wait frame" command?
    jp z, +
    ; "Wait samples" command
    MusicGetByte
    ld e, a
    MusicGetByte
    ld d, a
    ld a, 75 ; ~= 65536/882 (882 samples per frame)
    push hl
    call MultiplyUnsignedAByDE
    pop hl
    ld c, a
+:    
    ld a, c
    ld (MusicFrameWait), a
    
@Return:    
    ; context switch
    ld (MusicPtr), hl
    ld a, b
    ld (MapperSlot1), a
    pop de
    pop bc
    ; out of interrupt mode
    pop hl
    pop af
    ei
    ret

;==============================================================
; Fade in / fade out code
;==============================================================

FadeoutLocalPalette:
    ld hl, LocalPalette
    ld b, VDPPaletteSize
-:
    ld c, (hl)
    
    ; red
    ld a, c
    and $03
    jr z, +
    ld a, c
    dec a
    jp ++
+:    
    ; green
    ld a, c
    and $0c
    jr z, +
    ld a, c
    sub $04
    jp ++
+:    
    ; blue
    ld a, c
    and $30
    jr z, +
    ld a, c
    sub $10
+:    
++:    
    
    ld (hl), a

    inc hl    
    djnz -
    
    ret
    
FadeinLocalPaletteBGR:
    ld b, VDPPaletteSize
    ld de, FadeinPalette
    ld hl, LocalPalette
-:
    ld a, (de)
    ld c, a
    
    ; blue
    ld a, c
    and $30
    jr z, +
    ld a, c
    sub $10
    ld c, a
    ld a, (hl)
    add a, $10
    ld (hl), a
    jp ++
+:    
    ; green
    ld a, c
    and $0c
    jr z, +
    ld a, c
    sub $04
    ld c, a
    ld a, (hl)
    add a, $04
    ld (hl), a
    jp ++
+:    
    ; red
    ld a, c
    and $03
    jr z, +
    ld a, c
    dec a
    ld c, a
    inc (hl)
+:    
++:    
    
    ld a, c
    ld (de), a

    inc de
    inc hl
    djnz -
    
    ret
    
FadeinLocalPaletteBRG:
    ld b, VDPPaletteSize
    ld de, FadeinPalette
    ld hl, LocalPalette
-:
    ld a, (de)
    ld c, a
    
    ; blue
    ld a, c
    and $30
    jr z, +
    ld a, c
    sub $10
    ld c, a
    ld a, (hl)
    add a, $10
    ld (hl), a
    jp ++
+:    
    ; red
    ld a, c
    and $03
    jr z, +
    ld a, c
    dec a
    ld c, a
    inc (hl)
    jp ++
+:    
    ; green
    ld a, c
    and $0c
    jr z, +
    ld a, c
    sub $04
    ld c, a
    ld a, (hl)
    add a, $04
    ld (hl), a
+:    
++:    
    
    ld a, c
    ld (de), a

    inc de
    inc hl
    djnz -
    
    ret

Fadeout:
    ld a, (CurFrameIdx)
    and $01
    ret nz
    call FadeoutLocalPalette
    jp UploadPalette
    
FadeoutSlo:
    ld a, (CurFrameIdx)
    and $03
    ret nz
    call FadeoutLocalPalette
    jp UploadPalette
    
Fadein:
    ld a, (CurFrameIdx)
    and $01
    ret nz
    call FadeinLocalPaletteBGR
    jp UploadPalette
    
FadeinPCM:
    ld a, (CurFrameIdx)
    and $01
    ret nz
    call FadeinLocalPaletteBGR
    jp UploadPalettePCM    
    
FadeinSlowerBRG:
    ld a, (CurFrameIdx)
    and $07
    ret nz
    call FadeinLocalPaletteBRG
    jp UploadPalette
    
FadeoutSloBeat:
    WaitVBlank 0
    
    call FadeoutSlo
    ld a, (BeatCounter)
    or a
    jp z, NextEffect_JP
    ret

;==============================================================
; Intro code
;==============================================================
 
EnsureFifty:
    call DetectTVType
    or a
    ret nz
    
    ld a, 12
    ld (MapperSlot1), a

    ; load tiles (Background)
    SetVDPAddress $0000 | VRAMWrite
    ld hl, FiftyTiles
    ld de, FiftyTilesSize
    CopyToVDP

    ; load tilemap (Background)
    SetVDPAddress $3800 | VRAMWrite
    ld hl,FiftyTileMap
    ld de,2048
    CopyToVDP
    
    ; Load palette (Background)
    memcpy LocalPalette, FiftyPalette, TilePaletteSize
    memcpy LocalPalette + TilePaletteSize, FiftyPalette, TilePaletteSize
    
    call UploadPalette
    
    ; turn screen on
    ld a, %1000000
;          ||||||`- Zoomed sprites -> 16x16 pixels
;          |||||`-- Doubled sprites -> 2 tiles per sprite, 8x16
;          ||||`--- Mega Drive mode 5 enable
;          |||`---- 30 row/240 line mode
;          ||`----- 28 row/224 line mode
;          |`------ VBlank interrupts
;          `------- Enable display
    out (VDPControl), a
    ld a, $81
    out (VDPControl), a    

-:
    jp -
 
IntroInit:
    ; robots slot
    ld a, 12
    ld (MapperSlot1), a

    ; load tiles (Flying robots)
    SetVDPAddress $2000 | VRAMWrite
    ld hl,RobFlyTiles
    ld de,RobFlyTilesSize
    CopyToVDP

    ; Load palette (Flying robots)
    memcpy FadeinPalette + TilePaletteSize, RobFlyPalette, TilePaletteSize

    memcpy PM7SAT, RobFlySAT, 256
    
    ; bg slot
    ld a, 11
    ld (MapperSlot1), a

    ; load tiles (Background)
    SetVDPAddress $0000 | VRAMWrite
    ld hl, IntroTiles
    ld de, IntroTilesSize
    CopyToVDP

    ; load tilemap (Background)
    SetVDPAddress $3800 | VRAMWrite
    ld hl,IntroTileMap
    ld de,2048
    CopyToVDP
    
    ; Load palette (Background)
    memcpy FadeinPalette, IntroPalette, TilePaletteSize

    ; scroll y
    ld hl, $2000
    ld (RotoY), hl
    ld a, h
    out (VDPControl), a
    ld a, $89
    out (VDPControl), a

    ld hl, $a000
    ld (RobAY), hl
    ld (RobBY), hl
    ld hl, $0000
    ld (RobAX), hl
    ld hl, $e000
    ld (RobBX), hl

    SetVDPAddress $3f00 | VRAMWrite
    ld a, $d0
    out (VDPData), a
    
    ; hide left 8 px
    ld a, $34
    out (VDPControl), a
    ld a, $80
    out (VDPControl), a

    ret
    
Intro:
    WaitVBlank 0
    
    ld a, (CurBeatIdx)
    cp 16
    call nc, UploadPalette
    
    ld a, (CurBeatIdx)
    cp 3
    ret c
    cp 5
    jp c, FadeinSlowerBRG
    cp 9
    jp c, @Scroll
    cp 11
    jp c, @Robs
    cp 13
    ret c

    ld a, 1
    ld (UseXScroll), a

    ; line int every line
    ld a, 0
    out (VDPControl), a
    ld a, $8a
    out (VDPControl), a
    
    ; sinus "dissolve"
    
    ld hl, IntroDissCurFrame
    ld a, (hl)
    inc a
    ld (hl), a
    
    ld d, a
    srl d
    ld iyh, d
    add a, a
    add a, a
    ld ixl, a
    add a, a
    ld ixh, a
    
    ld de, XScroll
    
-:
    ld a, e
    
    add a, ixl
    ld c, a
    GetSinC
    
    ld b, iyh
    ld c, a
    call FPMultiplySignedBByC
    ld a, h
    
    ld (de), a
    inc e
    
    ld a, e
    
    add a, ixh
    ld c, a
    GetCosC

    ld b, iyh
    ld c, a
    call FPMultiplySignedBByC
    ld a, h
    
    ld (de), a
    inc e
    
    ld a, e
    cp Height
    jp nz, -

    
    
    ld a, (CurBeatIdx)
    cp 16
    jp z, FadeoutLocalPalette    

    ld a, (CurBeatIdx)
    cp 17
    jp z, NextEffect_JP

    ret
    
@Scroll:
    ; y scroll

    ld bc, $ffd9
    ld hl, (RotoY)
    add hl, bc
    ld (RotoY), hl

    ld a, h
    out (VDPControl), a
    ld a, $89
    out (VDPControl), a

    ret
    
@Robs:
    ; robots movement
    
    ld hl, IntroRobCurFrame
    ld a, (hl)
    inc a
    ld (hl), a
    
    ld c, a
    GetCosC
    add a, 28
    ld (RobAY + 1), a
    ld (RobBY + 1), a
    ld a, (hl)
    add a, 16
    and $7f
    ld c, a
    GetSinC
    srl a
    ld (RobAX + 1), a
    neg
    sub 32
    ld (RobBX + 1), a
    
    call PM7AnimateRobots
    call PM7RobotFlames

    ; upload current sat to VDP
    
    SetVDPAddress $3f00 | VRAMWrite
    ld hl, PM7SAT
    ld c, VDPData
    call SATOuti
    
    ret
    
;==============================================================
; 2D particles code
;==============================================================

PartDoOneInitByte:
    ld c, a

    ; x
    ld a, ixl
    add a, 8
    ld ixl, a
    ld (hl), a
    inc h
    inc h
    ; y
    ld a, 0
    adc a, ixh
    ld ixh, a
    add a, a
    add a, a
    add a, a
    ld (hl), a
    dec h
    dec h

    ld a, c

    rrca
    ret c

    inc l

    ret
    
PartRandomizePos:
    ld b, 64
-:
    random
    srl a
    ld (hl), a
    inc l
    djnz -
    
    ret

PartRandomizeLow:
    ld b, 64
-:
    random
    sra a
    sra a
    ld (hl), a
    inc l
    djnz -
    
    ret
    
PartRandomizeLifeA:
    ld b, 64
-:
    random
    .repeat 5
        srl a
    .endr
    add a, 2
    ld (hl), a
    inc l
    djnz -
    
    ret
    
PartRandomizeThres:
.define @Thres $10
    ld b, 64
-:
    random
    cp @Thres
    jr nc, +
    ld a, @Thres
+:
    ld (hl), a
    inc l
    djnz -
    
    ret
    
ParticlesInitGreets:
    ld a, 42
    ld (RandSeed), a
    xor a
    ld (PartMode), a
    ld de, PartGreets
    jp ParticlesInit

ParticlesInitTitan:
    ; pcm playback
    exx
    ld c, 6
    ld a, c
    ld (MapperSlot1), a
    ld hl, $4000
    exx
    
    ld a, $81
    out (PSGPort), a
    xor a
    out (PSGPort), a

    call ParticlesLoadVRAM

    ld a, 69
    ld (RandSeed), a
    ld a, 1
    ld (PartMode), a
    ld de, PartTitan
    jp ParticlesInit

ParticlesInitSMSPower:
    ld a, 231
    ld (RandSeed), a
    ld a, 1
    ld (PartMode), a
    ld de, PartSMSPower
    jp ParticlesInit
    
ParticlesInitPopsy:
    ld a, 69
    ld (RandSeed), a
    ld a, 1
    ld (PartMode), a
    ld de, PartPopsy
    jp ParticlesInit

ParticlesInitXMen:
    ld a, 69
    ld (RandSeed), a
    ld a, 1
    ld (PartMode), a
    ld de, PartXMen
    jp ParticlesInit

ParticlesInit:
    ld a, 1
    ld (MapperSlot1), a

    ld bc, $4000
    ld hl, PartXH
    ld ix, PartOriginY * 32
-:    
    ld a, (de)
    inc de
    
    .repeat 8
        call PartDoOneInitByte
    .endr
    
    djnz -
    
    ld a, l
    ld (PartCount), a
    
    exx
    ld a, c
    ld (MapperSlot1), a    
    exx
    
    memset PartXL, $00, 64
    memset PartYL, $00, 64
    memset PartXA, $00, 64
    memset PartYA, $00, 64
    memset PartLife, $ff, 64
    
    ld hl, PartLifeA
    call PartRandomizeLifeA

    ld a, (PartMode)
    or a
    jp z, +
    ld hl, PartXA
    call PartRandomizeThres
    ld hl, PartYA
    call PartRandomizeThres
    jp ++
+:    
    ld hl, PartXA
    call PartRandomizeLow
    ld hl, PartYA
    call PartRandomizePos
+:    
++:

    ld a, (CurBeatIdx)
    ld (PartInitialBeat), a
    
    ; SAT terminator
    ld hl, PartSAT
    ld a, (PartCount)
    AddAToHL
    ld a, $d0
    ld (hl), a
    ld hl, PartSAT ; for first update
    ld (hl), a
    
    ret

ParticlesLoadVRAM:
    ; no X scroll
    xor a
    ld (UseXScroll), a

    ; x scroll = 0
    ld a, $00
    out (VDPControl), a
    ld a, $88
    out (VDPControl), a

    ; no line int
    ld a, $ff
    out (VDPControl), a
    ld a, $8a
    out (VDPControl), a

    ld a, 13
    ld (MapperSlot1), a
    
    ; load tiles (Stars)
    SetVDPAddress $0000 | VRAMWrite
    ld hl,StarsTiles
    ld de,StarsTilesSize
    CopyToVDPPCM 13
    
    ; load tilemap (Stars)
    SetVDPAddress $3800 | VRAMWrite
    ld hl,StarsTileMap
    ld de,TileMapSize
    CopyToVDPPCM 13
    
    ; Load palette (Stars)
    memcpy FadeinPalette, StarsPalette, TilePaletteSize
    
    ld a, 1
    ld (MapperSlot1), a

    ; load tiles (Particles)
    SetVDPAddress $2000 | VRAMWrite
    ld hl,PartData
    ld de,PartSize
    CopyToVDPPCM 1

    ; Load palette (VectorBalls)
    memcpy LocalPalette + TilePaletteSize, VBPalette, TilePaletteSize

    ; show left 8 px
    ld a, $14
    out (VDPControl), a
    ld a, $80
    out (VDPControl), a
    
    ld a, %1100000
;          ||||||`- Zoomed sprites -> 16x16 pixels
;          |||||`-- Doubled sprites -> 2 tiles per sprite, 8x16
;          ||||`--- Mega Drive mode 5 enable
;          |||`---- 30 row/240 line mode
;          ||`----- 28 row/224 line mode
;          |`------ VBlank interrupts
;          `------- Enable display
    out (VDPControl), a
    ld a, $81
    out (VDPControl), a
    
    ; SAT address ($3f00)
    ld a, $7f
    out (VDPControl), a
    ld a, $85
    out (VDPControl), a
    
    ret

ParticlesChangePCMBank:
    inc c
    ld a, c
    ld (MapperSlot1), a
    srl h
    
    ret
    
ParticlesSilence:
    call FadeoutSloBeat
    
    ; silence pcm by playing end silence repeately
    exx
    ld c, 11
    ld a, c
    ld (MapperSlot1), a
    ld hl, $6000
    exx
    
    ret
    
Particles:
    WaitVBlank 1
    
    ; upload current sat to VDP
    
    SetVDPAddress $3f00 | VRAMWrite
    ld hl, PartSAT
    ld c, VDPData
    call SATOutiPCM

; c170    
    
    call FadeinPCM

    ld ixh, 0
PartLoop:    
    ; get xyl
    
    ld h, >PartXH
    ld a, ixh
    ld l, a
    ld b, (hl) ; xh
    inc h
    ld c, (hl) ; xl
    inc h
    ld d, (hl) ; yh
    inc h
    ld e, (hl) ; yl
    inc h
    ld a, (hl) ; xa
    ld iyl, a
    inc h
    ld a, (hl) ; ya
    ld iyh, a
    inc h
    ld a, (hl) ; life
    ld ixl, a

    ld a, (CurBeatIdx)
    ld hl, PartInitialBeat
    sub (hl)
    cp PartInitialBeatWait

    jp c, @NoTrans
    cp PartEndBeat
    jp z, NextEffect_JP
    
; c166
    PlaySampleL
    ; apply tranformations
    
    ld a, (PartMode)
    or a
    jp nz, +
    random
    .repeat 4
        sra a
    .endr
    add a, iyl
    ld iyl, a
+:    

; c304  

; c-20

    ; x
    ld a, iyl
    SignExtendAToHL
    .repeat 2
        add hl, hl
    .endr
    add hl, bc
    ld b, h
    ld c, l
    ld a, b
    cp 8
    jr nc, +
    ld de, Height * 256 ; out of screen y
    ld iy, 0 ; zero acceleration
+:    

    ; y
    ld a, iyh
    SignExtendAToHL
    .repeat 2
        add hl, hl
    .endr
    add hl, de
    ex de, hl
    ld a, d
    cp 192
    jr c, +
    ld de, Height * 256 ; out of screen y
    ld iy, 0 ; zero acceleration
+:    
    
; c72

    ; life
    ld a, ixl
    ex de, hl
    ld d, >PartLifeA
    ld e, ixh
    ex de, hl
    ld l, (hl)
    sub l
    ld ixl, a
    jp nc, +
    ld de, Height * 256 ; out of screen y
    ld iy, 0 ; zero acceleration
+:

; c138
    PlaySampleU
    ; save xyl
    
    ld h, >PartXH
    ld a, ixh
    ld l, a
    ld (hl), b ; xh
    inc h
    ld (hl), c ; xl
    inc h
    ld (hl), d ; yh
    inc h
    ld (hl), e ; yl
    inc h
    ld a, iyl
    ld (hl), a ; xa
    inc h
    ld a, iyh
    ld (hl), a ; ya
    inc h
    ld a, ixl
    ld (hl), a ; life
    
; c729

    
@NoTransEnd:
    
    ; sat update
    
    ex de, hl
    ld e, ixh
    ld d, >PartSAT
    ex de, hl
    
    ; sat y
    ld (hl), d
    
    ; sat second part
    set 6, l
    sla l

    ; sat x
    ld (hl), b
    inc l

    ; sat tile index
    ld a, ixl
    .repeat 4
        rrca
    .endr
    and $0f
    ld (hl), a
     
    inc ixh
    ld a, (PartCount)
    cp ixh
    jp nz, PartLoop
; c1002
     
    ret
   
@NoTrans:
    .repeat 7
        inc iy
        dec iy
    .endr
    PlaySampleL
    .repeat 13
        inc iy
        dec iy
    .endr
    PlaySampleU
    jp @NoTransEnd
  
;==============================================================
; VectorBalls code
;==============================================================

.macro VBGetCFromDE
    ex de, hl
    ld c, (hl)
    inc l
    ex de, hl
.endm

.macro VBGetNewCoord
    ; x
    VBGetCFromDE
    ld b, ixl
    FPMultiplySignedBByC_Faster
    ld a, h
    ; y
    VBGetCFromDE
    ld b, iyl
    ex af, af'
    FPMultiplySignedBByC_Faster
    ex af, af'
    add a, h
    ; z
    VBGetCFromDE
    ld b, iyh
    ex af, af'
    FPMultiplySignedBByC_Faster
    ex af, af'
    add a, h
.endm

;
; >>> Quicksort routine v1.1 <<<
; by Frank Yaul 7/14/04 (modified to sort SATs by GliGli)
;
; Usage: bc->first, de->last,
;        call qsort
; Destroys: abcdefhl
;
VBSATSort:
    ld bc, VBSAT + $40
    ld de, VBSAT + $40 + (VBCount - 1)
qsort:
    ld hl,0
    push hl
qsloop:
    ld h,b
    ld l,c
    or a
    sbc hl,de
    jp c,next1 ;loop until lo<hi
    pop bc
    ld a,b
    or c
    ret z       ;bottom of stack
    pop de
    jp qsloop
next1:
    push de      ;save hi,lo
    push bc
    ld a,(bc)  ;pivot
    ld h,a
    dec c
    inc e
fleft:
    inc c      ;do i++ while cur>piv
    ld a,(bc)
    ld l,a
    ld a,h
    cp l
    jp c,fleft
fright:
    dec e
    ld a,(de)
    cp h
    jp c,fright
    push hl      ;save pivot
    ld h,d     ;exit if lo>hi
    ld l,e
    or a
    sbc hl,bc
    jp c,next2
    
    ld l, c
    ld ixl, e
    
    ;swap z
    
    ld a,(bc)
    ld h,a
    ld a,(de)
    ld (bc),a
    ld a,h
    ld (de),a

    ; swap y
    
    res 6, c
    res 6, e
    
    ld a,(bc)
    ld h,a
    ld a,(de)
    ld (bc),a
    ld a,h
    ld (de),a
  
    ;swap x
    
    sla c
    sla e
    set 7, c
    set 7, e    
   
    ld a,(bc)
    ld h,a
    ld a,(de)
    ld (bc),a
    ld a,h
    ld (de),a
    
    ;swap tile index

    inc c
    inc e

    ld a,(bc)
    ld h,a
    ld a,(de)
    ld (bc),a
    ld a,h
    ld (de),a
    
    ld c, l
    ld e, ixl

    pop hl      ;restore pivot
    jp fleft
next2:
    pop hl      ;restore pivot
    pop hl      ;pop lo
    push bc      ;stack=left-hi
    ld b,h
    ld c,l     ;bc=lo,de=right
    jp qsloop

VectorBallsInit:
    ld a, 12
    ld (MapperSlot1), a
    
    ; load tiles (Checkerboard)
    SetVDPAddress $0000 | VRAMWrite
    ld hl,CheckTiles
    ld de,CheckTilesSize
    CopyToVDP
    
    ; load tilemap (Checkerboard)
    SetVDPAddress $3800 | VRAMWrite
    ld hl,CheckTileMap
    ld de,TileMapSize
    CopyToVDP
    
    ; Load palette (Checkerboard)
    memcpy FadeinPalette, CheckPalette, TilePaletteSize
    
    ; load tiles (VectorBalls)
    SetVDPAddress $2000 | VRAMWrite
    ld hl,VBTiles
    ld de,VBTilesSize
    CopyToVDP

    ld a, 1
    ld (MapperSlot1), a

    ; Load palette (VectorBalls)
    memcpy FadeinPalette + TilePaletteSize, VBPalette, TilePaletteSize

    ; load tiles (Shadow)
    SetVDPAddress $3600 | VRAMWrite
    ld hl,PartData
    ld de,TileSize
    CopyToVDP

    ; init X
    memcpy VBX, VBInitX, VBCount

    ; init Y
    memcpy VBY, VBInitY, VBCount
    
    ; init Z
    memcpy VBZ, VBInitZ, VBCount

    ; 
    ld a, %1100010
;          ||||||`- Zoomed sprites -> 16x16 pixels
;          |||||`-- Doubled sprites -> 2 tiles per sprite, 8x16
;          ||||`--- Mega Drive mode 5 enable
;          |||`---- 30 row/240 line mode
;          ||`----- 28 row/224 line mode
;          |`------ VBlank interrupts
;          `------- Enable display
    out (VDPControl), a
    ld a, $81
    out (VDPControl), a

    ; VDP tilemap pointer
    ld a, $ff
    out (VDPControl), a
    ld a, $82
    out (VDPControl), a

    ; SAT address ($3f00)
    ld a, $7f
    out (VDPControl), a
    ld a, $85
    out (VDPControl), a
    
    ; SAT terminator
    ld hl, VBSAT + VBCount * 2
    ld a, $d0
    ld (hl), a
    ld hl, VBSAT ; for first update
    ld (hl), a
    
    xor a
    ld (VBRX), a
    ld (VBRY), a
    ld (VBRZ), a
    
    ld (VBCurStepBeat), a
    ld hl, VBSequence
    ld (VBCurStep), hl
    
    ret

VectorBalls:
    WaitVBlank 0
    
    ; upload current sat to VDP
    
    SetVDPAddress $3f00 | VRAMWrite
    ld hl, VBSAT
    ld c, VDPData
    call SATOuti

    ; advance sequencer

-:    
    ld hl, (VBCurStep)
    ld a, (VBCurStepBeat)
    ld c, (hl)
    cp c
    jp nz, +
    ; to next step
    xor a
    ld (VBCurStepBeat), a
    ld de, VBSequenceOne - VBSequence
    add hl, de
    ld (VBCurStep), hl
    jp -
+:
    ; shift beat counter
    inc hl
    ld a, (BeatCounter)
    cp 0
    jr nz, +
    ; update cur step beat
    ld de, VBCurStepBeat
    ex de, hl
    inc (hl)
    ex de, hl
+:    
    ld b, (hl)
    jp +
-:
    srl a
+:
    djnz -
    ; add it to max 2 coords
    ld b, a
    .repeat 2
        inc hl
        ld e, (hl)
        inc hl
        ld d, (hl)
        ld a, (de)
        add a, b
        ld (de), a
    .endr

    ; flags
    inc hl
    ld a, (hl)
    bit 0, a
    jp z, +
    call Fadein
    jp ++
+:    
    bit 1, a
    jp z, +
    call Fadeout
    jp ++
+:    
    bit 7, a
    jp nz, NextEffect_JP
++:
    
    ; get sin / cos of angles
    
    ld de, VBSinCos
    ld hl, VBRX
    .repeat 3
        ld c, (hl)
        inc l
        GetSinC
        ld (de), a
        inc e
        GetCosC
        ld (de), a
        inc e
    .endr

    ; compute coordinates factors

    ld de, VBFactors

    ; x
    ld a, (VBSinCos + 3) ; cos b
    ld c, a
    ld a, (VBSinCos + 1) ; cos a
    ld b, a
    call FPMultiplySignedBByC
    ld a, h
    ld (de), a
    inc e
    ld a, (VBSinCos + 0) ; sin a
    ld b, a
    call FPMultiplySignedBByC
    ld a, h
    ld (de), a
    inc e
    ld a, (VBSinCos + 2) ; sin b
    ld (de), a
    inc e

    ; y pre
    ld a, (VBSinCos + 2) ; sin b
    ld b, a
    ld a, (VBSinCos + 4) ; sin c
    ld c, a
    call FPMultiplySignedBByC
    ld b, h
    ld ixl, b ; sin b * sin c
    ld a, (VBSinCos + 1) ; cos a
    ld c, a
    call FPMultiplySignedBByC
    ld a, h
    ld iyl, a ; cos a * sin b * sin c
    ld a, (VBSinCos + 0) ; sin a
    ld b, a
    ld c, ixl
    call FPMultiplySignedBByC
    ld a, h
    ld iyh, a ; sin a * sin b * sin c
    ; y
    ld a, (VBSinCos + 5) ; cos c
    ld c, a
    ld a, (VBSinCos + 0) ; sin a
    ld b, a
    call FPMultiplySignedBByC
    ld a, h
    add a, iyl
    ld (de), a
    inc e
    ld a, (VBSinCos + 1) ; cos a
    ld b, a
    call FPMultiplySignedBByC
    ld a, iyh
    sub h
    ld (de), a
    inc e
    ld a, (VBSinCos + 3) ; cos b
    neg
    ld b, a
    ld a, (VBSinCos + 4) ; sin c
    ld c, a
    call FPMultiplySignedBByC
    ld a, h
    ld (de), a
    inc e
    
    ; z pre
    ld a, (VBSinCos + 2) ; sin b
    ld b, a
    ld a, (VBSinCos + 5) ; cos c
    ld c, a
    call FPMultiplySignedBByC
    ld b, h
    ld ixl, b ; sin b * cos c
    ld a, (VBSinCos + 1) ; cos a
    ld c, a
    call FPMultiplySignedBByC
    ld a, h
    ld iyl, a ; cos a * sin b * cos c
    ld a, (VBSinCos + 0) ; sin a
    ld b, a
    ld c, ixl
    call FPMultiplySignedBByC
    ld a, h
    ld iyh, a ; sin a * sin b * cos c
    ; z
    ld a, (VBSinCos + 4) ; sin c
    ld c, a
    ld a, (VBSinCos + 0) ; sin a
    ld b, a
    call FPMultiplySignedBByC
    ld a, h
    sub iyl
    ld (de), a
    inc e
    ld a, (VBSinCos + 1) ; cos a
    neg
    ld b, a
    call FPMultiplySignedBByC
    ld a, h
    sub iyh
    ld (de), a
    inc e
    ld a, (VBSinCos + 3) ; cos b
    ld b, a
    ld a, (VBSinCos + 5) ; cos c
    ld c, a
    call FPMultiplySignedBByC
    ld a, h
    ld (de), a

    xor a
    ld ixh, a

VBLoop:    
    ; get xyz

    ld h, >VBX
    ld l, a ; ixh
    ld a, (hl) ; x
    ld ixl, a
    inc h ; >VBY
    ld a, (hl) ; y
    ld iyl, a
    inc h ; >VBZ
    ld a, (hl) ; z
    ld iyh, a

    ; apply tranformations

    ld de, VBFactors

    VBGetNewCoord
    push af
    VBGetNewCoord
    push af
    VBGetNewCoord
    
    add a, VBOriginZ
    ld l, a
    pop af
    ld e, a
    pop af
    ld c, a
    
    ; project & build SAT

    ld h, >InvLUT
    ld b, (hl) ; 1 / z
    
    call FPMultiplySignedBByC
    ld d, h

    ld c, e ; py
    call FPMultiplySignedBByC

    ; add x / y 2d origin
    
    ld a, d
    add a, VBOriginX
    ld c, a
    ld a, h ; py
    add a, VBOriginY

    ; don't let sprites y become $d0 (terminator)
    ; ld d, 192
    ; cp d
    ; jr c, +
    ; ld a, d
; +:   

    ; main sprite
    
    ex de, hl
    ld e, ixh
    ld d, >VBSAT
    ex de, hl
    
    ; sat y
    ld (hl), a

    ; sat z
    set 6, l
    ld (hl), b
    
    ; sat second part
    sla l

    ; sat x
    ld (hl), c
    inc l

    ; sat tile index
    ld a, b
    ld d, $3c
    and d
    rrca
    ld ixl, a

    ; stretch effect
    ld a, (BeatCounter)
    sub 8
    
    add a, a
    sub b
    add a, a
    
    jp m, +
    xor a
    jp ++
+:    
    ld d, >SSqrLUT + 1
    ld e, a
    ld a, (de)
    add a, a
    and $66
    
++:
    add a, ixl
    ld (hl), a

    ; shadow sprite
    
    ld a, ixh
    add a, VBCount
    ld l, a
    ld h, >VBSAT
    
    ; sat y (vb z)
    ld a, b
    add a, VBOriginYShadow
    ld (hl), a

    ; sat second part
    set 6, l
    sla l

    ; sat x
    ld (hl), c
    inc l

    ; sat tile index
    ld a, $b0
    ld (hl), a
    
    
    inc ixh
    ld a, ixh
    cp VBCount
    jp nz, VBLoop
    
    call VBSATSort

    ret

;==============================================================
; RotoZoom code
;==============================================================

.macro RotoZoomFromRotScale args toV, readScl
    .ifeq readScl 1
        ld de, (RotoScl) ; vy
    .endif

    ld bc, (RotoRot) ; vx
    GetCosC
    call FPMultiplySignedAByDE    
    push hl
    
    ld bc, (RotoRot) ; vx
    GetSinC
    call FPMultiplySignedAByDE    
    ld d, h
    ld e, l
    
    pop bc

    .ifeq toV 1
        ld (RotoVX), bc
        ld (RotoVY), de
    .endif
.endm

.macro RotoZoomX args times, other
    ; x offset
    .repeat times
        .ifeq other 1
            add ix, de
        .else
            add ix, bc
        .endif
    .endr
.endm

.macro RotoZoomY args times, other
    ; y offset
    .repeat times
        .ifeq other 1
            add iy, bc
        .else
            add iy, de
        .endif
    .endr
.endm

.macro RotoZoomPushIncs
    ; x coord
    ld a, ixh
    ld l, a

    ; y coord
    ld a, iyh
    ld h, a

    push hl
.endm

.macro RotoZoomBakeIncs
    exx

    ld a, (de)
    inc e
    ld c, a
    add hl, bc

    ; x coord
    ld a, ixh
    ld (hl), a

    inc hl

    ; y coord
    ld a, iyh
    add a, a
    ld (hl), a

    exx

    ; we want increments
    ld ixh, 0
    ld iyh, 0
.endm

.macro RotoZoomGetPixel args idx
.ifeq idx 0
RotoRAMCodeBakePos0:
.endif
.ifeq idx 1
RotoRAMCodeBakePos1:
.endif
.ifeq idx 2
RotoRAMCodeBakePos2:
.endif
.ifeq idx 3
RotoRAMCodeBakePos3:
.endif
.ifeq idx 4
RotoRAMCodeBakePos4:
.endif
    ld bc, $aa55 ; placeholder value (x/y coordinates will be copied in)

    sla h
    add hl, bc ; add line x/y offset
    srl h ; texture starts at $0000

    add a, a
    or (hl)

    ex de, hl

    sla h
    add hl, bc ; add line x/y offset
    srl h ; texture starts at $0000

    add a, a
    or (hl)

    ex de, hl
.endm

.macro RotoGetLineOffsets
    ld h, >RotoPrecalcData
    ; advance hl "line pair" items
    ld l, a
    ; even line
    ; y coord
    ld c, (hl)
    inc l
    ; x coord
    ld b, (hl)
    inc l
    ; odd line
    ; y coord
    ld e, (hl)
    inc l
    ; x coord
    ld d, (hl)
    ; fixup regs
    ld h, b
    ld l, c
.endm

.macro RotoSeqAddToCoord args addr, times
    ld a, (hl)
    inc hl
    ld de, (addr)
    SignExtendAToBC
    ex de, hl
    .repeat times
        add hl, bc
    .endr
    ex de, hl
    ld (addr), de
.endm
    
.macro RotoSeqLoadCoord args addr
    ld a, (hl)
    inc hl
    ld (addr), a
.endm
    
RotoZoomInit:
    ; no X scroll
    xor a
    ld (UseXScroll), a

    ; no line int
    ld a, $ff
    out (VDPControl), a
    ld a, $8a
    out (VDPControl), a

    ; y scroll =  0
    ld a, $00
    out (VDPControl), a
    ld a, $89
    out (VDPControl), a

    ; x scroll = 0
    ld a, $00
    out (VDPControl), a
    ld a, $88
    out (VDPControl), a

    ; show left 8 px
    ld a, $14
    out (VDPControl), a
    ld a, $80
    out (VDPControl), a

    memset XScroll, 0, Height
    
    ld a, 1
    ld (MapperSlot1), a
   
    ; load tiles (Monochrome framebuffer emulation)
    SetVDPAddress $0000 | VRAMWrite
    ld hl,MonoFBTiles
    ld de,MonoFBTilesSize
    CopyToVDP

    ; load tiles (Right columnn * 2)
    SetVDPAddress $2000 | VRAMWrite
    ld hl,Col1Tiles
    ld de,Col1TilesSize
    CopyToVDP
    SetVDPAddress $2800 | VRAMWrite
    ld hl,Col2Tiles
    ld de,Col2TilesSize
    CopyToVDP

    ; load tilemaps (Right column)
    SetVDPAddress $3000 | VRAMWrite
    ld hl,Col2TileMap
    ld de,TileMapSize
    CopyToVDP
    SetVDPAddress $3800 | VRAMWrite
    ld hl,Col2TileMap
    ld de,TileMapSize
    CopyToVDP
    
    ; Load palette
    memcpy FadeinPalette, RotoPalette, TilePaletteSize
    memcpy FadeinPalette + TilePaletteSize, ColPalette, TilePaletteSize

    ; texture slot
    ld a, 3
    ld (MapperSlot1), a

    ; copy code to RAM, duplicating it
    ld de, RAMCode
    .repeat RotoColumnCount / 2
        ld hl, RotoRAMCodeStart
        ld bc, RotoRAMCodeEnd - RotoRAMCodeStart
        ldir
    .endr
    ld bc, RotoRAMCodeRet - RotoRAMCodeEnd
    ldir

    ; get offset increments in RAM code where data will be copied in
    ld hl, RotoRAMBakeIncsEnd - 4 * RotoColumnCount
    ld d, RotoRAMCodeBakePos0 - RotoRAMCodeStart + 1
    ld e, 256 - 4 * RotoColumnCount
-:
    ld (hl), d
    inc l

    ld a, e
    and $03
    cp 3
    jp nz, +
    ld d, RotoRAMCodeBakePos4 - RotoRAMCodeBakePos3 - 1
    jp ++
+:
    cp 1
    jp nz, +
    ld d, RotoRAMCodeBakePos2 - RotoRAMCodeBakePos1 - 1
    jp ++
+:
    ld d, RotoRAMCodeBakePos1 - RotoRAMCodeBakePos0 - 1
++:

    inc e
    jp nz, -

    ; coords init
    ld hl, $0000
    ld (RotoRot), hl
    ld hl, $f800
    ld (RotoX), hl
    ld hl, $fc00
    ld (RotoY), hl
    ld hl, $0100
    ld (RotoScl), hl

    ; sequencer init
    ld hl, RotoSequence
    ld (RotoCurStep), hl
    ld a, (CurBeatIdx)
    add a, (hl)
    ld (RotoNextStepBeat), a

    ret
    
RotoLoadOtherColTilemap:
    ld a, (MapperSlot1)
    push af
    
    ld a, 1
    ld (MapperSlot1), a

    ld b, 24
    ld c, VDPData
    ld hl, Col1TileMap
-:
    ld a, 24 * 2
    AddAToHL
    ld a, 24 * 2
    AddAToDE
    
    ld a, e
    out (VDPControl), a
    ld a, d
    out (VDPControl), a
  
    .repeat 8 * 2
        outi
    .endr
    
    ld a, 8 * 2
    AddAToDE
    ld a, 8 * 2
    add a, b
    ld b, a

    djnz -
    
    pop af
    ld (MapperSlot1), a
    ret
    
RotoZoomMonoFB:
    WaitVBlank 0
    
    ; VDP tilemap pointer (double buffering)
    ld a, (CurFrameIdx)
    and 1
    ld a, $ff
    jr z, +
    ld a, $fd
+:
    out (VDPControl), a
    ld a, $82
    out (VDPControl), a

    ; advance sequencer

-:    
    ld hl, (RotoCurStep)
    ; beat counter
    ld a, (CurBeatIdx)
    ld c, a
    ld a, (RotoNextStepBeat)
    cp c
    jr nz, +
    ; to next step
    ld de, RotoSequenceOne - RotoSequence
    add hl, de
    ld (RotoCurStep), hl
    ld a, (CurBeatIdx)
    add a, (hl)
    ld (RotoNextStepBeat), a
    jp -
+:
    inc hl

    RotoSeqAddToCoord RotoRot, 1
    RotoSeqAddToCoord RotoScl, 1
    RotoSeqAddToCoord RotoX, 1
    RotoSeqAddToCoord RotoY, 1
    
    ld a, (hl)
    bit 2, a
    jp z, +
    ld a, (RotoLoadedOtherCol)
    or a
    jp nz, ++
    ld de, $3000 | VRAMWrite
    call RotoLoadOtherColTilemap
    ld de, $3800 | VRAMWrite
    call RotoLoadOtherColTilemap
    ld a, 1
    ld (RotoLoadedOtherCol), a
    jp ++
+:    
    bit 0, a
    jp z, +
    call Fadein
    call Fadein
    jp ++
+:    
    bit 1, a
    jp z, +
    call Fadeout
    call Fadeout
    jp ++
+:    
    bit 7, a
    jp nz, NextEffect_JP
++:
    
    ; precaclulate increments for one line

    exx
    RotoZoomFromRotScale 0, 1
   
    ld ix, (RotoX) ; x
    ld iy, (RotoY) ; y
    exx

RotoDoPrecalc:
    ld (SPSave), sp

    ld sp, RotoPrecalcData + RotoLineCount * 4
    exx
    NegateDE
    .repeat RotoLineCount * 2
        RotoZoomPushIncs
        RotoZoomX 2, 1
        RotoZoomY 2, 1
    .endr
    NegateDE
    exx

    ; copy line pixels offsets into RAM code

    ld sp, (SPSave)

    ld de, RotoRAMBakeIncsEnd - 4 * RotoColumnCount
    ld hl, RAMCode

    ld ix, (RotoX) ; x
    ld iy, (RotoY) ; y

    ld b, 0
-:
    exx
    .repeat RotoColumnCount / 2
        RotoZoomX 1, 0
        RotoZoomY 1, 0
        RotoZoomBakeIncs 1
    .endr
    exx

    xor a
    cp e
    jp nz, -

;    ret
RotoPrecalcEnd:

    ld sp, (SPSave)
    ld l, RotoLineCount ; line counter

    ; main loop on lines pairs

RotoLineLoop:
    .ifneq RotoColumnCount 32
        ; VRAM pointer position
        ex de, hl
        ld a, RotoLineCount
        sub e
        ld l, a
        ld h, 0
        ;line index to vram offset (line * 64)
        add hl, hl
        add hl, hl
        add hl, hl
        add hl, hl
        add hl, hl
        add hl, hl
        dec hl
        ld a, l
        out (VDPControl), a
        
        ld a, (CurFrameIdx)
        rrca    
        ld a, h
        jr nc, +
        add a, $08
    +:    
        add a, >VRAMWrite + $30
        out (VDPControl), a
        ex de, hl
    .endif
    
    ld a, l
    dec a
    add a, a
    add a, a
    exx
    RotoGetLineOffsets
    
    

    jp RAMCode
RotoRAMCodeStart:
    .repeat 2 index x_dummy
        RotoZoomGetPixel x_dummy * 4 + 0
        RotoZoomGetPixel x_dummy * 4 + 1
        ex af, af'
        in a, (VDPData)
        ex af, af'
        RotoZoomGetPixel x_dummy * 4 + 2
        RotoZoomGetPixel x_dummy * 4 + 3
        out (VDPData), a
    .endr
RotoRAMCodeEnd:
    jp RotoRAMCodeRet
RotoRAMCodeRet:

    exx
    
    dec l
    jp nz, RotoLineLoop

    ret

;==============================================================
; Pseudo mode 7 code
;==============================================================

.macro PM7GetLineIncs
    ld h, >RotoPrecalcData
    ; advance hl "line pair" items
    ld l, a
    ; even line
    ; x coord
    ld c, (hl)
    inc l
    ; y coord
    ld b, (hl)
    inc l
    ; y inc
    ld a, (hl)
    inc l
    exx
    ld e, a
    exx
    ld a, (hl)
    inc l
    exx
    ld d, a
    exx
    ; x inc
    ld a, (hl)
    inc l
    exx
    ld c, a
    exx
    ld a, (hl)
    inc l
    exx
    ld b, a
    exx
    ; odd line
    ; x coord
    ld e, (hl)
    inc l
    ; y coord
    ld d, (hl)
    inc l
.endm

.macro PM7GetPixel args idx
    .ifeq (idx >> 7) 0
        ; improve y resolution on first half of the line (odd line update)
        .ifeq (idx & 1) 1
            exx
            RotoZoomX 1, 0
            RotoZoomY 1, 0
            exx
        .endif
    .else
        .ifeq (idx & 1) 0
            exx
            RotoZoomX 1, 0
            RotoZoomY 1, 0
            exx
        .endif
    .endif


    ex de, hl
    ld d, iyh
    ld e, ixh
    ex de, hl
    
    .ifeq (idx & 1) 0
        add hl, bc ; add line x/y offset
    .else
        add hl, de ; add line x/y offset
    .endif

    srl h ; texture starts at $0000

    add a, a
    or (hl)
.endm

.macro PM7SATYSet args offset, len
    ld hl, PM7SAT + offset
    ld de, PM7SAT + offset + 1
    ld bc, len - 1
    ld (hl), a
    ldir
.endm

.macro PM7SATXSet args offset
    ld (hl), a
    add hl, de
    ld (hl), a
    add a, offset
    add hl, de
    ld (hl), a
    add hl, de
    ld (hl), a
    sub offset
.endm

PM7AnimateRobots:
    ld a, (RobAY + 1)
    PM7SATYSet $00, 4
    add a, $10
    PM7SATYSet $08, 4
    add a, $10
    PM7SATYSet $10, 4
    add a, $10
    PM7SATYSet $18, 4
    
    ld a, (RobBY + 1)
    PM7SATYSet $04, 4
    add a, $10
    PM7SATYSet $0c, 4
    add a, $10
    PM7SATYSet $14, 4
    add a, $10
    PM7SATYSet $1c, 4
    
    ld a, (RobAX + 1)
    ld de, $10
    ld hl, PM7SAT + $80
    PM7SATXSet -4
    ld hl, PM7SAT + $82
    add a, 8
    PM7SATXSet -4
    ld hl, PM7SAT + $84
    add a, 8
    PM7SATXSet 3
    ld hl, PM7SAT + $86
    add a, 8
    PM7SATXSet 3

    ld a, (RobBX + 1)
    ld de, $10
    ld hl, PM7SAT + $88
    PM7SATXSet -4
    ld hl, PM7SAT + $8a
    add a, 8
    PM7SATXSet -4
    ld hl, PM7SAT + $8c
    add a, 8
    PM7SATXSet 3
    ld hl, PM7SAT + $8e
    add a, 8
    PM7SATXSet 3

    ret
    
PM7RobotFlames:
    ; rotating flames colors (while no fadein/out)
    ld a, (CurFrameIdx)
    and $03
    ld hl, RobFlyFlamesPal
    AddAToHL
    ld de, LocalPalette + VDPPaletteSize - 4
    ld bc, 3
    ldir
    jp UploadPalette
        
 
PseudoMode7Init:
    call ClearTileMap
    
    ld a, 1
    ld (MapperSlot1), a

    ; load tiles (Monochrome framebuffer emulation)
    SetVDPAddress $0000 | VRAMWrite
    ld hl,MonoFBTiles
    ld de,MonoFBTilesSize
    CopyToVDP

    ; Load palette
    memcpy FadeinPalette, PM7Palette, TilePaletteSize

    ; robots slot
    ld a, 12
    ld (MapperSlot1), a

    ; load tiles (Flying robots)
    SetVDPAddress $2000 | VRAMWrite
    ld hl,RobFlyTiles
    ld de,RobFlyTilesSize
    CopyToVDP

    ; Load palette (Flying robots)
    memcpy FadeinPalette + TilePaletteSize, RobFlyPalette, TilePaletteSize

    memcpy PM7SAT, RobFlySAT, 256
    
    ; bg slot
    ld a, 13
    ld (MapperSlot1), a

    ; load tiles (Background)
    SetVDPAddress $25c0 | VRAMWrite
    ld hl, PM7BGTiles
    ld de, PM7BGTilesSize
    CopyToVDP

    ; load tilemap (Background)
    SetVDPAddress $3800 | VRAMWrite
    ld hl,PM7BGTileMap
    ld de,TileMapSize / 2
    CopyToVDP

    ; texture slot
    ld a, 14
    ld (MapperSlot1), a

    memset XScroll, $80, Height
    ld a, 1
    ld (UseXScroll), a

    ; hide left 8 px
    ld a, $34
    out (VDPControl), a
    ld a, $80
    out (VDPControl), a
    
    ld a, %1100010
;          ||||||`- Zoomed sprites -> 16x16 pixels
;          |||||`-- Doubled sprites -> 2 tiles per sprite, 8x16
;          ||||`--- Mega Drive mode 5 enable
;          |||`---- 30 row/240 line mode
;          ||`----- 28 row/224 line mode
;          |`------ VBlank interrupts
;          `------- Enable display
    out (VDPControl), a
    ld a, $81
    out (VDPControl), a

    ; line int
    ld a, $5f
    out (VDPControl), a
    ld a, $8a
    out (VDPControl), a
    
    ; coords init
    ld hl, $fec0
    ld (RotoRot), hl
    ld hl, $0280
    ld (RotoScl), hl
    ld hl, $0000
    ld (RotoX), hl
    ld hl, $e000
    ld (RotoY), hl
    ld hl, $7000
    ld (RobAY), hl
    ld (RobBY), hl
    ld hl, $3000
    ld (RobAX), hl
    ld hl, $b000
    ld (RobBX), hl

    call PM7AnimateRobots
    
    ; sequencer init
    ld hl, PM7Sequence
    ld (RotoCurStep), hl
    ld a, (CurBeatIdx)
    add a, (hl)
    ld (RotoNextStepBeat), a

    ret
    
PM7ComputePerLineIncs:
    exx
    ld de, (RotoScl)
    call MultiplyUnsignedAByDE
    ld d, a
    ld e, h
   
    RotoZoomFromRotScale 0, 0

    ld h, d
    ld l, e
    NegateDE
    exx

    ; x coord
    ld a, ixh
    ld e, a

    ; y coord
    ld a, iyh
    add a, a
    ld d, a

    ld (hl), e
    inc l
    ld (hl), d
    inc l
    
    exx
    RotoZoomX 2, 1
    RotoZoomY 2, 1

    add hl, hl

    push bc
    push hl
    exx

    .repeat 2
        pop de
        ld (hl), e
        inc l
        ld (hl), d
        inc l
    .endr

    ret

PM7ComputePerLineIncsSimple:
    ; x coord
    ld a, ixh
    ld e, a

    ; y coord
    ld a, iyh
    add a, a
    ld d, a

    ld (hl), e
    inc l
    ld (hl), d
    inc l
    
    exx
    RotoZoomX 2, 1
    RotoZoomY 2, 1
    exx

    ret

PseudoMode7MonoFB:
    WaitVBlank 0
; upload current sat to VDP
    
    SetVDPAddress $3f00 | VRAMWrite
    ld hl, PM7SAT
    ld c, VDPData
    call SATOuti

    ; advance sequencer

-:    
    ld hl, (RotoCurStep)
    ; beat counter
    ld a, (CurBeatIdx)
    ld c, a
    ld a, (RotoNextStepBeat)
    cp c
    jr nz, +
    ; to next step
    ld de, PM7SequenceOne - PM7Sequence
    add hl, de
    ld (RotoCurStep), hl
    ld a, (CurBeatIdx)
    add a, (hl)
    ld (RotoNextStepBeat), a
    jp -
+:
    inc hl
    
    RotoSeqAddToCoord RotoRot, 1
    RotoSeqAddToCoord RotoScl, 1
    RotoSeqLoadCoord RotoTX, 1
    RotoSeqLoadCoord RotoTY, 1
    RotoSeqAddToCoord RobAX, 2
    RotoSeqAddToCoord RobAY, 2
    RotoSeqAddToCoord RobBX, 2
    RotoSeqAddToCoord RobBY, 2
    
    ld a, (hl)
    bit 0, a
    jp z, +
    call Fadein
    jp ++
+:    
    bit 1, a
    jp z, +
    call Fadeout
    jp ++
+:    
    push af
    
    call PM7RobotFlames
    
    pop af
    
    bit 2, a
    jp z, +
    ld a, 15
    ld (MapperSlot1), a
    jp ++
+:    
    bit 7, a
    jp nz, NextEffect_JP
++:

    ; RotoTX/TY -> RotoX/Y

    RotoZoomFromRotScale 0, 1
    push bc
    .repeat 2
        srl d
        rr e
    .endr
    ld a, (RotoTX)
    ld c, e
    ld b, a
    call FPMultiplySignedBByC
    ld de, (RotoX)
    ex de, hl
    or a
    sbc hl, de
    ld (RotoX), hl

    pop de
    .repeat 2
        srl d
        rr e
    .endr
    ld a, (RotoTX)
    ld c, e
    ld b, a
    call FPMultiplySignedBByC
    ld de, (RotoY)
    ex de, hl
    add hl, de
    ld (RotoY), hl
    
    ; upload current tilemap to VDP
    ld b, PM7LineCount
    ld c, VDPData
    ld de, $3dc0 | VRAMWrite
    ld hl, PM7TMEnd - 1
-:    
    ld a, e
    out (VDPControl), a
    ld a, d
    out (VDPControl), a

    push bc
    
    .repeat 32
        outd
        ind
    .endr
    
    ld bc, -64
    ex de, hl
    add hl, bc
    ex de, hl

    pop bc

    dec b
    jp nz, -

    ; Robots animation
    
    call PM7AnimateRobots
    
    ; X scroll
    ld a, (RotoRot)
    sub 76
    ld (XScroll), a
    
    ; precaclulate increments for one line

    ld ix, (RotoX) ; x
    ld iy, (RotoY) ; y

PM7DoPrecalc:
    ld hl, RotoPrecalcData

    .repeat PM7LineCount index y_line
        ld a, 255 / (1 + (1 - y_line / PM7LineCount) * PM7Fov)
        call PM7ComputePerLineIncs
        call PM7ComputePerLineIncsSimple
    .endr

PM7PrecalcEnd:

    exx
    ld a, PM7LineCount
    ld hl, PM7CurLine
    ld (hl), a
    exx
    
    ld (SPSave), sp
    ld sp, PM7TMEnd

    ; main loop on lines pairs

PM7LineLoop:
    ld ix, 0 ; x
    ld iy, 0 ; y
    
    exx
    ld l, (hl)
    ld h, 0
    
    ; for line incs (a * 8)
    ld a, PM7LineCount
    sub l
    ld l, a
    add a, a
    add a, a
    add a, a
    exx

    PM7GetLineIncs

    .repeat 32 index x_byte
        .repeat 8 index x_bit
            PM7GetPixel x_byte * 8 + x_bit
        .endr
        push af
        
        .ifeq x_byte 15
            ; mirror plane
            NegateIX
            NegateIY
            ; improve y resolution on second half of the line
            exx
            ld a, d
            exx
            add a, b
            ld b, a
       .endif
    .endr

    exx
    ld hl, PM7CurLine
    dec (hl)
    exx
    jp nz, PM7LineLoop

    ld sp, (SPSave)
    ret

;==============================================================
; Data (sequencers)
;==============================================================

.bank 2 slot 2

VBSequence:
    .db 1       ; beats per step
    .db 1       ; counter right shift + 1
    .dw 0       ; first coord address
    .dw 0       ; second coord address
    .db 1       ; flags
VBSequenceOne:
   
    .db 1
    .db 5
    .dw VBRX
    .dw 0
    .db 0
    
    .db 1
    .db 4
    .dw VBRX
    .dw 0
    .db 0
    
    .db 1
    .db 3
    .dw VBRX
    .dw 0
    .db 0

    .repeat 2
        .db 1
        .db 5
        .dw VBRX
        .dw VBRY
        .db 0

        .db 1
        .db 4
        .dw VBRX
        .dw VBRY
        .db 0
    .endr

    .repeat 2 index idx
        .db 2
        .db 6 - idx
        .dw VBRY
        .dw VBRZ
        .db 0
    .endr
    
    .db 3
    .db 4
    .dw VBRZ
    .dw VBRX
    .db 0
    
    .db 1
    .db 4
    .dw VBRZ
    .dw VBRX
    .db 2
    
    .db 255
    .db 1
    .dw 0
    .dw 0
    .db $80
VBSequenceEnd:

RotoSequence:
    .db 1       ; beats per step
    .db 0       ; rotation inc
    .db 0       ; scale inc
    .db 0       ; x inc
    .db 0       ; y inc
    .db 1       ; flags
RotoSequenceOne:    

    .db 7       ; beats per step
    .db 0       ; rotation inc
    .db 0       ; scale inc
    .db 96      ; x inc
    .db 32      ; y inc
    .db 1       ; flags

    .db 8       ; beats per step
    .db 0       ; rotation inc
    .db 4       ; scale inc
    .db 16      ; x inc
    .db 64      ; y inc
    .db 0       ; flags

    .db 8       ; beats per step
    .db 2       ; rotation inc
    .db 6       ; scale inc
    .db 16      ; x inc
    .db 127     ; y inc
    .db 4       ; flags

    .db 7       ; beats per step
    .db 2       ; rotation inc
    .db -12     ; scale inc
    .db 96      ; x inc
    .db 127      ; y inc
    .db 0       ; flags

    .db 1       ; beats per step
    .db 2       ; rotation inc
    .db -12     ; scale inc
    .db 96      ; x inc
    .db 127      ; y inc
    .db 2       ; flags

    .db 255     ; beats per step
    .db 0       ; rotation inc
    .db 0       ; scale inc
    .db 0       ; x inc
    .db 0       ; y inc
    .db $80     ; flags
RotoSequenceEnd:

PM7Sequence:
    .db 3       ; beats per step
    .db 0       ; rotation inc
    .db 0       ; scale inc
    .db 3       ; x inc
    .db 0       ; y inc
    .db 127     ; rax inc
    .db 0       ; ray inc
    .db -127    ; rbx inc
    .db 0       ; rby inc
    .db 1       ; flags
PM7SequenceOne:    

    .db 1       ; beats per step
    .db 0       ; rotation inc
    .db -8      ; scale inc
    .db 3       ; x inc
    .db 0       ; y inc
    .db 0       ; rax inc
    .db 127     ; ray inc
    .db 0       ; rbx inc
    .db 127     ; rby inc
    .db 0       ; flags

    .db 2       ; beats per step
    .db 0       ; rotation inc
    .db -4      ; scale inc
    .db 3       ; x inc
    .db 0       ; y inc
    .db 0       ; rax inc
    .db 64      ; ray inc
    .db 0       ; rbx inc
    .db 64      ; rby inc
    .db 0       ; flags

    .db 2       ; beats per step
    .db 0       ; rotation inc
    .db 0       ; scale inc
    .db 4       ; x inc
    .db 0       ; y inc
    .db 0       ; rax inc
    .db 0       ; ray inc
    .db 0       ; rbx inc
    .db 0       ; rby inc
    .db 0       ; flags

    .db 2       ; beats per step
    .db -1      ; rotation inc
    .db 0       ; scale inc
    .db 3       ; x inc
    .db 0       ; y inc
    .db -32     ; rax inc
    .db 0       ; ray inc
    .db -64    ; rbx inc
    .db 0       ; rby inc
    .db 0       ; flags

    .db 2       ; beats per step
    .db 0       ; rotation inc
    .db 0       ; scale inc
    .db 4       ; x inc
    .db 0       ; y inc
    .db 32      ; rax inc
    .db 0       ; ray inc
    .db 64     ; rbx inc
    .db 0       ; rby inc
    .db 0       ; flags

    .db 3       ; beats per step
    .db 2       ; rotation inc
    .db 0       ; scale inc
    .db 3       ; x inc
    .db 0       ; y inc
    .db 127       ; rax inc
    .db 0       ; ray inc
    .db 64       ; rbx inc
    .db 0       ; rby inc
    .db 4       ; flags

    .db 5       ; beats per step
    .db 0       ; rotation inc
    .db 2       ; scale inc
    .db 5       ; x inc
    .db 0       ; y inc
    .db -77     ; rax inc
    .db -32     ; ray inc
    .db -39     ; rbx inc
    .db -32     ; rby inc
    .db 0       ; flags

    .db 4       ; beats per step
    .db -1      ; rotation inc
    .db -1      ; scale inc
    .db 4       ; x inc
    .db 0       ; y inc
    .db -32     ; rax inc
    .db 16      ; ray inc
    .db -64     ; rbx inc
    .db 16      ; rby inc
    .db 0       ; flags

    .db 4       ; beats per step
    .db 0       ; rotation inc
    .db 6       ; scale inc
    .db 5       ; x inc
    .db 0       ; y inc
    .db 0       ; rax inc
    .db -96     ; ray inc
    .db 96      ; rbx inc
    .db -96     ; rby inc
    .db 0       ; flags

    .db 2       ; beats per step
    .db 0       ; rotation inc
    .db 6       ; scale inc
    .db 6       ; x inc
    .db 0       ; y inc
    .db -64     ; rax inc
    .db -96     ; ray inc
    .db 127     ; rbx inc
    .db -96     ; rby inc
    .db 0       ; flags

    .db 2       ; beats per step
    .db 0       ; rotation inc
    .db 0       ; scale inc
    .db 7       ; x inc
    .db 0       ; y inc
    .db -127    ; rax inc
    .db -127    ; ray inc
    .db 127     ; rbx inc
    .db -127    ; rby inc
    .db 2       ; flags

    .db 255     ; beats per step
    .db 0       ; rotation inc
    .db 0       ; scale inc
    .db 0       ; x inc
    .db 0       ; y inc
    .db 0       ; rax inc
    .db 0       ; ray inc
    .db 0       ; rbx inc
    .db 0       ; rby inc
    .db $80     ; flags
PM7SequenceEnd:

.org $3a00
EffectsSequence:

.dw Intro               ; update sub
.dw IntroInit           ; init sub    
.dw SetDummySpriteTable ; finalize sub
.dw 0

.dw RotoZoomMonoFB
.dw RotoZoomInit
.dw NullSub
.dw 0

.dw VectorBalls
.dw VectorBallsInit
.dw SetDummySpriteTable
.dw 0

.dw PseudoMode7MonoFB
.dw PseudoMode7Init
.dw SetDummySpriteTable
.dw 0

.dw Particles
.dw ParticlesInitTitan
.dw NullSub
.dw 0

.dw Particles
.dw ParticlesInitSMSPower
.dw NullSub
.dw 0

.dw Particles
.dw ParticlesInitPopsy
.dw NullSub
.dw 0

.dw Particles
.dw ParticlesInitXMen
.dw NullSub
.dw 0

.dw Particles
.dw ParticlesInitGreets
.dw NullSub
.dw 0

.dw FadeoutSloBeat
.dw NullSub
.dw NullSub
.dw 0

.dw NullSub
.dw NullSub
.dw NullSub
.dw 0

EffectsSequenceEnd:

PSGInitData:
.db $9f $bf $df $ff $81 $00 $a1 $00 $c1 $00
PSGInitDataEnd:

VDPInitData:
.db $14,$80,$00,$81,$ff,$82,$ff,$85,$ff,$86,$ff,$87,$00,$88,$00,$89,$ff,$8a
VDPInitDataEnd:

RobFlyFlamesPal:
.db 63, 47, 27, 31, 63, 47, 27

;==============================================================
; Data (LUTs)
;==============================================================

.org $3b00
SSqrLUT:
.repeat 128 index x
    .db <(x ^ 2)
.endr
.repeat 128 index x
    .db <((x - 128) ^ 2)
.endr
.repeat 128 index x
    .db >(x ^ 2)
.endr
.repeat 128 index x
    .db >((x - 128) ^ 2)
.endr
InvLUT:
.db $ff
.repeat 255 index x
    .db <(16383 / (x + 1))
.endr
SinLUT:
.dbsin 0, 255, 360 / 256, 127.999, 0
CosLUT:
.dbcos 0, 255, 360 / 256, 127.999, 0

;==============================================================
; Data (2D GFX)
;==============================================================

.bank 1 slot 1
.org $0000

RotoPalette:
.repeat 4
    .db 16
    .db 62
    .db 16
    .db 27
.endr
PM7Palette:
.repeat 4
    .db 16
    .db 63
    .db 16
    .db 43
.endr
VBPalette:
.db $00 $10 $20 $34 $21 $31 $02 $03 $13 $07 $22 $0B $1F $2F $3F BorderColor
    
SpriteData:
.repeat 64 index idx
    .db 64 + (idx & 56) << 1
.endr
.repeat 64
    .db 0
.endr
.repeat 64 index idx
    .db 128 + ((idx * 8) & 63)
    .db (idx * 2) & $3f
.endr

.define nsz (-36)
.define sz 35

VBInitX:
.dsb 4, nsz
.dsb 4, sz
.dsb 4, nsz / 2
.dsb 4, sz / 2
VBInitY:
.repeat 2
    .dsb 2, nsz
    .dsb 2, sz
.endr
.repeat 2
    .dsb 2, nsz / 2
    .dsb 2, sz / 2
.endr
VBInitZ:
.repeat 4
    .db nsz
    .db sz
.endr    
.repeat 4
    .db nsz / 2
    .db sz / 2
.endr

MonoFBTiles:
.incbin "MonoFB.bin" fsize MonoFBTilesSize

Col1Tiles:
.incbin "test_gfx/64col1 (tiles).bin" fsize Col1TilesSize
Col1TileMap:
.repeat 24 index  y
    .dsb 48, 0
    .incbin "test_gfx/64col1 (tilemap).bin" skip (y * 64 + 48) read 16
.endr
Col2Tiles:
.incbin "test_gfx/64col2 (tiles).bin" fsize Col2TilesSize
Col2TileMap:
.repeat 24 index  y
    .dsb 48, 0
    .incbin "test_gfx/64col2 (tilemap).bin" skip (y * 64 + 48) read 16
.endr
ColPalette:
.incbin "test_gfx/64col1 (palette).bin"
.dsb TilePaletteSize, BorderColor

PartData:
.incbin "vb/part (tiles).bin" fsize PartSize

PartGreets:
.incbin "test_gfx/part_greets.mono" skip 0 read 64
PartSMSPower:
.incbin "test_gfx/part_greets.mono" skip 64 read 64
PartTitan:
.incbin "test_gfx/part_greets.mono" skip 128 read 64
PartPopsy:
.incbin "test_gfx/part_greets.mono" skip 192 read 64
PartXMen:
.incbin "test_gfx/part_greets.mono" skip 256 read 64

.bank 11 slot 1
.org $2900
IntroTiles:
.incbin "test_gfx/test_sms (tiles).bin" fsize IntroTilesSize
IntroTileMap:
.incbin "test_gfx/test_sms (tilemap).bin"
IntroPalette:
.incbin "test_gfx/test_sms (palette).bin"

.bank 12 slot 1
.org $0000
CheckTiles:
.incbin "test_gfx/check (tiles).bin" fsize CheckTilesSize
CheckTileMap:
.incbin "test_gfx/check (tilemap).bin"
CheckPalette:
.incbin "test_gfx/check (palette).bin"
.dsb TilePaletteSize, BorderColor

FiftyTiles:
.incbin "test_gfx/50hz (tiles).bin" fsize FiftyTilesSize
FiftyTileMap:
.incbin "test_gfx/50hz (tilemap).bin"
FiftyPalette:
.incbin "test_gfx/50hz (palette).bin"
.dsb TilePaletteSize, BorderColor

VBTiles:
.incbin "vb/vb.sms" fsize VBTilesSize

RobFlyTiles:
.incbin "test_gfx/robot3 (tiles).bin" fsize RobFlyTilesSize
RobFlyPalette:
.incbin "test_gfx/robot3 (palette).bin"

RobFlySAT:
.incbin "test_gfx/robot3 (sat).bin"

.bank 13 slot 1
.org $0000

StarsTiles:
.incbin "test_gfx/stars_dt (tiles).bin" fsize StarsTilesSize
StarsTileMap:
.incbin "test_gfx/stars_dt (tilemap).bin"
StarsPalette:
.incbin "test_gfx/stars_dt (palette).bin"

PM7BGTiles:
.incbin "test_gfx/pm7_bg (tiles).bin" fsize PM7BGTilesSize
PM7BGTileMap:
.incbin "test_gfx/pm7_bg (tilemap).bin"

;==============================================================
; Data (MonoFB textures)
;==============================================================

.bank 0 slot 0
.org $0000
jp main
.incbin "test_gfx/check_tex.bin" skip $03 read $35
.org $0038
jp interrupt
.incbin "test_gfx/check_tex.bin" skip $3b read $2b
.org $0066
rst 0
.incbin "test_gfx/check_tex.bin" skip $67

.bank 3 slot 1
.org $0000
.incbin "test_gfx/z80_tex.bin"

.bank 14 slot 1
.org $0000
.incbin "test_gfx/road2_tex.bin"

.bank 15 slot 1
.org $0000
.incbin "test_gfx/road_tex.bin"

;==============================================================
; Data (Music)
;==============================================================

.bank 4 slot 1
.org $0000
.incbin "psg/2un_57.vgm" skip $40 read $4000
.bank 5 slot 1
.org $0000
.incbin "psg/2un_57.vgm" skip $4040
.bank 6 slot 1
.org $0000
.incbin "psg/psg.bin" read $4000
.bank 7 slot 1
.org $0000
.incbin "psg/psg.bin" skip $04000 read $4000
.bank 8 slot 1
.org $0000
.incbin "psg/psg.bin" skip $08000 read $4000
.bank 9 slot 1
.org $0000
.incbin "psg/psg.bin" skip $0c000 read $4000
.bank 10 slot 1
.org $0000
.incbin "psg/psg.bin" skip $10000
