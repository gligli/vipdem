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
bankstotal 4
banksize $4000
banks 4
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
.define BankSize_ 16384
.define Width 256
.define Height 192
.define HalfWidth (Width / 2)
.define HalfHeight (Height / 2)

;==============================================================
; Program defines
;==============================================================

.define RotoColumnCount 24
.define RotoLineCount 24

.define PM7LineCount 12
.define PM7Fov 0.75

.define VBCount 16
.define VBOriginX HalfWidth
.define VBOriginY HalfHeight
.define VBOriginZ (-64)

;==============================================================
; Utility macros
;==============================================================

.macro WaitVBlank args playSmp ; c11
    in a, (VDPControl)

---:
    in a, (VDPControl)
    .ifeq playSmp 1
        .repeat 10
            inc iy
        .endr
        PlaySampleSkew 125
    .endif
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
; parameters: hl = data address, bc = data length
; affects: a, hl, bc
-:  ld a,(hl)    ; get data byte
    out (VDPData),a
    inc hl       ; point to next letter
    dec bc
    ld a,b
    or c
    jr nz,-
.endm

.macro PlaySample
    exx
    outd
    exx
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

.macro memcpy args dest, src, len
    ld de, dest
    ld hl, src
    ld bc, len
    ldir
.endm

.macro memset args dest, val, len
    ld hl, dest
    ld de, dest + 1
    ld bc, len
    ld a, val
    ld (hl), a
    ldir
.endm

;==============================================================
; RAM variables
;==============================================================

; Global variables
.enum $c000 export
    CurFrameIdx       dw
    SPSave            dw
    CurEffect         db
    LocalPalette      dsb 32
.ende

; RotoZoom / PseudoMode7
.enum $c100 export
    RotoRAMBakeIncs   dsb 256 ; align 256
    RotoRAMBakeIncsEnd .
    RotoPrecalcData   dsb 256 ; align 256

    RotoRot           dw ; (8.8 fixed point)
    RotoScl           dw ; (8.8 fixed point)
    RotoX             dw ; (8.8 fixed point)
    RotoY             dw ; (8.8 fixed point)
    RotoVX            dw ; (8.8 fixed point)
    RotoVY            dw ; (8.8 fixed point)
    PM7CurLine        db
    
    ; keep this last
    RAMCode           .
.ende

; VectorBalls
.enum $c100 export
    VBX               dsb 256
    VBY               dsb 256
    VBZ               dsb 256
    VBSAT             dsb 256
    VBRX              db
    VBRY              db
    VBRZ              db
    VBSinCos          dsb 6
    VBFactors         dsb 9
.ende

;==============================================================
; Init code
;==============================================================

.bank 2 slot 2
.org $0000
interrupt:
    in a, (VDPControl) ;ack vdp int
    ei
    ret

init_tab: ; table must exist within this bank
    .db $00, $00, $01, $02

main:
    di              ; disable interrupts
    im 1            ; interrupt mode 1
    
    ; set up stack

    ld sp, $dff0

    ; clear RAM

    memset $c000, 0, $2000

Reinit:

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

    ; dummy Sprite table
    SetVDPAddress $2800 | VRAMWrite
    ld a, $d0
    out (VDPData), a

    ; load tiles (Monochrome framebuffer emulation)
    SetVDPAddress $0000 | VRAMWrite
    ld hl,MonoFBData
    ld bc,MonoFBSize
    CopyToVDP

    ; init PSG
    ld hl,PSGInitData
    ld bc,(PSGInitDataEnd-PSGInitData)<<8 + PSGPort
    otir

    memcpy LocalPalette, RotoPalette, 32
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

    ; Effect init
    
    call CurEffectInit

;==============================================================
; Main loop
;==============================================================

MainLoop:
    WaitVBlank 0
p0:

    ; frame index advance
    ld a, (CurFrameIdx)
    inc a
    ld (CurFrameIdx), a

    ; VDP tilemap pointer (double buffering)
    and 1
    ld a, $ff
    jr z, +
    ld a, $fd
+:
    out (VDPControl), a
    ld a, $82
    out (VDPControl), a

    ; Effect switcher
    
    in a, (IOPortA)
    ld c, a
    bit 5, c
    jp nz, +
        ld a, (CurEffect)
        inc a
        cp (EffectsEnd-Effects) / 4
        jp nz, ++
        xor a
    ++:
        ld (CurEffect), a
        jp Reinit
        
+:
    ld a, (CurEffect)
    or a
    jp z, ++

    ; Roto / PM7 controls

    ld ixl, c
    bit 4, c
    jp nz, +
    RotoZoomFromRotScale 0, 1
    ld hl, (RotoX)
    or a
    sbc hl, de
    sbc hl, de
    ld (RotoX), hl
    ld hl, (RotoY)
    add hl, bc
    add hl, bc
    ld (RotoY), hl
+:    
    ld a, ixl

    ld bc, $0020
    ld hl, (RotoScl)
    rrca
    jp c, +
        or a
        sbc hl, bc
+:
    rrca
    jp c, +
        add hl, bc
+:
    ld (RotoScl), hl
    ld bc, $0002
    ld hl, (RotoRot)
    rrca
    jp c, +
        or a
        sbc hl, bc
+:
    rrca
    jp c, +
        add hl, bc
+:
    ld (RotoRot), hl

    jp p1
   
++:    
    ; VB controls

    ld a, c
    bit 4, c
    jp nz, +
    ld hl, VBRZ
    inc (hl)
+:    
    ld hl, VBRY
    rrca
    jp c, +
    dec (hl)
+:   
    rrca
    jp c, +
    inc (hl)
+:   
    ld hl, VBRX
    rrca
    jp c, +
    dec (hl)
+:   
    rrca
    jp c, +
    inc (hl)
+:   
    
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
    
CurEffectInit:
    ld h, >Effects
    ld a, (CurEffect)
    add a, a
    add a, a
    ld l, a
    ld e, (hl)
    inc l
    ld d, (hl)
    ex de, hl
    jp (hl)
    
CurEffectUpdate:
    ld h, >Effects
    ld a, (CurEffect)
    add a, a
    inc a
    add a, a
    ld l, a
    ld e, (hl)
    inc l
    ld d, (hl)
    ex de, hl
    jp (hl)

UploadPalette:
    ld c, VDPData
    SetVDPAddress $0000 | CRAMWrite
    ld hl, LocalPalette
    .repeat 32
        outi
    .endr
    
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
    call FPMultiplySignedBByC
    ld a, h
    ; y
    VBGetCFromDE
    ld b, iyl
    ex af, af'
    call FPMultiplySignedBByC
    ex af, af'
    add a, h
    ; z
    VBGetCFromDE
    ld b, iyh
    ex af, af'
    call FPMultiplySignedBByC
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
    ; init X
    memcpy VBX, VBInitX, VBCount

    ; init Y
    memcpy VBY, VBInitY, VBCount
    
    ; init Z
    memcpy VBZ, VBInitZ, VBCount

    ; load tiles (VectorBalls)
    SetVDPAddress $2000 | VRAMWrite
    ld hl,VBData
    ld bc,VBSize
    CopyToVDP

    ; SAT address ($3f00)
    ld a, $7f
    out (VDPControl), a
    ld a, $85
    out (VDPControl), a
    
    ; SAT terminator
    ld hl, VBSAT + VBCount
    ld a, $d0
    ld (hl), a
    
    xor a
    ld (VBRX), a
    ld (VBRY), a
    ld (VBRZ), a
    
    ret

VectorBalls:
    ; upload current sat to VDP
    
    SetVDPAddress $3f00 | VRAMWrite
    ld hl, VBSAT
    ld c, VDPData
    .repeat 256
        outi
    .endr

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
    ld d, 192
    cp d
    jr c, +
    ld a, d
+:    
    
    ex de, hl
    ld e, ixh
    ; sla e
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
    ld c, a

    ; stretch effect
    ld a, (CurFrameIdx)
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
    and $62
    
++:
    add a, c
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

RotoZoomInit:
    ; texture slot
    ld a, 3
    ld (MapperSlot1), a

    ld hl, $0000
    ld (RotoRot), hl
    ld hl, $02b0
    ld (RotoScl), hl

    ld a, $00
    out (VDPControl), a
    ld a, $88
    out (VDPControl), a

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

    ret

RotoZoomMonoFB:
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

PseudoMode7Init:
    ; texture slot
    ld a, 3
    ld (MapperSlot1), a

    ld hl, $0140
    ld (RotoRot), hl
    ld hl, $0400
    ld (RotoScl), hl
    
    ld a, $80
    out (VDPControl), a
    ld a, $88
    out (VDPControl), a
    
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

    ; main loop on lines pairs

PM7LineLoop:
    ld ix, 0 ; x
    ld iy, 0 ; y
    
    exx
    ld l, (hl)
    ld h, 0

    ; VRAM pointer position
    push hl
    dec l
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
    add a, >VRAMWrite + $36 - (PM7LineCount >> 2)
    out (VDPControl), a
    pop hl
    
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
        .repeat 4 index x_bit
            PM7GetPixel x_byte * 8 + x_bit
        .endr
        ex af, af'
        in a, (VDPData)
        ex af, af'
        .repeat 4 index x_bit
            PM7GetPixel x_byte * 8 + x_bit + 4
        .endr
        out (VDPData), a
        
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

    ret

;==============================================================
; Data
;==============================================================

.bank 2 slot 2

.org $3a00
Effects:
.dw VectorBallsInit
.dw VectorBalls
.dw RotoZoomInit
.dw RotoZoomMonoFB
.dw PseudoMode7Init
.dw PseudoMode7MonoFB
EffectsEnd:

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

.bank 1 slot 1
.org $0000

PSGInitData:
.db $9f $bf $df $ff $81 $00 $a1 $00 $c1 $00
PSGInitDataEnd:

VDPInitData:
.db $14,$80,$00,$81,$ff,$82,$51,$85,$ff,$86,$ff,$87,$00,$88,$00,$89,$ff,$8a
VDPInitDataEnd:

RotoPalette:
.repeat 4
    .db 1
    .db 27
    .db 16
    .db 47
.endr
VBPalette:
.db $00 $10 $20 $34 $21 $31 $02 $03 $13 $07 $22 $0B $1F $2F $3F
    
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

MonoFBData:
.incbin "MonoFB.bin" fsize MonoFBSize

VBData:
.incbin "vb/vb.sms" fsize VBSize

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

.bank 0 slot 0
.org $0000
jp main
.incbin "anim_128_1/fixed.bin" skip $03 read $35
.org $0038
jp interrupt
.incbin "anim_128_1/fixed.bin" skip $3b read $2b
.org $0066
rst 0
.incbin "anim_128_1/fixed.bin" skip $67 read $3f99
.bank 3 slot 1
.org $0000
.incbin "anim_128_1/fixed.bin" skip $4000 read $4000
