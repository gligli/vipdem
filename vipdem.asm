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

.define VBCount 8
.define VBOriginX HalfWidth
.define VBOriginY HalfHeight
.define VBOriginZ 96

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

;==============================================================
; RAM variables
;==============================================================

; Global variables
.enum $c000 export
    CurFrameIdx       dw
    SPSave            dw
    CurEffect         db
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
    VBSinCos          dsb 6
    VBFactors         dsb 9
    VBRX              db
    VBRY              db
    VBRZ              db
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

    ld hl, $c000
    ld de, $c001
    ld bc, $2000
    xor a
    ld (hl), a
    ldir

    ; init current frame idx
    xor a
    ld (CurFrameIdx), a
    ld (CurFrameIdx + $01), a

    ; default effect
    ld a, 2
    ld (CurEffect), a

Reinit:

    ; this maps the first 48K of ROM to $0000-$BFFF
    ld de, $FFFC
    ld hl, init_tab
    ld bc, $0004
    ldir
   
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

    ld c, VDPData
    SetVDPAddress $0000 | CRAMWrite
    ld hl, LocalPalette
    .repeat 32
        outi
        inc iy
    .endr

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
    
    ld a, (CurEffect)
    or a
    jp nz, +
    call RotoZoomInit
    jp ++
+:
    dec a
    jp nz, +
    call PseudoMode7Init
    jp ++
+:
    call VectorBallsInit
++:    

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

    ; VRAM tilemap address
    and 1
    jp z, +
    ld a, $fd
    out (VDPControl), a
    ld a, $82
    out (VDPControl), a
    SetVDPAddress $37ff | VRAMWrite
    jp ++
+:
    ld a, $ff
    out (VDPControl), a
    ld a, $82
    out (VDPControl), a
    SetVDPAddress $2fff | VRAMWrite
++:

    ; Effect switcher
    
    in a, (IOPortA)
    ld c, a
    bit 5, c
    jp nz, +
        ld a, (CurEffect)
        inc a
        cp 3
        jp nz, ++
        xor a
    ++:
        ld (CurEffect), a
        jp Reinit
        
+:
    ld a, (CurEffect)
    cp 2
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

    ld a, (CurEffect)
    or a
    jp nz, +
    call RotoZoomMonoFB
    jp MainLoop
+:
    dec a
    jp nz, +
    call PseudoMode7MonoFB
    jp MainLoop
+:
    call VectorBalls
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

    ld  h, >SMulLUT
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
    jp  pe, @Plus     ; jump if no overflow

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
    push af
    call FPMultiplySignedBByC
    pop af
    ld a, h
    ; y
    VBGetCFromDE
    ld b, iyl
    push af
    call FPMultiplySignedBByC
    pop af
    add a, h
    ; z
    VBGetCFromDE
    ld b, iyh
    push af
    call FPMultiplySignedBByC
    pop af
    add a, h
.endm

VectorBallsInit:
    ; clear XYZ
    ld hl, $c100
    ld de, $c101
    ld bc, $0300
    xor a
    ld (hl), a
    ldir

    ; init X
    ld hl, VBInitX
    ld de, VBX
    ld bc, VBCount
    ldir

    ; init Y
    ld hl, VBInitY
    ld de, VBY
    ld bc, VBCount
    ldir
    
    ; init Z
    ld hl, VBInitZ
    ld de, VBZ
    ld bc, VBCount
    ldir

    ; load tiles (VectorBalls)
    SetVDPAddress $2000 | VRAMWrite
    ld hl,VBData
    ld bc,VBSize
    CopyToVDP

    ; SAT address ($2800)
    ld a, $51
    out (VDPControl), a
    ld a, $85
    out (VDPControl), a
    
    ; SAT terminator
    ld hl, VBSAT + VBCount * 2
    ld a, $d0
    ld (hl), a
    
    xor a
    ld (VBRX), a
    ld (VBRY), a
    ld (VBRZ), a
    
    ret

VectorBalls:
    ; upload current sat to VDP
    
    SetVDPAddress $2800 | VRAMWrite
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
        sra a
        ld (de), a
        inc e
        GetCosC
        sra a
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
    ld b, a
    pop af
    ld c, a
    
    ; project & build SAT

    ld h, >InvLUT
    ld d, (hl) ; 1 / z
    inc h
    ld e, (hl)
    
    ld a, c
    call FPMultiplySignedAByDE
    add hl, hl

    ld a, b ; py
    ld b, h ; px
    call FPMultiplySignedAByDE
    add hl, hl

    ; add x / y 2d origin
    
    ld a, b
    add a, VBOriginX
    ld c, a
    ld a, h ; py
    add a, VBOriginY

    ; don't let sprites y become $d0 (terminator)
    ld b, 192
    cp b
    jr c, +
    ld a, b
+:    
    
    ex de, hl
    ld e, ixh
    sla e
    ld d, >VBSAT
    ex de, hl
    
    ; sat y
    ld (hl), a
    inc l
    ld (hl), a
    dec l

    ; sat second part
    sla l
    set 7, l

    ; sat x
    ld (hl), c
    inc l
    inc l
    ld a, c
    add a, 8
    ld (hl), a
    dec l

    ; sat index (z)
    ld a, d
    rrca
    rrca
    ld d, $3c
    and d
    ; avoid fully transparent version
    cp d
    jr nz, +
    sub 4
+:    
    ld (hl), a
    inc l
    inc l
    add a, 2
    ld (hl), a

    inc ixh
    ld a, ixh
    cp VBCount
    jp nz, VBLoop

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
SMulLUT:
.db $00, $01, $04, $09, $10, $19, $24, $31, $40, $51, $64, $79, $90, $A9, $C4, $E1
.db $00, $21, $44, $69, $90, $B9, $E4, $11, $40, $71, $A4, $D9, $10, $49, $84, $C1
.db $00, $41, $84, $C9, $10, $59, $A4, $F1, $40, $91, $E4, $39, $90, $E9, $44, $A1
.db $00, $61, $C4, $29, $90, $F9, $64, $D1, $40, $B1, $24, $99, $10, $89, $04, $81
.db $00, $81, $04, $89, $10, $99, $24, $B1, $40, $D1, $64, $F9, $90, $29, $C4, $61
.db $00, $A1, $44, $E9, $90, $39, $E4, $91, $40, $F1, $A4, $59, $10, $C9, $84, $41
.db $00, $C1, $84, $49, $10, $D9, $A4, $71, $40, $11, $E4, $B9, $90, $69, $44, $21
.db $00, $E1, $C4, $A9, $90, $79, $64, $51, $40, $31, $24, $19, $10, $09, $04, $01
.db $00, $01, $04, $09, $10, $19, $24, $31, $40, $51, $64, $79, $90, $A9, $C4, $E1
.db $00, $21, $44, $69, $90, $B9, $E4, $11, $40, $71, $A4, $D9, $10, $49, $84, $C1
.db $00, $41, $84, $C9, $10, $59, $A4, $F1, $40, $91, $E4, $39, $90, $E9, $44, $A1
.db $00, $61, $C4, $29, $90, $F9, $64, $D1, $40, $B1, $24, $99, $10, $89, $04, $81
.db $00, $81, $04, $89, $10, $99, $24, $B1, $40, $D1, $64, $F9, $90, $29, $C4, $61
.db $00, $A1, $44, $E9, $90, $39, $E4, $91, $40, $F1, $A4, $59, $10, $C9, $84, $41
.db $00, $C1, $84, $49, $10, $D9, $A4, $71, $40, $11, $E4, $B9, $90, $69, $44, $21
.db $00, $E1, $C4, $A9, $90, $79, $64, $51, $40, $31, $24, $19, $10, $09, $04, $01
.db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $01, $01, $01, $01, $01, $01, $01, $02, $02, $02, $02, $02, $03, $03, $03, $03
.db $04, $04, $04, $04, $05, $05, $05, $05, $06, $06, $06, $07, $07, $07, $08, $08
.db $09, $09, $09, $0A, $0A, $0A, $0B, $0B, $0C, $0C, $0D, $0D, $0E, $0E, $0F, $0F
.db $10, $10, $11, $11, $12, $12, $13, $13, $14, $14, $15, $15, $16, $17, $17, $18
.db $19, $19, $1A, $1A, $1B, $1C, $1C, $1D, $1E, $1E, $1F, $20, $21, $21, $22, $23
.db $24, $24, $25, $26, $27, $27, $28, $29, $2A, $2B, $2B, $2C, $2D, $2E, $2F, $30
.db $31, $31, $32, $33, $34, $35, $36, $37, $38, $39, $3A, $3B, $3C, $3D, $3E, $3F
.db $40, $3F, $3E, $3D, $3C, $3B, $3A, $39, $38, $37, $36, $35, $34, $33, $32, $31
.db $31, $30, $2F, $2E, $2D, $2C, $2B, $2B, $2A, $29, $28, $27, $27, $26, $25, $24
.db $24, $23, $22, $21, $21, $20, $1F, $1E, $1E, $1D, $1C, $1C, $1B, $1A, $1A, $19
.db $19, $18, $17, $17, $16, $15, $15, $14, $14, $13, $13, $12, $12, $11, $11, $10
.db $10, $0F, $0F, $0E, $0E, $0D, $0D, $0C, $0C, $0B, $0B, $0A, $0A, $0A, $09, $09
.db $09, $08, $08, $07, $07, $07, $06, $06, $06, $05, $05, $05, $05, $04, $04, $04
.db $04, $03, $03, $03, $03, $02, $02, $02, $02, $02, $01, $01, $01, $01, $01, $01
.db $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
InvLUT:
.repeat 256 index x
    .ifeq x 0
        .db 0
    .else
        .db <(32767 / x)
    .endif
.endr
.repeat 256 index x
    .ifeq x 0
        .db 0
    .else
        .db >(32767 / x)
    .endif
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

LocalPalette:
.repeat 4
    .db 1
    .db 27
    .db 16
    .db 47
.endr
.db 63, 63, 47, 31,
.db 11,  7,  3, 19,
.db  2, 34, 33, 49,
.db 52, 48, 32, 16,
    
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
.incbin "vb.sms" fsize VBSize

VBInitX:
.dsb 4, -64
.dsb 4, 63
VBInitY:
.repeat 2
    .dsb 2, -64
    .dsb 2, 63
.endr
VBInitZ:
.repeat 4
    .db -64
    .db 63
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
