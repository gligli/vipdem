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

;==============================================================
; Program defines
;==============================================================

.define RotoLineCount 24
.define RotoBakeSlotCount 5
.define PM7LineCount 14

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

; 256 step cosinus
; input in bc and output in a
.macro GetCosBC
    ld b, >CosLUT
    ld a, (bc)
.endm

; 256 step sinus
; input in bc and output in a
.macro GetSinBC
    ld b, >SinLUT
    ld a, (bc)
.endm
    


;==============================================================
; RAM variables
;==============================================================

.enum $c000 export
    RotoRAMBakeIncs   dsb 256 ; align 256
    RotoRAMBakeIncsEnd .
    RotoPrecalcData   dsb RotoLineCount * 2 ; align 256
    RotoPrecalcDataEnd .

    SPSave            dw
    CurFrameIdx       dw
    RotoX             dw ; (8.8 fixed point)
    RotoY             dw ; (8.8 fixed point)
    RotoRot           dw ; (8.8 fixed point)
    RotoScl           dw ; (8.8 fixed point)
    CurEffect         db

    ; keep this last
    RAMCode           .
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
    
    ; this maps the first 48K of ROM to $0000-$BFFF
    ld de, $FFFC
    ld hl, init_tab
    ld bc, $0004
    ldir

    ; set up stack

    ld sp, $dff0

    ; clear RAM

    ld hl, $c000
    ld de, $c001
    ld bc, $2000
    xor a
    ld (hl), a
    ldir

    ; set up VDP registers

    ld hl,VDPInitData
    ld b,VDPInitDataEnd-VDPInitData
    ld c,VDPControl
    otir

    ; clear VRAM

    ; 1. cet VRAM write address to $0000
    SetVDPAddress $0000 | VRAMWrite
    ; 2. output 16KB of zeroes
    ld bc, $4000     ; counter for 16KB of VRAM
-:  xor a
    out (VDPData), a ; output to VRAM address, which is auto-incremented after each write
    dec bc
    ld a, b
    or c
    jr nz, -

    ; Sprite table
    SetVDPAddress $2000 | VRAMWrite
    ld hl,SpriteData
    ld bc,256
    CopyToVDP

    ; init current frame idx
    xor a
    ld (CurFrameIdx), a
    ld (CurFrameIdx + $01), a

    ld a, 0
    ld (CurEffect), a

    ; init RotoZoom
    call RotoZoomInit

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

    in a, (VDPControl) ; ack any previous int
    ; ei

    ; anim slot
    ld a, 3
    ld (MapperSlot1), a

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

    ; RotoZoom control using D-Pad
    in a, (IOPortA)
    bit 5, a
    jp nz, +
        ld hl, CurEffect
        bit 0, (hl)
        jp z, ++
        res 0, (hl)
        call RotoZoomInit
        jp +++
    ++:
        set 0, (hl)
        call PseudoMode7Init
    +++:
+:

    bit 4, a
    jp z, ++

    ld bc, $0010
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
    jp +++

++:
    ld bc, $0200
    ld hl, (RotoY)
    rrca
    jp c, +
        or a
        sbc hl, bc
+:
    rrca
    jp c, +
        add hl, bc
+:
    ld (RotoY), hl
    ld hl, (RotoX)
    rrca
    jp c, +
        or a
        sbc hl, bc
+:
    rrca
    jp c, +
        add hl, bc
+:
    ld (RotoX), hl

+++:

p1:

    ld a, (CurEffect)
    or a
    jp nz, +
    call RotoZoomMonoFB
    jp MainLoop
+:
    call PseudoMode7MonoFB
    jp MainLoop

;==============================================================
; Utility functions
;==============================================================

; 16*8 unsigned multiplication
; The following routine multiplies de by a and places the result in ahl
; (which means a is the most significant byte of the product, l the least significant and h the intermediate one...)
MultiplyUnsignedAByDE:
    ld	c, 0
    ld	h, c
    ld	l, h

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
    ld	c, 0
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
    or a
    jr z, ++
    jp p, +
    ld c, %00101101 ; dec l
-:
    ld (hl), c
    inc hl
    inc a
    jp nz, -
    jp ++
+:  
    ld c, %00101100 ; inc l
-:
    ld (hl), c
    inc hl
    dec a
    jp nz, -
++:
    ld ixh, a

    ; y coord
    ld a, iyh
    or a
    jr z, ++
    jp p, +
    ld c, %00100101 ; dec h
-:
    ld (hl), c
    inc hl
    inc a
    jp nz, -
    jp ++
+:  
    ld c, %00100100 ; inc h
-:
    ld (hl), c
    inc hl
    dec a
    jp nz, -
++:
    ld iyh, a
    
    ; remove any leftover inc/dec
    jp +
-:
    ld (hl), a
    inc hl
+:    
    bit 7, (hl)
    jp z, -

    exx
.endm

.macro RotoZoomGetPixel args idx
.ifeq idx 0
RotoRAMCodeBakePos0:
.endif
.ifeq idx 1
RotoRAMCodeBakePos1:
.endif
.ifeq idx 3
RotoRAMCodeBakePos3:
.endif
.ifeq idx 4
RotoRAMCodeBakePos4:
.endif
.ifeq idx 7
RotoRAMCodeBakePos7:
.endif
.ifeq idx 8
RotoRAMCodeBakePos8:
.endif
    .repeat RotoBakeSlotCount
        nop
    .endr
    
    res 7, h ; texture starts at $0000

    add a, a
    or (hl)
.endm

.macro RotoGetLineOffset
    ld h, >RotoPrecalcData
    ; advance hl "line pair" items
    ld l, a
    ; even line
    ; y coord
    ld e, (hl)
    inc l
    ; x coord
    ld d, (hl)
    ; output in hl
    ex de, hl
.endm

RotoZoomInit:
    ld hl, $0000
    ld (RotoRot), hl
    ld hl, $0400
    ld (RotoScl), hl

    ; copy code to RAM, duplicating it
    ld de, RAMCode
    .repeat 16
        ld hl, RotoRAMCodeStart
        ld bc, RotoRAMCodeEnd - RotoRAMCodeStart
        ldir
    .endr
    ld bc, RotoRAMCodeRet - RotoRAMCodeEnd
    ldir

    ; get offset increments in RAM code where data will be copied in
    ld hl, RotoRAMBakeIncs
    ld d, RotoRAMCodeBakePos0 - RotoRAMCodeStart
    ld e, 0
-:
    ld (hl), d
    inc l

    ld a, e
    and $07
    cp 3
    jp nz, +
    ld d, RotoRAMCodeBakePos4 - RotoRAMCodeBakePos3 - RotoBakeSlotCount
    jp ++
+:
    cp 7
    jp nz, +
    ld d, RotoRAMCodeBakePos8 - RotoRAMCodeBakePos7 - RotoBakeSlotCount
    jp ++
+:
    ld d, RotoRAMCodeBakePos1 - RotoRAMCodeBakePos0 - RotoBakeSlotCount
++:

    inc e
    jp nz, -

    ret

RotoZoomMonoFB:
    ex de, hl

    ; precaclulate increments for one line

    exx
    ld de, (RotoScl) ; vy

    ld bc, (RotoRot) ; vx
    GetCosBC
    call FPMultiplySignedAByDE    
    push hl
    
    ld bc, (RotoRot) ; vx
    GetSinBC
    call FPMultiplySignedAByDE    
    ld d, h
    ld e, l
    
    pop bc
   
    ; ld ($d800), bc
    ; ld ($d802), de
   
    ld ix, (RotoX) ; x
    ld iy, (RotoY) ; y
    exx

    ld (SPSave), sp

    ld sp, RotoPrecalcDataEnd
    exx
    NegateDE
    .repeat RotoLineCount
        RotoZoomPushIncs
        RotoZoomX 2, 1
        RotoZoomY 2, 1
    .endr
    NegateDE
    exx

    ; copy line pixels increments into RAM code

    ld sp, (SPSave)

    ld de, RotoRAMBakeIncs
    ld hl, RAMCode

    ld ix, (RotoX) ; x
    ld iy, (RotoY) ; y

    ld b, 0
    
    exx
    ld h, d
    ld l, e
    NegateHL
    exx
    
RotoPrecalcIncs:
    exx
    .repeat 1
        RotoZoomX 1, 0
        RotoZoomY 1, 0
        RotoZoomBakeIncs

        ex de, hl ; NegateDE
        RotoZoomX 1, 1
        ex de, hl ; NegateDE
        RotoZoomY 1, 1
        RotoZoomBakeIncs

        RotoZoomX 1, 0
        RotoZoomY 1, 0
        RotoZoomBakeIncs
        
        RotoZoomX 1, 1
        NegateBC
        RotoZoomY 1, 1
        NegateBC
        RotoZoomBakeIncs
    .endr
    exx

    xor a
    cp e
    jp nz, RotoPrecalcIncs
    
;    ret
RotoPrecalcEnd:

    ld sp, (SPSave)
    ld l, RotoLineCount ; line counter
    exx
    ld c, VDPData
    exx

    ; main loop on lines pairs

RotoLineLoop:
    ld a, l
    dec a
    add a, a
    exx
    RotoGetLineOffset

    jp RAMCode
RotoRAMCodeStart:
    .repeat 2 index x_dummy
        .repeat 4 index x_bit
            RotoZoomGetPixel x_dummy * 8 + x_bit
        .endr
        in (c)
        .repeat 4 index x_bit
            RotoZoomGetPixel x_dummy * 8 + x_bit + 4
        .endr
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

.macro PM7GetPixel args idx
    .ifeq (idx & 1) 0
        exx
        RotoZoomX 1, 0
        RotoZoomY 1, 0
        exx
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

    res 7, h ; texture starts at $0000

    add a, a
    or (hl)
.endm

.macro PM7GetLineOffsets
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
.endm

PseudoMode7Init:
    ld hl, $0000
    ld (RotoRot), hl
    ld hl, $0200
    ld (RotoScl), hl

    ret

PseudoMode7MonoFB:
    ex de, hl

    ; precaclulate increments for one line

    exx
    ld ix, (RotoX) ; x
    ld iy, (RotoY) ; y
    ld bc, (RotoRot) ; vx
    ld de, (RotoScl) ; vy
    ld hl, (RotoScl) ; -vy
    NegateHL
    exx

PM7DoPrecalc:
    ld (SPSave), sp
    ld sp, RotoPrecalcData + PM7LineCount * 4

    xor a
    exx
    ex de, hl ; vy = - vy

    .repeat PM7LineCount * 2 index y_line
        ; x coord
        ld a, ixh
        ld l, a

        ; y coord
        ld a, PM7LineCount * 0
        add a, iyh
        ld h, a

        push hl

        RotoZoomX 1, 1
        RotoZoomY 1, 1
    .endr

    ; hl thrashed by PM7PushIncs
    ld hl, (RotoScl) ; vy
    ex de, hl ; vy = - vy
    exx

    ; line increments of vx

    ld hl, (RotoScl)
    ld sp, RotoRAMBakeIncs + PM7LineCount * 2 ; reusing this buffer for line incs
    ld bc, PM7LineCount
-:
    add hl, bc
    push hl
    inc c
    ld a, c
    cp PM7LineCount * 2
    jp nz, -

PM7PrecalcEnd:

    ld sp, (SPSave)
    exx
    ld l, PM7LineCount ; line counter
    exx

    ld c, VDPData

    ; main loop on lines pairs

PM7LineLoop:
    ld ix, (RotoX) ; x
    ld iy, (RotoY) ; y

    exx
    ; get line inc
    ld a, l
    neg
    add a, a
    add a, <(RotoRAMBakeIncs + PM7LineCount * 2)
    ld d, >RotoRAMBakeIncs
    ld e, a
    ld a, (de)
    ld h, a
    inc e
    ld a, (de)
    ld d, a
    ld e, h

    ; for line offsets
    ld a, l
    dec a
    add a, a
    add a, a
    exx

    PM7GetLineOffsets

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
    .endr

    exx
    dec l
    exx
    jp nz, PM7LineLoop

    ret

;==============================================================
; Data
;==============================================================

.bank 2 slot 2
.org $3e00
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
.db $14,$80,$00,$81,$ff,$82,$41,$85,$fb,$86,$ff,$87,$00,$88,$00,$89,$ff,$8a
VDPInitDataEnd:

LocalPalette:
.repeat 4
    .db 1
    .db 27
    .db 16
    .db 47
.endr
.repeat 16
    .db 16
.endr

SpriteData:
.repeat 64 index idx
    .db 192 + (idx & 48)
.endr
.repeat 64
    .db 0
.endr
.repeat 64 index idx
    .db 128 + ((idx * 8) & 127)
    .db idx
.endr

MonoFBData:
.incbin "MonoFB.bin" fsize MonoFBSize

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
