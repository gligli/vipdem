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

.define FrameSampleCount 825 ; 344 cycles per PCM sample = one sample every 320 cycles
.define PCMBufferSizeShift 11
.define PCMBufferSize (1 << PCMBufferSizeShift)

;==============================================================
; Useful macros
;==============================================================

.macro WaitVBlank args playSmp ; c11
    in a, (VDPControl)

---:
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

;==============================================================
; RAM variables
;==============================================================

.enum $c000 export
    RotoPrecalcData   dsb 96 ; align 256
    RotoPrecalcDataEnd .
    RotoRAMBakeAlign  dsb 32
    RotoRAMBakeIncs   dsb 128 ; align 128
    RotoRAMBakeIncsEnd .
    MonoFB            dsb 1600
    MonoFBEnd          .

    SPSave            dw
    CurFrameIdx       dw
    RotoVX            dw ; (8.8 fixed point)
    RotoVY            dw ; (8.8 fixed point)
    DoAnim            db

    ; keep this last
    RAMCode           .
.ende

;==============================================================
; Init code
;==============================================================

.bank 2 slot 2
.org $0000
interrupt:
    ex af, af'
    in a, (VDPControl) ;ack vdp int

    jr nc, +

    ; change tilemap ($3000)
    ld a, $fd
    out (VDPControl), a
    ld a, $82
    out (VDPControl), a

    or a ; clear carry

    ex af, af'
    ei
    ret

+:
    ; change tilemap ($3800)
    ld a, $ff
    out (VDPControl), a
    ld a, $82
    out (VDPControl), a

    scf

    ex af, af'
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
    ld b, $00
-:
    ld (hl), b
    inc hl
    ld a,h
    sub $e0
    or l
    jr nz,-

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

    ; init line int
    in a, (VDPControl) ; ack any previous int
    ex af, af'
    scf
    ex af, af'
    ld a, 3 ; every 4 lines
    out (VDPControl), a
    ld a, $8a
    out (VDPControl), a

    ; turn screen on
    ld a, %1000010
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

    ei

    ; anim slot
    ld a, 3
    ld (MapperSlot1), a

;==============================================================
; Main loop
;==============================================================

MainLoop:
    in a, (VDPScanline)
    cp 192
    jp c, +
-:
    in a, (VDPScanline)
    or a
    jp nz, -
+:
-:    
    in a, (VDPScanline)
    cp 192
    jp c, -
p0:

    ; frame index advance
    ld a, (CurFrameIdx)
    inc a
    ld (CurFrameIdx), a
    ld d, a

    and 1
    jp z, +
    SetVDPAddress $3800 | VRAMWrite
    jp ++
+:
    SetVDPAddress $3000 | VRAMWrite
++:
    xor a
    ld hl, MonoFBEnd - 1
    ld c, VDPData
    .repeat TileMapSize / 2
        outd
        ind
    .endr

    ; RotoZoom control using D-Pad
    in a, (IOPortA)
    ld bc, $0004
    bit 4, a
    jp nz, +
        ld bc, $0010
+:
    bit 5, a
    jp nz, +
        push af
        ld a, (DoAnim)
        cpl
        or 1
        ld (DoAnim), a
        pop af
+:
    bit 0, a
    jp nz, +
        ld hl, (RotoVY)
        or a
        sbc hl, bc
        ld (RotoVY), hl
+:
    bit 1, a
    jp nz, +
        ld hl, (RotoVY)
        add hl, bc
        ld (RotoVY), hl
+:
    bit 2, a
    jp nz, +
        ld hl, (RotoVX)
        or a
        sbc hl, bc
        ld (RotoVX), hl
+:
    bit 3, a
    jp nz, +
        ld hl, (RotoVX)
        add hl, bc
        ld (RotoVX), hl
+:

p1:

    ; interlace indicator
    ld h, d

    call RotoZoomMonoFB
p2:
    jp MainLoop

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
.ifeq idx 3
RotoRAMCodeBakePos3:
.endif
.ifeq idx 4
RotoRAMCodeBakePos4:
.endif
    ld bc, $aa55 ; placeholder value (x/y coordinates will be copied in)

    add hl, bc ; add line x/y offset
    res 7, h ; texture starts at $0000

    add a, a
    or (hl)
    
    ex de, hl
    
    add hl, bc ; add line x/y offset
    res 7, h ; texture starts at $0000

    add a, a
    or (hl)
    
    ex de, hl
.endm

RotoZoomInit:
    ld hl, $0200
    ld (RotoVX), hl
    ld hl, $0000
    ld (RotoVY), hl
    ld a, 1
    ld (DoAnim), a

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
    ld d, RotoRAMCodeBakePos0 - RotoRAMCodeStart + 1
    ld e, 128
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
    ld d, RotoRAMCodeBakePos1 - RotoRAMCodeBakePos0 - 1
++:

    inc e
    jp nz, -

    ret

RotoZoomMonoFB:
    ex de, hl

    ; precaclulate increments for one line

    exx
    ld ix, 0 ; x
    ld iy, 0 ; y
    ld bc, (RotoVX) ; vx
    ld de, (RotoVY) ; vy
    ld hl, (RotoVY) ; -vy
    NegateHL
    exx

    ld a, d
    ld de, 0
    and 1
    jp z, RotoDoPrecalc

    exx
    ex de, hl ; vy = - vy
    RotoZoomX 2, 1
    ex de, hl ; vy = - vy
    RotoZoomY 2, 1
    exx

RotoDoPrecalc:
    ld (SPSave), sp
    ld sp, RotoPrecalcDataEnd

    ld c, 24
    exx
    ex de, hl ; vy = - vy
    exx
-:
    exx
    RotoZoomPushIncs
    RotoZoomX 1, 1
    RotoZoomY 1, 1

    RotoZoomPushIncs
    RotoZoomX 3, 1
    RotoZoomY 3, 1
    exx

    dec c
    jp nz, -

    exx
    ; hl thrashed by RotoZoomPushIncs
    ld hl, (RotoVY) ; vy
    ex de, hl ; vy = - vy
    exx

    ; copy line pixels offsets into RAM code

    ld sp, (SPSave)

    ld de, RotoRAMBakeIncs
    ld hl, RAMCode

    ld ix, 0 ; x
    ld iy, 0 ; y

    ld b, 0
-:
    exx
    .repeat 8
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

    ld sp, MonoFBEnd
    ld l, 24 ; line counter

    ; main loop on lines pairs

RotoLineLoop:
    ld a, l
    dec a
    add a, a
    add a, a
    exx
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
    
    jp RAMCode
RotoRAMCodeStart:
    .repeat 2 index x_dummy
        .repeat 4 index x_bit
            RotoZoomGetPixel x_dummy * 4 + x_bit
        .endr
        push af
    .endr
RotoRAMCodeEnd:
    jp RotoRAMCodeRet
RotoRAMCodeRet:

    exx

    dec l
    jp nz, RotoLineLoop

    ld sp, (SPSave)

    ret

;==============================================================
; Data
;==============================================================

.bank 1 slot 1
.org $0000

PSGInitData:
.db $9f $bf $df $ff $81 $00 $a1 $00 $c1 $00
PSGInitDataEnd:

; VDP initialisation data
VDPInitData:
.db $14,$80,$00,$81,$ff,$82,$41,$85,$fb,$86,$ff,$87,$00,$88,$00,$89,$ff,$8a
VDPInitDataEnd:

LocalPalette:
.repeat 2
    .db 32
    .db 47
    .db 16
    .db 16
    .db 32
    .db 63
    .db 16
    .db 16
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
