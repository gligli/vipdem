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
bankstotal 8
banksize $4000
banks 8
.endro

;==============================================================
; SDSC tag and SMS rom header
;==============================================================

.sdsctag 0.0,"VIP'17 demo","VIP'17 demo","GliGli"

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

.define DblBufTileOffset 49 * TileSize
.define FrameSampleCount 825 ; 344 cycles per PCM sample = one sample every 320 cycles
.define PCMBufferSizeShift 11
.define PCMBufferSize (1 << PCMBufferSizeShift)

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
        ; Sets the VDP address
    ld a, <addr
    out (VDPControl),a
    ld a, >addr
    out (VDPControl),a
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
    MonoFB            dsb 768
    CurFrameIdx       dw
    RotoVX            dw ; (8.8 fixed point)
    RotoVY            dw ; (8.8 fixed point)
    DoAnim            db
.ende

;==============================================================
; Code
;==============================================================

.bank 0 slot 0
.org $0000
    di              ; disable interrupts
    im 1            ; Interrupt mode 1
    ; This maps the first 48K of ROM to $0000-$BFFF
    ld de, $FFFC
    ld hl, init_tab
    ld bc, $0004
    ldir

    jp main

init_tab: ; table must exist within first 1K of ROM
    .db $00, $00, $01, $02

.org $0038
    ex af, af'
    in a, (VDPControl)
    ei

    in a, (VDPScanline)

    and $04
    or $fb

    .repeat 5
        inc iy
        dec iy
    .endr

    out (VDPControl), a
    ld a, $82
    out (VDPControl), a

    ex af, af'
    ret

.org $0066
    jp 0

main:
    ld sp, $dff0

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
    call CopyToVDP

    ; init current frame idx
    xor a
    ld (CurFrameIdx), a
    ld (CurFrameIdx + $01), a

    ; Init RotoZoom
    ld hl, $0100
    ld (RotoVX), hl
    ld hl, $0000
    ld (RotoVY), hl
    ld a, 1
    ld (DoAnim), a
    
    ; Load tiles (Monochrome framebuffer emulation)
    SetVDPAddress $0000 | VRAMWrite
    ld hl,MonoFBData
    ld bc,MonoFBSize
    call CopyToVDP

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

;     ld a, $04
;     ld (MapperSlot1), a
;     ld a, $05
;     ld (MapperSlot2), a
; 
;     ; Output tilemap data
;     SetVDPAddress $2800 | VRAMWrite
;     ld hl, AnimData + $20
;     call UploadMonoFB
; 
;     SetVDPAddress $3800 | VRAMWrite
;     ld hl, AnimData
;     call UploadMonoFB

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

    in a, (VDPControl) ; ack any previous int
    ei

MainLoop:
    in a, (VDPScanline)
    cp 192
    jp c, MainLoop
p0:

    ld a, (CurFrameIdx)
    inc a
    ld (CurFrameIdx), a
    
    ld d, a
    ld a, (DoAnim)
    and d
    ld d, a
    
    and 1
    jp z, +
    SetVDPAddress $3800 | VRAMWrite
    jp ++
+:
    SetVDPAddress $2800 | VRAMWrite
++:

    xor a
    ld hl, MonoFB
    ld c, VDPData
    .repeat 768
        outi
        out (VDPData), a
    .endr   

    ; temporal de-dither using X scroll
;     ld a, d
;     and 1
;     rlca
;     out (VDPControl), a
;     ld a, $88
;     out (VDPControl), a

    ; RotoZoom control using D-Pad
    in a, (IOPortA)
    ld bc, $0010
    bit 4, a
    jp nz, +
        ld bc, $0100
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

    ; anim slot
    ld a, d
    and $30
    .repeat 4
        rrca
    .endr
    or $04
    ld (MapperSlot1), a

    ; anim source pointer
    ld hl, AnimData
    ld a, d
    and $01
    add a, h
    ld h, a

    ; bit mask in b
    ld b, $80
    ld a, d
    and $0e
    rrca
    jp z, +
    -:
        srl b
        dec a
        jp nz, -
    +:

    ld de, MonoFB
    call RotoZoomMonoFB
    ; call UpdateMonoFB
p2:
    jp MainLoop

CopyToVDP:
; Copies data to the VDP
; Parameters: hl = data address, bc = data length
; Affects: a, hl, bc
-:  ld a,(hl)    ; Get data byte
    out (VDPData),a
    inc hl       ; Point to next letter
    dec bc
    ld a,b
    or c
    jr nz,-
    ret



.macro RotoZoomX args twice, other, negate
    ; x offset
    .ifeq negate 1
        .ifeq other 1
            NegateDE
        .else
            NegateBC
        .endif
    .endif
    .repeat twice + 1
        .ifeq other 1
            add ix, de
        .else
            add ix, bc
        .endif
    .endr
    .ifeq negate 1
        .ifeq other 1
            NegateDE
        .else
            NegateBC
        .endif
    .endif
.endm

.macro RotoZoomY args twice, other, negate
    ; y offset
    .ifeq negate 1
        .ifeq other 1
            NegateBC
        .else
            NegateDE
        .endif
    .endif
    .repeat twice + 1
        .ifeq other 1
            add iy, bc
        .else
            add iy, de
        .endif
    .endr
    .ifeq negate 1
        .ifeq other 1
            NegateBC
        .else
            NegateDE
        .endif
    .endif
.endm

.macro RotoZoomGetPixel
    ld a, iyh
    or $80 ; 128px wrap + slot 1 address
    rra
    ld d, a
        
    ld e, ixh
    rl e ; 128px wrap + y bit include

    ld a, (de)
    and b
    cp b
    rr c
.endm

RotoZoomMonoFB:
    ex de, hl

    ld ix, 0 ; x
    ld iy, 0 ; y

    exx
    ld bc, (RotoVX) ; vx
    ld de, (RotoVY) ; vy
    exx

    ld a, d
    and 1
    jp z, +
    exx
    RotoZoomX 1, 1, 1
    RotoZoomY 1, 1, 0
    exx
+:

-:
    push ix
    push iy
    .repeat 32 index x_byte
        .repeat 4 index x_bit
            RotoZoomGetPixel
            exx
            RotoZoomX 0, 0, 0
            RotoZoomY 0, 0, 0
            exx
        .endr
        ld (hl), c
        .ifneq x_byte 31
            inc hl
        .endif
    .endr

    pop iy
    pop ix
    exx
    RotoZoomX 0, 1, 1
    RotoZoomY 0, 1, 0
    exx
    push ix
    push iy

    ld a, l
    sub 31
    ld l, a
    
    .repeat 32 index x_byte
        ld c, (hl)
        .repeat 4 index x_bit
            RotoZoomGetPixel
            exx
            RotoZoomX 0, 0, 0
            RotoZoomY 0, 0, 0
            exx
        .endr
        ld (hl), c
        inc hl
    .endr
    
    pop iy
    pop ix
    exx
    RotoZoomX 2, 1, 1
    RotoZoomY 2, 1, 0
    exx

    ld a, h
    cp $c3
    jp c, -

    ret

UpdateMonoFB:
    ex de, hl
    ld ixl, 24
-:
    .repeat 32 index x_byte
        .repeat 4 index x_bit
            ld e, (4 * x_byte + x_bit) * 2
            ld a, (de)
            and b
            cp b
            rr c
        .endr
        .repeat 4 index x_bit
            ld e, (4 * x_byte + x_bit) * 2 + 1
            ld a, (de)
            and b
            cp b
            rr c
        .endr

        ld (hl), c
        inc hl
    .endr

    inc d
    inc d

    dec ixl
    jp nz, -

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
.db $14,$80,$00,$81,$ff,$82,$41,$85,$fb,$86,$ff,$87,$00,$88,$00,$89,$03,$8a
VDPInitDataEnd:

LocalPalette:
.db $00
.db $aa
.db $55
.db $ff
.repeat 28 index idx
    .db (idx + 4) * 1.5
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

.incdir "anim_128_1/"
.bank 4 slot 1
.org $0000
AnimData:
.incbin "anim1.bin"
.bank 5 slot 1
.org $0000
.incbin "anim2.bin"
.bank 6 slot 1
.org $0000
.incbin "anim3.bin"
.bank 7 slot 1
.org $0000
.incbin "anim4.bin"
