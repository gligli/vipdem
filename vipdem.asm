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
    MonoFB            dsb 768
    RotoPrecalcData   dsb 560
    RotoPrecalcDataEnd    .
    SPSave            dw
    CurFrameIdx       dw
    RotoX             dw ; (8.8 fixed point)
    RotoY             dw ; (8.8 fixed point)
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
    im 1            ; interrupt mode 1
    ; this maps the first 48K of ROM to $0000-$BFFF
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
    ; set up stack
    
    ld sp, $dff0

    ; set up VDP registers

    ld hl,VDPInitData
    ld b,VDPInitDataEnd-VDPInitData
    ld c,VDPControl
    otir

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

    ; init RotoZoom
    ld hl, $0100
    ld (RotoVX), hl
    ld hl, $0100
    ld (RotoVY), hl
    ld a, 1
    ld (DoAnim), a
    
    ; load tiles (Monochrome framebuffer emulation)
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
    
    jp MainLoopStart

MainLoop:
    in a, (VDPScanline)
    cp 192
    jp c, MainLoop
p0:

    ; frame index advance 
    ld a, (CurFrameIdx)
    inc a
    ld (CurFrameIdx), a

    ; animation index
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

    ; RotoZoom control using D-Pad
    in a, (IOPortA)
    ld bc, $0004
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

MainLoopStart:
p1:
    ; anim slot

    ld a, d
    and $30
    .repeat 4
        rrca
    .endr
    or $04

    ld (MapperSlot2), a

    ; anim source pointer

    ld hl, AnimData
    bit 0, d
    jp z, +
    inc h
+:

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
    sla a
    ld l, a

    ; y coord
    ld a, iyh
    sla a
    ld h, a

    push hl
.endm

.macro RotoZoomInitialIncs
    ; y coord
    ld d, (hl)
    dec l

    ; x coord
    ld e, (hl)
.endm

.macro RotoZoomGetPixel args address
    ld hl, (address)
   
    ; y coord (128px wrapped)
    ld a, h
    add a, d
    rrca
    scf ; slot 2 address ($8000)
    rra
    ld h, a

    ; x coord (128px wrapped)
    ld a, l
    adc a, e ; add low y coord bit
    ld l, a 

    ld a, (hl)
    and b
    cp b
    rr c
.endm

RotoZoomMonoFB:
    ex de, hl

    ; precaclulate increments for one line

    exx
    ld hl, 0 ; PushIncs diff
    ld ix, 0 ; x
    ld iy, 0 ; y
    ld bc, (RotoVX) ; vx
    ld de, (RotoVY) ; vy
    exx
    
    ld a, d
    ld de, 0
    and 1
    jp z, RotoDoPrecalc
    
    exx
    NegateDE ; vy = - vy
    RotoZoomX 2, 1
    NegateDE ; vy = - vy
    RotoZoomY 2, 1
    exx
    
    ;jp RotoPrecalcEnd ; no need to redo precalc
    
RotoDoPrecalc:
    ld (SPSave), sp
    ld sp, RotoPrecalcDataEnd

    ld c, 24
    exx
    NegateDE ; vy = - vy
-:    
        RotoZoomX 4, 1
        RotoZoomY 4, 1
        RotoZoomPushIncs 1
        exx
        dec c
        exx
        jp nz, -

    NegateDE ; vy = - vy

    ; even line

    ld ix, 0 ; x
    ld iy, 0 ; y

    exx
    ld c, 128
    exx
-:    
        RotoZoomX 1, 0
        RotoZoomY 1, 0
        RotoZoomPushIncs 0
        exx
        dec c
        exx
        jp nz, -
    
    ; odd line, reverse direction (zig-zag)

    NegateDE ; vy = - vy
    RotoZoomX 1, 1
    RotoZoomY 1, 1
    NegateBC ; vx = - vx
    
    exx
    ld c, 128
    exx
    jp +
-:    
        RotoZoomX 1, 0
        RotoZoomY 1, 0
+:
        RotoZoomPushIncs 0
        exx
        dec c
        exx
        jp nz, -

    exx

    ld sp, (SPSave)

;    ret
RotoPrecalcEnd:
    
    ; main loop on lines pairs
    
    ld hl, MonoFB
    ld iy, 23 ; line counter
    
    ld a, b
    exx
    ld b, a
    exx

RotoLineLoop:
    exx
    ld hl, RotoPrecalcDataEnd - 1
    ; advance hl "line" items
    ld a, iyh
    sla a
    add a, l
    ld l, a
    RotoZoomInitialIncs
    exx

    ; even line
    
    .repeat 32 index x_byte
        exx
        .repeat 4 index x_bit
            RotoZoomGetPixel RotoPrecalcDataEnd - 2 - 48 - x_bit * 2 - x_byte * 8
        .endr
        ld a, c
        exx
        ld (hl), a
        .ifneq x_byte 31
            inc hl
        .endif
    .endr
    
    ; odd line
    
    .repeat 32 index x_byte
        ld a, (hl)
        exx
        ld c, a
        .repeat 4 index x_bit
            RotoZoomGetPixel RotoPrecalcDataEnd - 2 - 48 - 256 - x_bit * 2 - x_byte * 8
        .endr
        ld a, c
        exx
        ld (hl), a
        .ifneq x_byte 31
            dec hl
        .endif
    .endr
    
    ; advance to next line pair
    
    ld a, 32
    AddAToHL
    
    dec iyh
    dec iyl
    jp nz, RotoLineLoop
    
    ret

UpdateMonoFB:
    ld de, MonoFB
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
            ld e, (4 * x_byte + 4 - x_bit) * 2 - 1
            ld a, (de)
            and b
            cp b
            rr c
        .endr

        ld (hl), c
        inc hl
    .endr

    .repeat 2
        inc d
    .endr

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
.repeat 2
    .db 27
    .db 16
    .db 47
    .db 32
    .repeat 11 index idx
        .db idx * 3
    .endr
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

.incdir "anim_128_1/"
.bank 4 slot 2
.org $0000
AnimData:
.incbin "anim1.bin"
.bank 5 slot 2
.org $0000
.incbin "anim2.bin"
.bank 6 slot 2
.org $0000
.incbin "anim3.bin"
.bank 7 slot 2
.org $0000
.incbin "anim4.bin"
