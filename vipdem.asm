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
    MonoFB            dsb 800
    MonoFBEnd          . 
    RotoPrecalcData   dsb 48
    RotoPrecalcDataEnd .
    RotoRAMBakeIncs   dsb 512  

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
    ei
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
    
;==============================================================
; Main loop
;==============================================================

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
    ld hl, MonoFBEnd - 1
    ld c, VDPData
    .repeat 768
        outd
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

.ifeq 1 1
    call RotoZoomMonoFB
.else
    call UpdateMonoFB
.endif
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
    sla a
    ld l, a

    ; y coord
    ld a, iyh
    sla a
    ld h, a

    push hl
.endm

.macro RotoZoomBakeIncs
    exx

    ld a, (hl)
    inc hl
    AddAToDE

    ; y coord
    ld a, iyh
    sla a
    ld (de), a
    
    ld a, (hl)
    inc hl
    AddAToDE
    
    ; x coord
    ld a, ixh
    sla a
    ld (de), a

    exx
.endm

.macro RotoZoomGetPixel args idx

.ifeq idx 0
RotoRAMCodeBakePosA0:
.endif
.ifeq idx 1
RotoRAMCodeBakePosA1:
.endif
.ifeq idx 8
RotoRAMCodeBakePosA8:
.endif
.ifeq idx 16
RotoRAMCodeBakePosA16:
.endif

    ; y coord (128px wrapped)
    ld a, $55 ; placeholder value
    add a, d
    rrca
    scf ; slot 2 address ($8000)
    rra
    ld h, a

.ifeq idx 0
RotoRAMCodeBakePosB0:
.endif
.ifeq idx 7
RotoRAMCodeBakePosB7:
.endif
.ifeq idx 15
RotoRAMCodeBakePosB15:
.endif

    ; x coord (128px wrapped)
    ld a, $55 ; placeholder value
    adc a, e ; add low y coord bit
    ld l, a 

    ld a, (hl)
    and b
    cp b
    rr c
.endm

RotoZoomInit:
    ld hl, $0100
    ld (RotoVX), hl
    ld hl, $0100
    ld (RotoVY), hl
    ld a, 1
    ld (DoAnim), a

    ; copy code to RAM, duplicating it
    ld de, RAMCode
    .repeat 8
        ld hl, RotoRAMCodeStart
        ld bc, RotoRAMCodeEnd - RotoRAMCodeStart
        ldir
    .endr
    ld bc, RotoRAMCodeRet - RotoRAMCodeEnd
    ldir

    ; get offset increments in RAM code where data will be copied in
    ld hl, RotoRAMBakeIncs
    ld d, RotoRAMCodeBakePosA0 - RotoRAMCodeStart + 1
    ld e, 0
-:    
    ld (hl), d
    inc hl
    
    ld d, RotoRAMCodeBakePosB0 - RotoRAMCodeBakePosA0

    ld (hl), d
    inc hl
        
    ld a, e
    and $0f
    cp 7
    jp nz, +
    ld d, RotoRAMCodeBakePosA8 - RotoRAMCodeBakePosB7
    jp ++
+:
    cp 15
    jp nz, +
    ld d, RotoRAMCodeBakePosA16 - RotoRAMCodeBakePosB15
    jp ++
+:
    ld d, RotoRAMCodeBakePosA1 - RotoRAMCodeBakePosB0
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
    RotoZoomX 4, 1
    RotoZoomY 4, 1
    RotoZoomPushIncs
    exx

    dec c
    jp nz, -

    exx
    ; hl thrashed by RotoZoomPushIncs
    ld hl, (RotoVY) ; vy
    ex de, hl ; vy = - vy
    exx

    ; zig-zagging through 2 consecutive lines (pattern: ¨¨|_| )

    ld hl, RotoRAMBakeIncs
    ld de, RAMCode
    
    ld ix, 0 ; x
    ld iy, 0 ; y

    ld c, 64
-:    
    exx
    RotoZoomX 1, 0
    RotoZoomY 1, 0
    RotoZoomBakeIncs

    ex de, hl ; vy = - vy
    RotoZoomX 1, 1
    ex de, hl ; vy = - vy
    RotoZoomY 1, 1
    RotoZoomBakeIncs

    RotoZoomX 1, 0
    RotoZoomY 1, 0
    RotoZoomBakeIncs

    RotoZoomX 1, 1
    NegateBC ; vx = - vx
    RotoZoomY 1, 1
    NegateBC ; vx = - vx
    RotoZoomBakeIncs
    exx

    dec c
    jp nz, -
    
;    ret
RotoPrecalcEnd:

    ld a, b
    exx
    ld b, a
    exx

    ld sp, MonoFBEnd
    ld hl, 24 ; line counter
    
    ; main loop on lines pairs
    
RotoLineLoop:
    ld a, h
    exx
    ld hl, RotoPrecalcDataEnd - 1
    ; advance hl "line" items
    add a, l
    ld l, a
    ; y coord
    ld d, (hl)
    dec l
    ; x coord
    ld e, (hl)
    
    jp RAMCode
RotoRAMCodeStart:    
    .repeat 2 index x_dummy
        .repeat 8 index x_bit
            RotoZoomGetPixel x_dummy * 16 + x_bit
        .endr
        ld iyh, c
        .repeat 8 index x_bit
            RotoZoomGetPixel x_dummy * 16 + x_bit + 8
        .endr
        ld iyl, c
        
        push iy
    .endr
RotoRAMCodeEnd:
    jp RotoRAMCodeRet
RotoRAMCodeRet:
    
    exx

    dec h
    dec h
    dec l
    jp nz, RotoLineLoop
    
    ld sp, (SPSave)

    ret

;==============================================================
; MonoFB code
;==============================================================

.macro MonoFBPixel args x_byte_, pos
    ld e, 8 * x_byte_ + pos
    ld a, (de)
    and b
    cp b
    rr c
.endm
    
UpdateMonoFB:
    ld de, MonoFBEnd - 1
    ex de, hl
    ld ixl, 24
-:
    .repeat 32 index x_byte
        MonoFBPixel x_byte, 0
        MonoFBPixel x_byte, 1
        MonoFBPixel x_byte, 3
        MonoFBPixel x_byte, 2
        MonoFBPixel x_byte, 4
        MonoFBPixel x_byte, 5
        MonoFBPixel x_byte, 7
        MonoFBPixel x_byte, 6
        ld (hl), c
        dec hl
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
