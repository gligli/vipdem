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

;==============================================================
; RAM variables
;==============================================================

.enum $c000 export
    RotoPrecalcData   dsb 96 ; align 256
    RotoPrecalcDataEnd .
    RotoRAMBakeAlign  dsb 32
    RotoRAMBakeIncs   dsb 128 ; align 128
    RotoRAMBakeIncsEnd .

    SPSave            dw
    CurFrameIdx       dw
    RotoX             dw ; (8.8 fixed point)
    RotoY             dw ; (8.8 fixed point)
    RotoVX            dw ; (8.8 fixed point)
    RotoVY            dw ; (8.8 fixed point)
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
    ld b, $00
-:
    ld (hl), b
    inc hl
    ld a,h
    cp $e0
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
    SetVDPAddress $37ff | VRAMWrite

    ; RotoZoom control using D-Pad
    in a, (IOPortA)
    bit 5, a
    jp nz, +
        push af
        ld a, (CurEffect)
        cpl
        ld (CurEffect), a
        pop af
        call RotoZoomInit
+:

    bit 4, a
    jp z, ++

    ld bc, $0010

    ld hl, (RotoVY)
    bit 0, a
    jp nz, +
        or a
        sbc hl, bc
+:
    bit 1, a
    jp nz, +
        add hl, bc
+:
    ld (RotoVY), hl
    ld hl, (RotoVX)
    bit 2, a
    jp nz, +
        or a
        sbc hl, bc
+:
    bit 3, a
    jp nz, +
        add hl, bc
+:
    ld (RotoVX), hl
    jp +++
   
++:
    ld bc, $0200
    ld hl, (RotoY)
    bit 0, a
    jp nz, +
        or a
        sbc hl, bc
+:
    bit 1, a
    jp nz, +
        add hl, bc
+:
    ld (RotoY), hl
    ld hl, (RotoX)
    bit 2, a
    jp nz, +
        or a
        sbc hl, bc
+:
    bit 3, a
    jp nz, +
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

    inc l

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

.macro RotoGetLineOffsets args fixup
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
.ifeq fixup 1
    ; fixup regs
    ld h, b
    ld l, c
.endif
.endm

RotoZoomInit:
    ld hl, $0200
    ld (RotoVX), hl
    ld hl, $0000
    ld (RotoVY), hl

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
    ex de, hl

    ; precaclulate increments for one line

    exx
    ld ix, (RotoX) ; x
    ld iy, (RotoY) ; y
    ld bc, (RotoVX) ; vx
    ld de, (RotoVY) ; vy
    ld h, d
    ld l, e
    NegateHL
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
    RotoZoomX 1, 1
    RotoZoomY 1, 1
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

    ld ix, (RotoX) ; x
    ld iy, (RotoY) ; y

    ld b, 0
-:
    exx
    .repeat 16
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
    ld l, 24 ; line counter

    ; main loop on lines pairs

RotoLineLoop:
    ld a, l
    dec a
    add a, a
    add a, a
    exx
    RotoGetLineOffsets 1    
     
    jp RAMCode
RotoRAMCodeStart:
    .repeat 2 index x_dummy
        RotoZoomGetPixel x_dummy * 4 + 0
        RotoZoomGetPixel x_dummy * 4 + 1
        ld c, VDPData
        in (c)
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

.macro PM7PushIncs args line
    ; x coord
    ld a, (line >> 1) * 8
    add a, ixh
    ld l, a

    ; y coord
    ld a, iyh
    ld h, a

    push hl
.endm

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

PseudoMode7MonoFB:
    ex de, hl

    ; precaclulate increments for one line

    exx
    ld ix, (RotoX) ; x
    ld iy, (RotoY) ; y
    ld bc, (RotoVX) ; vx
    ld de, (RotoVY) ; vy
    ld hl, (RotoVY) ; -vy
    NegateHL
    exx

PM7DoPrecalc:
    ld (SPSave), sp
    ld sp, RotoPrecalcDataEnd

    xor a
    exx
    ex de, hl ; vy = - vy

    .repeat 48 index y_line
        PM7PushIncs y_line
        RotoZoomX 1, 1
        RotoZoomY 1, 1
    .endr

    ; hl thrashed by PM7PushIncs
    ld hl, (RotoVY) ; vy
    ex de, hl ; vy = - vy
    exx
    
    ; line increments of vx
    
    ld sp, RotoRAMBakeIncs + 48 ; reusing this buffer for line incs
    ld a, 48
    ld c, a
-:
    ld a, c
    add a, a
    add a, a
    ld e, a

    ld d, 0
    sla e
    rl d
    sla e
    rl d

    ld hl, (RotoVX)
    add hl, de
    push hl

    dec c
    dec c
    jp p, -

PM7PrecalcEnd:

    ld sp, (SPSave)
    exx
    ld l, 14 ; line counter
    exx
    
    ld c, VDPData

    ; main loop on lines pairs

PM7LineLoop:
    ld ix, (RotoX) ; x
    ld iy, (RotoY) ; y

    exx
    ; get line inc
    ld a, l
    add a, a
    add a, <RotoRAMBakeIncs - 2
    ld b, >RotoRAMBakeIncs
    ld c, a
    ld a, (bc)
    ld h, a
    inc c
    ld a, (bc)
    ld b, a
    ld c, h

    ; for line offsets
    ld a, l
    dec a
    add a, a
    add a, a
    exx
        
    RotoGetLineOffsets 0
    
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
