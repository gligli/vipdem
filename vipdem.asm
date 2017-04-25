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

;==============================================================
; RAM variables
;==============================================================

.enum $c000 export
    CurFrameIdx       dw
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
    or a
    jp m, VBlankInt
    in a, (VDPScanline)
    and $04
    or $fb
    out (VDPControl), a
    ld a, $82
    out (VDPControl), a

    ex af, af'
    ei
    reti

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

    ld a, $02
    ld (MapperSlot1), a
    ld a, $03
    ld (MapperSlot2), a

    ; Output tilemap data
    SetVDPAddress $2800 | VRAMWrite
    ld hl, AnimData + $20
    call UploadMonoFB

    SetVDPAddress $3800 | VRAMWrite
    ld hl, AnimData
    call UploadMonoFB

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

    ei

MainLoop:
    halt
    jp MainLoop

VBlankInt:
    ld a, (CurFrameIdx)
    inc a
    ld (CurFrameIdx), a

    and $0f
    .repeat 3
        rlca
    .endr

    ld hl, AnimData
    or h
    ld h, a
    ;SetVDPAddress $3800 | VRAMWrite
    ;call UploadMonoFB

    ex af, af'
    ei
    reti

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

UploadMonoFB:
    ld b, 24
-:
    .repeat 16 index idx
        res 4, l
        ld d, (hl)
        set 4, l
        ld e, (hl)

        ld a, d
        and $0f
        ld c, a
        
        .ifneq idx 0
            xor a
            out (VDPData), a
        .endif

        ld a, e
        .repeat 4
            rlca
        .endr
        and $f0
        or c
        out (VDPData), a

        ld a, d
        .repeat 4
            rrca
        .endr
        and $0f
        ld c, a

        xor a
        out (VDPData), a

        ld a, e
        and $f0
        or c
        out (VDPData), a

        .ifneq idx 15
            inc l
        .endif
    .endr

    ld a, l
    and $e0
    add a, $40
    ld l, a
    adc a, h
    sub l
    ld h, a

    xor a
    out (VDPData), a

    dec b
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
.db $ff
.db $55
.db $aa
.db $00
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
.bank 2 slot 1
.org $0000
AnimData:
.incbin "0000.mono"
.incbin "0001.mono"
.incbin "0002.mono"
.incbin "0003.mono"
.incbin "0004.mono"
.incbin "0005.mono"
.incbin "0006.mono"
.incbin "0007.mono"
.bank 3 slot 2
.org $0000
.incbin "0008.mono"
.incbin "0009.mono"
.incbin "0010.mono"
.incbin "0011.mono"
.incbin "0012.mono"
.incbin "0013.mono"
.incbin "0014.mono"
.incbin "0015.mono"
