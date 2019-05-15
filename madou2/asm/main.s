
.include "sys/sms_arch.s"
  ;.include "base/ram.s"
;.include "base/macros.s"
  ;.include "res/defines.s"

.rombankmap
  bankstotal 64
  banksize $4000
  banks 64
.endro

.emptyfill $FF

.background "madou2.gg"

;.unbackground $80000 $FFFFF
; don't free the script banks
.unbackground $80000 $FFFFF

;======================
; free unused space
;======================

; end-of-banks
.unbackground $7F6B $7FEF
.unbackground $BA60 $BB5F

; diacritic handler
.unbackground $281E $2839

; dictionary handler
.unbackground $28C8 $28EB

.include "vwf_consts.inc"
.include "ram.inc"
.include "util.s"
.include "vwf.s"
.include "vwf_user.s"

;.macro orig_read16BitTable
;  rst $20
;.endm

; B = tile count
; DE = srcptr
; HL = dstcmd
.macro rawTilesToVdp_macro
  ; set vdp dst
  ld c,vdpCtrlPort
  out (c),l
  out (c),h
  ; write data to data port
  ex de,hl
  dec c
  ld a,b
  -:
    .rept bytesPerTile
      push ix
      pop ix
      outi
    .endr
    dec a
    jp nz,-
.endm

; B = tile count
; DE = srcptr
; HL = dstcmd
.macro rawTilesToVdp_macro_safe
  ; set vdp dst
  ld c,vdpCtrlPort
  out (c),l
  nop
  out (c),h
  nop
  ; write data to data port
  ex de,hl
  dec c
  ld a,b
  -:
    .rept bytesPerTile
      push ix
      pop ix
      outi
    .endr
    dec a
    jp nz,-
.endm

; B = tile count
; DE = srcptr
; HL = srccmd
.macro rawTilesFromVdp_macro
  ; set vdp src
  ld c,vdpCtrlPort
  out (c),l
  nop
  out (c),h
  nop
  ; read data from data port
  ex de,hl
  dec c
  ld a,b
  -:
    .rept bytesPerTile
      push ix
      pop ix
      ini
    .endr
    dec a
    jp nz,-
.endm

; BC = tile count
; DE = srcptr
; HL = dstcmd
.macro rawTilesToVdp_big_macro
  push bc
    ; set vdp dst
    ld c,vdpCtrlPort
    out (c),l
    nop
    out (c),h
    nop
  pop bc
  ; write data to data port
  ex de,hl
  -:
    push bc
      ld c,vdpDataPort
      .rept bytesPerTile
        push ix
        pop ix
        outi
      .endr
    pop bc
    
    dec bc
    ld a,b
    or c
    jr nz,-
.endm

;.macro old_read16BitTable
;  rst $28
;.endm

;===============================================
; Update header after building
;===============================================
.smstag

;========================================
; local defines
;========================================

.define numScriptRegions 5

;========================================
; vwf settings
;========================================

;  ld a,vwfTileSize_main
;  ld b,vwfScrollZeroFlag_main
;  ld c,vwfNametableHighMask_main
;  ld hl,vwfTileBase_main
;  doBankedCall setUpVwfTileAlloc

;.bank $01 slot 1
;.section "extra startup code" free
;  newStartup:
;    ; init vwf
;    ld a,vwfTileSize_main
;    ld b,vwfScrollZeroFlag_main
;    ld c,vwfNametableHighMask_main
;    ld hl,vwfTileBase_main
;    doBankedCallSlot1 setUpVwfTileAlloc
;    
;    ret
;.ends

;========================================
; script
;========================================

; each script region is assigned one bank starting from this bank
.define scriptBaseBank $20

;.include "out/script/string_bucket_hashtabledialogue.inc"
;
;.slot 2
;.section "enemy names" superfree
;  enemyNames:
;    .incbin "out/script/enemies.bin"
;  enemyNamesPlural:
;    .incbin "out/script/enemies_plural.bin"
;  
;  
;.ends

.bank scriptBaseBank+0 slot 2
.org $0000
.section "script region 0" force
  scriptRegion0:
    .incbin "out/script/region0.bin"
.ends

.bank scriptBaseBank+1 slot 2
.org $0000
.section "script region 1" force
  scriptRegion1:
    .incbin "out/script/region1.bin"
.ends

/*.bank scriptBaseBank+2 slot 2
.org $0000
.section "script region 2" force
  scriptRegion2:
    .incbin "out/script/region2.bin"
.ends */

.bank scriptBaseBank+2 slot 2
.org $0000
.section "script region 2a" force
  scriptRegion2a:
    .incbin "out/script/region2a.bin"
.ends

.bank scriptBaseBank+2+numScriptRegions slot 2
.org $0000
.section "script region 2b" force
  scriptRegion2b:
    .incbin "out/script/region2b.bin"
.ends

.bank scriptBaseBank+3 slot 2
.org $0000
.section "script region 3" force
  scriptRegion3:
    .incbin "out/script/region3.bin"
.ends

.bank scriptBaseBank+4 slot 2
.org $0000
.section "script region 4" force
  scriptRegion4:
    .incbin "out/script/region4.bin"
.ends

;========================================
; use new script
;========================================

.bank 0 slot 0
.org $26A4
.section "use new script" overwrite
  ; B = region num
  ; HL = $FFFF
  
  doBankedCallSlot2 useNewScript2
  
  ; load target bank in slot2
  ld (hl),a
  ; base pointer will always be $8000
  ld hl,$8000
  ; jump to script lookup
  jp $26D5
.ends

.slot 2
.section "use new script 2" superfree
  useNewScript2:
    ; B = region num
    ; HL = $FFFF
    
    ; if region is 2 and message number >= 0x80, target second half
    ; in extra bank
    ld a,2
    cp b
    jr nz,+
      ld a,$7F
      cp c
      jr nc,+
        ld a,b
        add a,numScriptRegions
        ld b,a
        
        ld a,c
        sub $80
        ld c,a
    +:
    
    ; new target bank = scriptBaseBank+$20
    ld a,scriptBaseBank
    add a,b
    
    ret
.ends

; unbackground disused space
.unbackground $26B9 $26D4

;========================================
; use vwf printing
;========================================

; reset vwf before starting new string
.bank 0 slot 0
.org $2714
.section "use new printing 1a" overwrite
  jp newScriptRunLoop
.ends

.bank 0 slot 0
.section "use new printing 1b" free
  newScriptRunLoop:
    ; reset vwf before running script
;    call resetVwf
    doBankedCallSlot2 fullyResetVwf
    -:
      ; handle next command
      call $271B
      ; continue while termination code not returned
      or a
      jr z,-
    ret
.ends

.bank 0 slot 0
.org $27D4
.section "use new printing 2" SIZE $47 overwrite
  ; save tilemap target address
  ld (vwfTilemapTargetAddr),de
  
  ; fetch from srcptr
  ld a,(hl)
  
  ; check for char-by-char print flag
  cp vwfCbcModeIndex
  jr nz,+
    ; save address of remaining script data for future printing operations
    ld a,(mapperSlot2Ctrl)
    ld (vwfCbcScriptBank),a
    inc hl
    ld (vwfCbcScriptAddr),hl
    
    ; reset vwf in preparation for cbc print
    doBankedJumpSlot2 doCbcSetup
  +:
  
  ; print character
;  push hl
  ; B = src bank
  ld a,(mapperSlot2Ctrl)
  ld b,a
  ; A = character to handle
  ld a,(hl)
  
  ; hl should point to next script byte
  inc hl
    
    doBankedCallSlot2 handleVwfOp
    
    ; get updated tilemap target address
    ld de,(vwfTilemapTargetAddr)
  ; will be incremented back in following code
  dec hl
;  pop hl
  
  @done:
  jp $281B
  
.ends

; reset vwf at start of each box
.bank 0 slot 0
.org $286A
.section "use new printing 3a" overwrite
  jp resetVwfBeforeBox
.ends

.bank 1 slot 1
.section "use new printing 3b" free
  resetVwfBeforeBox:
    ; make up work
    ld (pendingExpRamTileCount),a
    
    doBankedCallSlot2 fullyResetVwf
    jp $281B
.ends

;========================================
; 1-line linebreaks
;========================================

.bank 0 slot 0
.org $28A0
.section "linebreak height" overwrite
  ; bytes in virtual tilemap to skip
  ld de,$0050/2
.ends

;========================================
; reset vwf on linebreak
;========================================

.bank 0 slot 0
.org $28A9
.section "linebreak 1" overwrite
  ; done
  jp linebreakVwfReset
.ends

.bank 0 slot 0
.section "linebreak 2" free
  linebreakVwfReset:
    doBankedCallSlot2 resetVwf
    jp $281B
.ends

;========================================
; char-by-char printing
;========================================

.bank 0 slot 0
.org $2B64
;.section "char-by-char printing 1" SIZE $5A overwrite
.section "char-by-char printing 1" SIZE $24 overwrite
  ; if cbc printing off, do nothing
  ld a,(vwfCbcActiveFlag)
  or a
  ret z
  
  ; fetch next script byte
  ld a,(vwfCbcScriptBank)
  ld b,a
  ld hl,(vwfCbcScriptAddr)
  push hl
    call bankedFetch
    
    doBankedCallSlot2 cbcPrint
  @done:
  pop hl

  ; advance scriptptr
  inc hl
  ld (vwfCbcScriptAddr),hl
  
  ret
.ends

.unbackground $2B88 $2BBD

;========================================
; new main print routines
;========================================

  ;========================================
  ; new dialogue printing
  ;========================================

  ; HL = local x/y
  ; IX = string pointer
/*  .macro printString_logic_macro
    push ix
    push de
      ; save target x/y
      push hl
        ; get string pointer in HL
        push ix
        pop hl
        ; look up hashed string
        call getStringHash
        ; get hashed pointer in IX
        push hl
        pop ix
      ; restore target x/y
      pop hl
      
      ; switch to target bank
      ld a,(mapperSlot2Ctrl)
      push af
        ld a,c
        
        ; check if bank switch needed (shouldn't happen unless error)
        or a
        jp m,+
          ld (mapperSlot2Ctrl),a
        +:
        ; print hashed string
        doBankedCallSlot1 startNewString
      pop af
      ld (mapperSlot2Ctrl),a
    pop de
    pop ix
  .endm
  
  .bank $00 slot 0
  .org $1D74
  .section "new printString 1" SIZE $4B overwrite
    printString:
      printString_logic_macro
      
      ; callers expect IX to now point to one past the original string's
      ; terminator, so seek through it.
      ; we don't need to parse any ops -- the only ones with parameters
      ; are [char] and [textspeed], which should never use FF anyways
      -:
        ld a,(ix+0)
        inc ix
        cp terminatorIndex
        jr nz,-
      
      ret
  .ends */

;========================================
; use extra VRAM to allow for more text
; in boxes
;========================================

; if more material is pending than will fit in the original area,
; split up the transfer
.bank 0 slot 0
.org $2795
.section "extra text space 1" overwrite
  doBankedCallSlot2 extraTextSpace_transferSplit
;  jp $27D1
  jp $27AC
.ends

.slot 2
.section "extra text space 2" superfree
  extraTextSpace_transferSplit:
    ; HL = srcaddr
    ; DE = vdpdst
    
    ; save current vdpdst
    ld (currentTextTilesVdpTarget),de
    ld (currentTextTilesExpRamTarget),hl
      
    ; get current text box type
    ; 0 = right box
    ; 1 = left box
    ; 2 = bottom box
    ld a,(textWindowType)
    cp 1
    jr z,@leftBox
    cp 2
    jp z,@bottomBox
      
    @rightBox:
      ld hl,rightBoxNewSpaceVdpAddr
      ld (boxNewSpaceVdpAddr),hl
      ld hl,rightBoxOldSpaceEndTileNum
      ld (boxOldSpaceEndTileNum),hl
      ld hl,rightBoxOldSpaceEndVdpAddr
      ld (boxOldSpaceEndVdpAddr),hl
      jr @firstTransferCheck
      
    @leftBox:
      ld hl,leftBoxNewSpaceVdpAddr
      ld (boxNewSpaceVdpAddr),hl
      ld hl,leftBoxOldSpaceEndTileNum
      ld (boxOldSpaceEndTileNum),hl
      ld hl,leftBoxOldSpaceEndVdpAddr
      ld (boxOldSpaceEndVdpAddr),hl
      ; drop through
    
    @firstTransferCheck:
      ;=====
      ; if vdpdst (de) >= boxNewSpaceVdpAddr, we've already moved
      ; to the new area: skip
      ;=====
      
      ld hl,(boxNewSpaceVdpAddr)
      or a
      sbc hl,de
      
;      jr z,+
;      jr c,+
        jp c,@finalTransfer
;      +:
      
      ;=====
      ; if (vdpdst + (tilecount * bytesPerTile)) <= boxOldSpaceEndVdpAddr,
      ; we're transferring entirely to the old area: skip
      ;=====
      
      ; A = count of tiles to be sent
      ld a,(pendingExpRamTileCount)
      
      ; HL = vdpdst
;      ld hl,(currentTextTilesVdpTarget)
      ; divide by bytesPerTile (32)
      .rept 5
        srl d
        rr e
      .endr
      
      ; A = low byte of target tile index (high byte is 1)
      add a,e
      
      ; if target tile index <= boxOldSpaceEndTileNum, no second
      ; transfer needed
      ld hl,boxOldSpaceEndTileNum
;      cp <boxOldSpaceEndTileNum
      cp (hl)
      jp z,@finalTransfer
      jp c,@finalTransfer
      
      ;=====
      ; otherwise,
      ; transfer ((boxOldSpaceEndVdpAddr - vdpdst) / bytesPerTile) tiles
      ; to vdpdst.
      ; then set up for final transfer to boxNewSpaceVdpAddr
      ; with (tilecount - ((boxOldSpaceEndVdpAddr - vdpdst) / bytesPerTile))
      ; tiles.
      ;=====
      
      ld hl,(boxOldSpaceEndVdpAddr)
      ld de,(currentTextTilesVdpTarget)
      or a
      sbc hl,de
      ; divide by bytesPerTile (32)
      .rept 5
        srl h
        rr l
      .endr
      
      ; A = first transfer size
      ld a,l
      
      ; do transfer
      ld hl,(currentTextTilesExpRamTarget)
      ld de,(currentTextTilesVdpTarget)
      push af
        call sendPendingExpRamTiles
      pop af
      
      ; set up new srcaddr for final transfer
      push af
        ld de,(currentTextTilesExpRamTarget)
        
        ; multiply tiles transferred by bytesPerTile (32)
        ld l,$00
        srl a
        rr l
        srl a
        rr l
        srl a
        rr l
        ld h,a
        
        ; add srcaddr
        add hl,de
        ld (currentTextTilesExpRamTarget),hl
      pop af
      
      ; calculate size of final transfer
      ld e,a
      ld a,(pendingExpRamTileCount)
      sub e
      ld (pendingExpRamTileCount),a
      
      ; vdpdst = new space
      ld de,(boxNewSpaceVdpAddr)
      ld (currentTextTilesVdpTarget),de
      
      jp @finalTransfer
      
      
    @bottomBox:
      ; do nothing
    
    
    
    @finalTransfer:
    ld hl,(currentTextTilesExpRamTarget)
    ld de,(currentTextTilesVdpTarget)
    ld a,(pendingExpRamTileCount)
    push af
      call sendPendingExpRamTiles
    pop af

    ; make up work
    ; HL = tile count * 32
    ld l,$00
    srl a
    rr l
    srl a
    rr l
    srl a
    rr l
    ld h,a
    ; update target VDP address
;    ld (currentTextTilesVdpTarget),hl
    
    ; mark transfer as complete
;    res 6,(hl)
    ret
.ends

;========================================
; bugfix: reinitialize reserve encounter
; table at CC00 during the lyra ruins
; cutscene.
; otherwise, garbage data will be left
; behind that will cause glitched
; encounters on dungeon floor 4 if the
; player manages to make it there
; without resetting.
;========================================

  ;=====
  ; during normal scene progression
  ;=====

;  .bank 2 slot 2
;  .org $2EB2
;  .section "reserve encounter table fix 1" overwrite
;    ; initialize the table instead of simply shutting off
;    ; the effect
;    call $0BCD
;  .ends

  ;=====
  ; if scene skipped with button press
  ; (actually, turns out this runs at the end of the scene anyway)
  ;=====

  .bank 2 slot 2
  .org $28D1
  .section "reserve encounter table fix 2a" overwrite
    doBankedCallSlot2NoParams reserveTableFix_sceneEnd
    nop
    nop
  .ends

  .slot 2
  .section "reserve encounter table fix 2b" superfree
    reserveTableFix_sceneEnd:
      ; make up work
;      call $0ED0
;      call $2DC9
      call $3001
      xor a
      ld ($C016),a
      ld ($C0F0),a
      
      ; shut off cbc mode
      xor a
      ld (vwfCbcActiveFlag),a
      
      ; initialize the table instead of simply shutting off
      ; the effect.
      
      ; THIS WILL FORCE SLOT 2 TO BANK 2, MAPPING THIS CODE OUT!
      ; IT MUST BE THE LAST THING THAT HAPPENS IN THIS ROUTINE!
      jp $0BCD
      
      ; make up work
;      jp $0B5F
  .ends

  ;=====
  ; for opening scene, make sure cbc mode gets shut off
  ;=====

  .bank 2 slot 2
  .org $1A95
  .section "reserve encounter table fix 3a" overwrite
    doBankedCallSlot2NoParams intro_sceneEnd_ext
    nop
    nop
  .ends

  .slot 2
  .section "reserve encounter table fix 3b" superfree
    intro_sceneEnd_ext:
      ; make up work
;      call $0ED0
;      call $2DC9
      call $3001
      xor a
      ld ($C016),a
      ld ($C0F0),a
      
      ; shut off cbc mode
      xor a
      ld (vwfCbcActiveFlag),a
      
      ret
  .ends

;========================================
; don't print hardcoded prices in shops
; (they're now handled with script ops)
;========================================

; sell price
.bank 0 slot 0
.org $3CA8
.section "no shop prices 1" overwrite
  nop
  nop
  nop
.ends

; sell price confirmation
.bank 0 slot 0
.org $3D46
.section "no shop prices 2" overwrite
  nop
  nop
  nop
.ends

; buy price confirmation
.bank 0 slot 0
.org $3C8A
.section "no shop prices 3" overwrite
  nop
  nop
  nop
.ends

;========================================
; don't print hardcoded save file
; numbers
;========================================

.bank 0 slot 0
.org $368D
.section "no save file numbers 1" overwrite
  jp $3699
.ends

;========================================
; don't print hardcoded dungeon floor
; numbers
;========================================

.bank 0 slot 0
.org $1EB7
.section "no dungeon floor numbers 1" overwrite
  ; overwrite original call to print routine
  
  ; save scriptnum
  ld a,c
  ld (paramWord1),a
  nop
.ends

.bank 0 slot 0
.org $1ED5
.section "no dungeon floor numbers 2" SIZE 22 overwrite
  ; C = tens digit of floor number
  ; B = ones digit
  
  ; update number buffer
  
  ld hl,numberBufferStart
  
  ; top 2 digits zero
  xor a
  ld (hl),a
  inc hl
  ld (hl),a
  inc hl
  
  ld a,c
  ld (hl),a
  inc hl
  
  ld a,b
  ld (hl),a
;  inc hl
  
  ; make up the print call we previously omitted
  ld a,(paramWord1)
  ld c,a
  call runRegion0Script
  nop
  nop
  
.ends

;========================================
; fix "leave" button in shops.
; original game optimizes うる and でる
; to recycle the right part of the る
; for both options, which obviously
; isn't going to fly in English.
;========================================

  .bank 0 slot 0
  .org $3AC9
  .section "fix shop buttons 1" overwrite
    doBankedCallSlot2NoParams loadNewShopButtons
  .ends
  
  .bank 2 slot 2
  .section "fix shop buttons 2" free
    loadNewShopButtons:
      doBankedCallSlot2NoParams loadNewShopButtons_content
    
      ; make up work
      ld de,$B5E5
      ; load tilemap
      call $3F1C
      ld c,$64
      ret
  .ends
  
  ; tile $176
  .define newLeaveButtonVramTile $176
  .define newLeaveButtonVramTarget (newLeaveButtonVramTile*bytesPerTile)|$4000
  
  .slot 2
  .section "fix shop buttons 3" superfree
    newLeaveButton:
      .incbin "out/button_leave_new.bin" FSIZE newLeaveButtonSize
    .define newLeaveButtonNumTiles newLeaveButtonSize/bytesPerTile
    
    loadNewShopButtons_content:
      ; copy in new tiles
      ld b,newLeaveButtonNumTiles
      ld de,newLeaveButton
      ld hl,newLeaveButtonVramTarget
      di
        rawTilesToVdp_macro_safe
      ei
      
      ret
  .ends

  ; update tilemap
  
  .bank 6 slot 2
  .org $35F1
  .section "fix shop buttons tilemap 1" overwrite
    .db <newLeaveButtonVramTile+0,<newLeaveButtonVramTile+1,<newLeaveButtonVramTile+2
  .ends
  
  .bank 6 slot 2
  .org $35F8
  .section "fix shop buttons tilemap 2" overwrite
    .db <newLeaveButtonVramTile+3,<newLeaveButtonVramTile+4,<newLeaveButtonVramTile+5
  .ends

;========================================
; we're storing extra dialogue tiles in
; the cropped-out bottom of the tilemap.
; this is fine most of the time, but
; some screen-shake effects briefly show
; this area and now need to clean out
; any garbage beforehand.
;========================================

  ;=====
  ; free up space by moving frog hop animation out of bank
  ;=====
  
  .bank 1 slot 1
  .org $2318
  .section "clear tilemap garbage on screen shake 1" overwrite
    doBankedJumpSlot2 newFrogShopAnimation
  .ends
  
  .slot 2
  .section "clear tilemap garbage on screen shake 2" superfree
    frogAnimClearTiles:
      .rept 2
        .rept bytesPerTile
          .db $00
        .endr
      .endr
    
    newFrogShopAnimation:
      ; clear garbage
/*      push hl
      push de
      push bc
        ld b,2
        ld de,frogAnimClearTiles
        ld hl,$7D40
        di
          rawTilesToVdp_macro_safe
        ei
      pop bc
      pop de
      pop hl */
      
      ; do normal work
      
      ld b,$04
      -:
        push bc
        ld hl,$9CF7
        ld ($C0A9),hl
        call $331F
        ; do a screen-shake
        call $5CE3
        call $347F
        ; hop animation
        call $331F
        ld a,$5B
        call $0EB4
        ; do a screen-shake
        call $5CE3
        call $347F
        pop bc
        djnz -
      ret 
  .ends
  
  .unbackground $6318+14 $633B

  ;=====
  ; clear screen when shake effect triggered
  ;=====
  
  .bank 1 slot 1
  .org $1CE3
  .section "clear tilemap garbage on screen shake 3" overwrite
    call clearTilemapStorageTopRow_hop
  .ends
  
  .bank 1 slot 1
  .section "clear tilemap garbage on screen shake 4" free
    clearTilemapStorageTopRow_hop:
      doBankedJumpSlot2NoParams clearTilemapStorageTopRow_hop_ext
  
  .ends
  
  .slot 2
  .section "clear tilemap garbage on screen shake 5" superfree
    newTilemapStorageRowClearTiles:
      .rept 2
        .rept bytesPerTile
          .db $00
        .endr
      .endr
    
    clearTilemapStorageTopRow_ext:
      ; clear garbage
      push hl
      push de
      push bc
        ld b,2
        ld de,newTilemapStorageRowClearTiles
        ld hl,$7D40
        di
          rawTilesToVdp_macro_safe
        ei
      pop bc
      pop de
      pop hl
      
      ret
    
    clearTilemapStorageTopRow_hop_ext:
      ; clear garbage
      call clearTilemapStorageTopRow_ext
      
      ; make up work
      ld hl,$5CF4
      ret
    
    clearTilemapStorageTopRow_landmine_ext:
      ; make up work (run script)
;      call $2688
      
      ; clear garbage
      jp clearTilemapStorageTopRow_ext
      
      ; make up work
;      ld b,$03
;      call $1C26
;      ret
  
  .ends

  ;=====
  ; clear screen for landmine effect
  ;=====
  
  .bank 1 slot 1
  .org $30C6
  .section "clear tilemap garbage on screen shake landmine 1" overwrite
    call clearTilemapStorageTopRow_landmine
  .ends
  
  .bank 0 slot 0
  .section "clear tilemap garbage on screen shake landmine 2" free
    clearTilemapStorageTopRow_landmine:
      call $1C26
      doBankedJumpSlot2NoParams clearTilemapStorageTopRow_landmine_ext
  .ends

  ;=====
  ; clear screen for gems placed effect
  ;=====
  
  .bank 2 slot 2
  .org $050F
  .section "clear tilemap garbage on screen shake gems 1" overwrite
    call clearTilemapStorageTopRow_landmine
  .ends

  ;=====
  ; clear screen for sea lion army
  ;=====
  
  .bank 2 slot 2
  .org $096E
  .section "clear tilemap garbage on screen shake sea lions 1" overwrite
    call clearTilemapStorageTopRow_landmine
  .ends

  ;=====
  ; clear screen for world destruction
  ;=====
  
  .bank 0 slot 0
  .org $2A25
  .section "clear tilemap garbage on screen shake world destruction 1" overwrite
    call clearTilemapStorageTopRow_landmine
  .ends

  ;=====
  ; clear screen for earthquake
  ;=====
  
  .bank 2 slot 2
  .org $084C
  .section "clear tilemap garbage on screen shake earthquake 1" overwrite
    call clearTilemapStorageTopRow_landmine
  .ends

;========================================
; use new title screen
;========================================

  ; new stuff
  .slot 1
  .section "new title screen components" superfree
    newTitleLogo_grp: .incbin "out/grp/title_logo.bin" FSIZE newTitleLogo_grp_size
    .define newTitleLogo_grp_numTiles newTitleLogo_grp_size/bytesPerTile
    
    newTitleComponent_grp: .incbin "out/grp/title_subcomponents.bin" FSIZE newTitleComponent_grp_size
    .define newTitleComponent_grp_numTiles newTitleComponent_grp_size/bytesPerTile
  .ends

  ; overwrite original logo tilemap
  .bank 13 slot 2
  .org $3AED
  .section "new title screen logo 1" overwrite
    .incbin "out/maps/title_logo.bin"
  .ends

  ; use new logo
  .bank 0 slot 0
  .org $0BE5
  .section "new title screen logo 2" overwrite
    ; title
    ld a,newTitleLogo_grp_numTiles
    ld b,:newTitleLogo_grp
    ld hl,newTitleLogo_grp
    ; vdp dst
    ld de,$4000
    call $3227
    
    ; components
    ld a,newTitleComponent_grp_numTiles
    ld hl,newTitleComponent_grp
    ; vdp dst
    ld de,$6000
    call $3227
  .ends

;========================================
; adjust cutscene timing
;========================================

  ;========================================
  ; halve the rate at which the cutscene text timer advances.
  ; this compensates for the decrease in the delay per cycle
  ; of the cutscene logic.
  ;========================================

  .bank 2 slot 2
  .org $2122
  .section "halve global cutscene timer 1" overwrite
    jp newTickSceneTimer
  .ends

  .bank 0 slot 0
  .section "halve global cutscene timer 2" free
    newTickSceneTimer:
      doBankedJumpNoParamsSlot2 newTickSceneTimer_ext
  .ends

  .slot 2
  .section "halve global cutscene timer 3" superfree
    newTickSceneTimer_ext:
      ; only increment the timer every other call
      ld hl,extraSceneTimer
      inc (hl)
      bit 0,(hl)
      jr nz,+
        ld a,($C0D7)
        ret
      +:
      
      ld hl,$C0D7
      inc (hl)
      ld a,(hl)
      ret
  .ends
  
  ;========================================
  ; intro
  ;========================================
  
  .bank 2 slot 2
  .org $2114
  .section "cutscenes 1" overwrite
    ; wait for vblank
    xor a
    call $0D3D
    doBankedJumpNoParamsSlot2 newTickSceneSubTimer
  .ends
  
  .slot 2
  .section "cutscenes 2" superfree
    newTickSceneSubTimer:
      ; halve counter parameter
      srl b
      ; if target counter value is odd and interpolation counter is
      ; set, add 1 to get interpolated target value
      jr nc,+
        ld hl,subsceneTimerInterpolationCounter
;        inc (hl)
        ld a,(hl)
        bit 0,a
        jr z,+
          inc b
      +:

      ; reduce counter parameter to 5/8ths
;      srl b
;      ld a,b
;      srl a
;      add a,b
;      srl b
;      srl b
;      sub b
;      ld b,a
      
      ; tick timer
      ld hl,$C0D6
      inc (hl)
      ld a,(hl)
      cp b
      ret c
      ld (hl),$00
      
      ; increment interpolation counter after each full count
      push hl
        ld hl,subsceneTimerInterpolationCounter
        inc (hl)
      pop hl
      
      ret
  .ends
  
  ;=====
  ; extend intro running section 1
  ; ("believe it or not"...)
  ;=====

  .bank 2 slot 2
  .org $1D98
  .section "intro running final length" overwrite
    ; NOTE: arle's animation phase for the second part of the running
    ; scene is not correctly reset after this part finishes.
    ; if her running animation finishes in the wrong phase, then it
    ; will be displayed incorrectly during the second part of the scene.
    ; this bug happens not to menifest with the numbers used in the original
    ; game and apparently went unnoticed for that reason.
    ; basically, if the animation is broken, change this number until it
    ; works.
    cp $5A+5
  .ends
  
  ;=====
  ; extend intro schezo appearance
  ; ("heheheh"...)
  ;=====

  .bank 2 slot 2
  .org $2100
  .section "intro schezo appearance" overwrite
    ld a,$14+22
  .ends
  
  ;=====
  ; intro 5 ("who are you")
  ;=====

;  .bank 2 slot 2
;  .org $2140
;  .section "intro 5a" overwrite
;    ld b,$0A/2
;  .ends
  
  ; delay to end of scene
  .bank 2 slot 2
  .org $2149
  .section "intro 5b" overwrite
    cp $1E+4
  .ends

  ; delay to end of text printing
  .bank 2 slot 2
  .org $214D
  .section "intro 5c" overwrite
    cp $14+4
  .ends
  
  ;=====
  ; adjust intro text positioning for lines that are supposed
  ; to be centered
  ;=====
  
  ; "whoa"
  .bank 2 slot 2
  .org $1FE7
  .section "intro text pos 9" overwrite
    ld hl,$D61A+(2*1)
  .ends
  
  ; "heheheh"
  .bank 2 slot 2
  .org $20D6
  .section "intro text pos 10" overwrite
    ld hl,$D48C+(2*0)
  .ends
  
  ; "who are you"
  .bank 2 slot 2
  .org $2134
  .section "intro text pos 8" overwrite
    ld hl,$D484+(2*(-2))
  .ends
  
  ; "i want you"
  .bank 2 slot 2
  .org $21D1
  .section "intro text pos 1" overwrite
    ld hl,$D482+(2*2)
  .ends
  
  ; "eek"
  .bank 2 slot 2
  .org $229C
  .section "intro text pos 2" overwrite
    ld hl,$D48A+(2*3)
  .ends
  
  ; "it's a pervert"
  .bank 2 slot 2
  .org $22C1
  .section "intro text pos 3" overwrite
    ld hl,$D486+(2*2)
  .ends
  
  ; "very much"
  .bank 2 slot 2
  .org $230B
  .section "intro text pos 4" overwrite
    ld hl,$D482+(2*4)
  .ends
  
  ; "!!!"
  .bank 2 slot 2
  .org $2374
  .section "intro text pos 5" overwrite
    ld hl,$D48A+(2*5)
  .ends
  
  ; "sleep"
  .bank 2 slot 2
  .org $240D
  .section "intro text pos 6" overwrite
    ld hl,$D486+(2*3)
  .ends
  
  ; "aaah"
  .bank 2 slot 2
  .org $24E5
  .section "intro text pos 7" overwrite
    ld hl,$D482+(2*4)
  .ends
  
  ;=====
  ; centering for lyra cutscene
  ;=====
  
  ; "lyra's ruins"
  .bank 2 slot 2
  .org $2951
  .section "lyra text pos 1" overwrite
    ld hl,$D610+(2*4)
  .ends
  
  ; "what's left of"
  .bank 2 slot 2
  .org $2993
  .section "lyra text pos 2" overwrite
    ld hl,$D610+(2*0)
  .ends

/*  ;=====
  ; intro 1 (running 1)
  ;=====

  .bank 2 slot 2
  .org $1D8F
  .section "intro 1" overwrite
    ld b,$05-3
  .ends

  ;=====
  ; intro 2 (school)
  ;=====

  .bank 2 slot 2
  .org $1EB6
  .section "intro 2" overwrite
    ld b,$08-4
  .ends

  ;=====
  ; intro 3 (running 2)
  ;=====

  .bank 2 slot 2
  .org $1F16
  .section "intro 3" overwrite
    ld b,$05-3
  .ends
  
  
  
  
  

  
  ;=====
  ; intro 6 ("i want you")
  ;=====

  .bank 2 slot 2
  .org $21DD
  .section "intro 6a" overwrite
    ld b,$0A/2
  .ends
  
  ;=====
  ; intro 7 ("eeek")
  ;=====

  .bank 2 slot 2
  .org $22A8
  .section "intro 7a" overwrite
    ld b,$07-3
  .ends
  
  ;=====
  ; intro 8 ("eeek")
  ;=====

  .bank 2 slot 2
  .org $22A8
  .section "intro 8a" overwrite
    ld b,$07-3
  .ends
  
  ;=====
  ; intro 9 ("very much")
  ;=====

  .bank 2 slot 2
  .org $2317
  .section "intro 9a" overwrite
    ld b,$0F/2
  .ends
  
  ;=====
  ; intro 10 ("!!!")
  ;=====

  .bank 2 slot 2
  .org $2380
  .section "intro 10a" overwrite
    ld b,$07/2
  .ends
  
  ;=====
  ; intro 11 ("sleep")
  ;=====

  .bank 2 slot 2
  .org $2419
  .section "intro 11a" overwrite
    ld b,$0F/2
  .ends
  
  ;=====
  ; intro 12 ("sleep")
  ;=====

  .bank 2 slot 2
  .org $24F1
  .section "intro 12a" overwrite
    ld b,$07/2
  .ends */
  
  ;=====
  ; intro dungeon scenes
  ;=====

  .bank 2 slot 2
  .org $2713
  .section "intro dungeon scene timers" overwrite
    ; byte 1 = timer
    ; byte 2 = number of characters (ignored in hack)
    
    ; "ow"
    .db $20,$16
    ; "what was up with that mage"
    .db $17+4,$0D
    ; "he can't get away with it"
    .db $1C+16,$12
    ; "ugh"
    .db $1F,$15
    ; "i'll use"
    .db $1F,$15
    ; "uhmm"
    .db $1A,$10
    ; "gasp"
    .db $20,$16
    ; "oh come on"
    .db $1A,$10
    ; "i lost"
    .db $28,$12
  .ends
  
  ;=====
  ; intro: fix dungeon scene scrolling
  ;=====

  ; subtimer
  .bank 2 slot 2
  .org $274F
  .section "intro dungeon scroll 1" overwrite
    ld b,$07*2
  .ends

  ; timer
  .bank 2 slot 2
  .org $2758
  .section "intro dungeon scroll 2" overwrite
    cp $38/2
  .ends
  
  ;=====
  ; intro: longer display of "that monster's standing watch"
  ;=====

  .bank 2 slot 2
  .org $279A
  .section "intro dungeon 'standing watch'" overwrite
    cp $58+8
  .ends
  
  ;=====
  ; ruins entrance cutscene: fix double-rope glitch
  ;=====

  .bank 2 slot 2
  .org $2B66
  .section "ruins cutscene fix 1" overwrite
    doBankedCallSlot2NoParams ruinsRope_ext
    nop
  .ends

  .slot 2
  .section "ruins cutscene fix 2" superfree
    ruinsRope_ext:
      ; increment scene timer.
      ; the game already did this, expecting it to tick from 0x36
      ; to 0x37, but due to the half-rate hack it remained at 0x36.
      ; if not corrected, this will cause arle's second rappeling
      ; animation to play again after the rope frays.
      ld hl,$C0D7
      inc (hl)
      
      ; make up work
      ld a,$40
      ld ($C0D8),a
      xor a
      ld ($C0D9),a
      ret
  .ends

;========================================
; credits
;========================================
  
  ;=====
  ; use standard char-by-char printing
  ;=====

  .bank 0 slot 0
  .org $2BEB
  .section "credits cbc 1" overwrite
    @loop:
      ; print until CBC mode shut off
      ld a,(vwfCbcActiveFlag)
      or a
      jr z,@done
      
      ; print character
      call $2B64
      
      ; wait 5 frames
      ld a,$0A/2
      call waitVblank
      jr @loop
      
    @done:
    jp $2C39
  .ends

  ;=====
  ; extend credits
  ;=====

  .bank 2 slot 2
  .org $3930
  .section "credits 1" overwrite
    doBankedCallSlot2 newCredits_ext
    jp $B966
  .ends

  .slot 2
  .section "credits 2" superfree
    newCredits_ext:
      ; original credits
/*      call $2BBE
      call $2BBE
      call $2BC1
      call $2BC8
      call $2BBE
      call $2BBE
      call $2BBE
      call $2BC1
      call $2BC8
      call $2BBE
      call $2BC1
      call $2BC4
      call $2BC4
      call $2BC4
      call $2BC8
      call $2BC1
      call $2BC4
      call $2BC8 */
      
      ; the original game doesn't reprint repeated job titles,
      ; which is why it has calls to various routines other than
      ; the one used here.
      ; not worth the effort to reproduce with the new system.
      call $2BBE
      call $2BBE
      call $2BBE
      call $2BBE
      call $2BBE
      call $2BBE
      call $2BBE
      call $2BBE
      call $2BBE
      call $2BBE
      call $2BBE
      call $2BBE
      call $2BBE
      call $2BBE
      call $2BBE
      call $2BBE
      call $2BBE
      call $2BBE
      
      ; new translation credits, starting from script 4-7F
      ld c,$7F
      call $2BBE
      call $2BBE
      call $2BBE
      call $2BBE
      call $2BBE
      call $2BBE
      call $2BBE
      
      ret
  .ends
  
  ;=====
  ; fix final copyright notice
  ;=====

  .bank 2 slot 2
  .org $3989
  .section "credits end 1" overwrite
    ; script ID
    ld c,$7E
    ; target address within buffer
    ld hl,$D480+(2*6)
    ; run script
    call $267D
    
    ; count of characters
    ld b,$0F*2
  .ends
  
  ;=====
  ; arle walking animation speed is directly tied to scene timer.
  ; halve to match
  ;=====

  .bank 2 slot 2
  .org $36CF
  .section "ending walk fix 1" overwrite
    call creditsWalkFix
  .ends

  .bank 2 slot 2
  .section "ending walk fix 2" free
    creditsWalkFix:
      ; only update walk animation every other sceneTimer tick
      bit 0,a
      jr z,+
        call $B793
      +:
      ret
  .ends
  
  ;=====
  ; extend "it's probably nothing important" line
  ;=====
  
  ; extra ticks to display the line
  .define endingExtendedLineExtraTime $0C

  .bank 2 slot 2
  .org $38BA
  .section "ending extend line 1" overwrite
    cp $46+endingExtendedLineExtraTime
  .ends

  .bank 2 slot 2
  .org $38C2
  .section "ending extend line 2" overwrite
    cp $28+endingExtendedLineExtraTime
  .ends

  .bank 2 slot 2
  .org $38CB
  .section "ending extend line 3" overwrite
    cp $11+endingExtendedLineExtraTime
  .ends
  
  
  
  