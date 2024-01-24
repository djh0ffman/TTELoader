;-----------------------------------------------------------
;
; TTE Cache Loader
;
; Coded by h0ffman
; uses 4489 byte loader and zx0 packer
;
; To use caching, apply the min / max ram range in the 
; two longwords at "ExtraRam" 
;
; if the file on the disk is marked as cacheable, the file
; is loaded into the cache memory and unpacked to the destination
; on subsequent loads, it will just unpack the file
;
;-----------------------------------------------------------

SCORE_TRACK   = (80*2)-1
SCORE_BYTE    = SCORE_TRACK*$1600
SCORE_SIZE    = $168
SCORE_ADDRESS = $575c

TestMode      = 0                           ; 0 builds just loader / 1 builds with test rig
FileCount     = 25                          ; total files on disk
FileTableSize = FileCount*4*4               ; file table byte size

;-----------------------------------------------------------
;
; test rig code
;
;-----------------------------------------------------------

  section    loader,code_c

  IF         TestMode=1
Tester:
  ;lea        RamBuffer,a0
  ;move.l     a0,ExtraRam
  ;add.l      #$80000,a0
  ;move.l     a0,ExtraRam+4

  move.w     #$7fff,$dff096
  move.w     #$7fff,$dff09a

  moveq      #$a,d0
  move.l     #TestArea,d2
  jsr        RtypeLoader

  lea        TestArea,a0
  move.w     #$ffff,d7
.clear
  clr.l      (a0)+
  dbra       d7,.clear

  moveq      #$a,d0
  move.l     #TestArea,d2
  jsr        RtypeLoader


  lea        TestArea,a0
  nop
.stop
  bra        .stop

  ENDIF


;-----------------------------------------------------------
;
; Loader
;
;-----------------------------------------------------------

jumps:

  bra.w      RtypeLoader
  bra.w      HiscoreLoader
  bra.w      HiscoreSaver

ExtraRam:
  dc.l       0                              ; current position
  dc.l       0                              ; maximum address

ScoreTest: 
  dc.l       0 

HiscoreLoader:
  move.l     #SCORE_BYTE,d0
  moveq      #4,d1
  moveq      #0,d2
  lea        ScoreTest(pc),a0
  lea        MFMBuffer(pc),a1
  bsr        LOADER
  tst.l      d0
  bne        .skip

  tst.l      (a0)
  beq        .skip

  move.l     #SCORE_BYTE,d0
  lea        SCORE_ADDRESS,a0
  move.l     #SCORE_SIZE,d1
  bsr        LOADER
.skip
  rts

HiscoreSaver:
  move.w     LoaderTrack(pc),d0
  lea        TrackVars+CurrentTrk(pc),a0
  move.w     d0,(a0)

  lea        SCORE_ADDRESS,a0
  lea        MFMBuffer(pc),a1
  bsr        TrackSave
  lea        LoaderTrack(pc),a0
  move.w     #SCORE_TRACK,(a0)
  rts

; d0 - filename long
; d1 - load address

RtypeLoader:
  movem.l    d0-d7/a0-a6,-(sp)

  IF         TestMode=1
  lea        MFMBuffer,a4
  ELSE
  lea        MFMBuffer(pc),a4
  ENDIF

  move.l     d2,d6                          ; backup load address

  and.w      #$ff,d0                        ; convert track number to file id
  lea        HEX(pc),a1
  moveq      #4-1,d7
.hexloop	
  rol.w      #4,d0
  lsl.l      #8,d5
  move.w     d0,d1
  and.w      #$f,d1
  move.b     (a1,d1.w),d5                   ; file id
  dbra       d7,.hexloop

  lea        FileTable(pc),a0
  tst.l      (a0)
  bne        .tableok

  movem.l    d0/d2,-(sp)

  move.l     a4,a1                          ; MFM Buffer
  move.l     #$400,d0
  move.l     #FileTableSize,d1
  moveq      #0,d2
  bsr        LOADER
  tst.l      d0
  bne        LoadError
  movem.l    (sp)+,d0/d2
.tableok
  moveq      #FileCount-1,d7
.search
  cmp.l      (a0)+,d5
  beq        .found
  lea        3*4(a0),a0
  dbra       d7,.search
  bra        LoadError
.found
  move.l     (a0)+,d0                       ; disk position
  move.l     (a0)+,d1                       ; length of data to load
  move.l     (a0)+,d3                       ; unpacked size

  move.l     d1,d4                          ; is it cacheable
  and.l      #$ffffff,d1                    ; remove any config bits from length param

  lea        ExtraRam(pc),a5
  move.l     (a5),d2
  beq        .standardload                  ; no extra ram

  rol.l      #8,d4
  btst       #0,d4
  beq        .standardload                  ; not cacheable

  btst       #1,d4
  beq        .cacheload                     ; perform a load into the cache

  move.l     d0,a0
  move.l     d6,a1
  bsr        zx0_decompress
  bra        .exit

	
.cacheload
  move.l     4(a5),d4
  add.l      d1,d2
  cmp.l      d4,d2
  bcc        .standardload                  ; no free ram

  move.l     (a5),d5                        ; new load address
  move.l     d2,(a5)                        ; store next address

  or.l       #1<<25,-(4*2)(a0)              ; set cached flag
  move.l     d5,-(4*3)(a0)                  ; set ram position of cached file

  move.l     d5,a0                          ; load address
  move.l     a4,a1                          ; mfm location
  moveq      #0,d2                          ; drive 0
  bsr        LOADER
  tst.l      d0
  bne        LoadError

  move.l     d5,a0
  move.l     d6,a1
  bsr        zx0_decompress
  bra        .exit

.standardload
  move.l     d6,a0                          ; load address
  move.l     a4,a1                          ; MFM Buffer
  moveq      #0,d2                          ; drive 0
  bsr        LOADER
  tst.l      d0
  bne        LoadError

  move.l     a0,a1
  add.l      d3,a0                          ; end of unpacked area
  move.l     (a0)+,-(sp)                    ; backup the 8 bytes from safe area
  move.l     (a0)+,-(sp)                    ; a0 = end of packed data dest
  move.l     a0,a5                          ; save this for later
  add.l      d1,a1                          ; a1 = end of packed data source

.move				; move the file up after loading (because mfm and unpack overlap!)
  move.b     -(a1),-(a0)		
  subq.l     #1,d1
  bne        .move                          ; end of this a0 should contain packed data
	
  move.l     d6,a1                          ; destination

  movem.l    d5/d6/a5,-(sp)                 ; save file name and destination
  bsr        zx0_decompress
  movem.l    (sp)+,d5/d6/a5

  move.l     (sp)+,-(a5)                    ; restore 8 bytes from safety area
  move.l     (sp)+,-(a5)


.exit
  movem.l    (sp)+,d0-d7/a0-a6
  moveq      #0,d0
  rts


HEX:
  dc.b       "0123456789ABCDEF"


LoadError:
  move.w     #$f00,$dff180
  move.w     #$000,$dff180
  bra        LoadError

	
  include    "newloader.asm"
  include    "unzx0_68000.s"
  include    "trackdisk.asm"

FileTable:
  dcb.b      FileTableSize

  dc.b       "END!"


;-----------------------------------------------------------
;
; Ram testing areas
;
;-----------------------------------------------------------


  IF         TestMode=0
MFMBuffer:
  ELSE

  section    diskbuffer,bss_c
MFMBuffer:
  ds.w       $2000

  section    testarea,bss
TestArea:
  ds.b       $80000

  section    rambuffer,bss
RamBuffer:
  ds.b       $80000

  ENDIF

