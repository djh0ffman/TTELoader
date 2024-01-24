
                 incdir     include
                 include    hw.i


SEEK_ZERO      = 0                                            ; 0 - assume track number has been set correctly / seek to zero then to track

CIAA           = $bfe001
CIAB           = $bfe101

CIAPRA       EQU $0000
CIAPRB       EQU $0100
CIADDRA      EQU $0200
CIADDRB      EQU $0300
CIATALO      EQU $0400
CIATAHI      EQU $0500
CIATBLO      EQU $0600
CIATBHI      EQU $0700
CIATODLOW    EQU $0800
CIATODMID    EQU $0900
CIATODHI     EQU $0A00
CIASDR       EQU $0C00
CIAICR       EQU $0D00
CIACRA       EQU $0E00
CIACRB       EQU $0F00

; from memory.asm


; Port A bits (input)
DSKCHANGE    equ 2
DSKPROT      equ 3
DSKTRACK0    equ 4
DSKRDY       equ 5

; Port B bits (output)
DSKSTEP      equ 0
DSKDIREC     equ 1
DSKSIDE      equ 2
DSKSEL0      equ 3
DSKMOTOR     equ 7

; constants
NUM_TRACKS   equ 160
SECT_PER_TRK equ 11
STEPDELAY    equ 64                                           ; (* 64us) ca. 4ms  (minimum: 3ms)
SETTLEDELAY  equ 400                                          ; (* 64us) ca. 25ms (minimum: 18ms)

MFM_BUFSIZE  equ $1a00*2
MFM_READLEN  equ $19f0                                        ; in words
MFM_WRITELEN equ $1900                                        ; in words
MFM_GAPSIZE  equ $f0                                          ; gap in words at track-start and end

NUM_RETRIES  equ 4                                            ; retries on read error
DMA_TIMEOUT  equ 2000                                         ; timeout after 2 seconds


                ;near       a4

                 ;section    trackdisk,code


; a0 = track data $1600 bytes
; a1 = mfm buffer

TrackSave:
                 movem.l    d1-d7/a0-a6,-(sp)
                 lea        TrackVars(pc),a4
                 move.l     a0,DataPointer(a4)
                 move.l     a1,MFMbuffer(a4)

                 lea        $dff000,a6
                 lea        $BFE001,a2
                 lea        $BFD100,a3
                   
                 move.w     #$4000,DSKLEN(a6)
                 move.w     #$8210,DMACON(a6)
                 move.w     #$1002,INTENA(a6)

                  ; motor on
                   ;bsr        td_motoron
                   
                 moveq      #$7d,d1
                 move.b     d1,(a3)
                 bclr       #3,d1
                 move.b     d1,(a3)

                 moveq      #100-1,d2                         ; timeout after ~500ms
.waitdrive:      moveq      #78,d0
                 bsr        td_delay                          ; ~5ms
                 btst       #DSKRDY,(a2)
                 dbeq       d2,.waitdrive

                 IF         SEEK_ZERO=1
                 bsr        td_seekzero
                 ENDIF

                 move.w     #SCORE_TRACK,d0
                 bsr        td_seek

                 bsr        td_trackwrite

                 or.b       #$f8,(a3)
                 nop
                 and.b      #$87,(a3)
                 nop
                 or.b       #$78,(a3)                         ; deselect all

                 movem.l    (sp)+,d1-d7/a0-a6
                 moveq      #0,d0
                 rts


;---------------------------------------------------------------------------
td_trackwrite:
; Construct a valid AmigaDOS track in the MFM buffer from the sectors
; in the given track buffer. Add a gap at the start and at the end of
; the buffer, which will overlap when writing the track to disk using DMA.
; The head is expected to be positioned at the correct track to write
; (usually by a previous read operation).
; a0 = track buffer (SECT_PER_TRACK * 512 bytes)
; -> d0/Z = error code (0=ok, 1=timeout, 5=writeProtect)
                 move.l     DataPointer(a4),a2
                 move.l     MFMbuffer(a4),a0
                 move.l     #$55555555,d3                     ; MFM mask

	;----------------------------------------
	; construct an AmigaDOS format MFM track
	;----------------------------------------

	; write gap at the beginning of the track
                 bsr        write_gap

	; constant part of the info field: $ff, track.b, 0, 0
                 moveq      #0,d6
                 subq.w     #1,d6
                 move.b     CurrentTrk+1(a4),d6
                 swap       d6

                 moveq      #SECT_PER_TRK-1,d5

.sect_loop:
	; sector starts with two 0-bytes followed by two SYNCs
                 bsr        write_nulls
                 move.l     #$44894489,(a0)+

	; Write sector number and number of sectors until the gap into
	; the info field. Layout:
	; 0: $ff
	; 1: track number
	; 2: sector number
	; 3: number of sectors until gap
                 moveq      #SECT_PER_TRK-1,d0
                 sub.w      d5,d0
                 lsl.w      #8,d0
                 move.b     d5,d0
                 addq.b     #1,d0
                 or.l       d6,d0
                 bsr        encodeLong

	; Fill the sector label field with zeros.
                 moveq      #7,d4
.1:              bsr        write_nulls
                 dbf        d4,.1

	; skip the checksum fields for now
                 lea        16(a0),a0

	; encode the data block
                 lea        512(a0),a1
                 moveq      #127,d4
.2:              move.l     (a2)+,d7                          ; data longword to encode into MFM
                 move.l     d7,d0
                 lsr.l      #1,d0
                 bsr        encodeEven                        ; encode odd bits
                 move.l     d0,(a0)+
                 exg        a0,a1
                 move.l     d7,d0
                 bsr        encodeEven                        ; encode even bits
                 move.l     d0,(a0)+
                 exg        a0,a1
                 dbf        d4,.2
                 bsr        checkBorder

	; calculate checksums
                 lea        -568(a0),a0                       ; back to header start
                 bsr        dataChksum
                 move.l     d0,a1
                 bsr        headerChksum

	; insert checksums
                 lea        40(a0),a0                         ; checksum fields in the header
                 bsr        encodeLong                        ; write header checksum
                 move.l     a1,d0
                 bsr        encodeLong                        ; write data checksum
                 bsr        checkBorder

                 lea        1024(a0),a0                       ; skip data
                 dbf        d5,.sect_loop                     ; next sector

	; write gap at the end of the track
                 bsr        write_gap

	;-----------------------------------
	; write the track to disk using DMA
	;-----------------------------------

                   ;lea        CIAA+CIAPRA,a2
                   ;lea        CIAB+CIAPRB,a3
                 lea        $BFE001,a2
                   ;lea        $BFD100,a3
	; check if disk is write-protected
                 btst       #DSKPROT,(a2)
                 beq        .err_wprot

                 move.w     #$4000,DSKLEN(a6)

	; set MFM precompensation, disable WORDSYNC
                 move.w     #$6600,ADKCON(a6)
                 move.w     #$9100,ADKCON(a6)

	; select 140ns precompensation for cylinders 40-79
                 cmp.w      #NUM_TRACKS/2,CurrentTrk(a4)
                 blo        .3
                 move.w     #$a000,ADKCON(a6)                 ; set PRECOMP0 (140ns)

	; start disk-write DMA
.3:              move.l     MFMbuffer(a4),DSKPT(a6)
                 move.w     #$c000|MFM_WRITELEN,DSKLEN(a6)
                 move.w     #$c000|MFM_WRITELEN,DSKLEN(a6)

                 bsr        wait_disk_dma
                 bmi        .err_timeout

                 moveq      #0,d0                             ; ok, no error
                 bra        .done
.err_timeout:
                 moveq      #1,d0                             ; timeout while waiting for disk-DMA
                 bra        .done
.err_wprot:
                 moveq      #5,d0                             ; disk is write-protected

.done:
                 rts


;---------------------------------------------------------------------------
write_nulls:
; Write two MFM-encoded 0-bytes.
; a0 = MFM destination pointer
; -> a0 = new MFM pointer (+4)

                 btst       #0,-1(a0)
                 beq        .1
                 move.l     #$2aaaaaaa,(a0)+
                 rts
.1:              move.l     #$aaaaaaaa,(a0)+
                 rts


;---------------------------------------------------------------------------
write_gap:
; Write MFM_GAPSIZE words of $aaaa as gap.
; a0 = MFM destination pointer
; d3 = MFM data mask
; -> a0 = new MFM pointer

                 not.l      d3
                 moveq      #MFM_GAPSIZE/2-1,d0
.1:              move.l     d3,(a0)+
                 dbf        d0,.1
                 not.l      d3
                 rts


;---------------------------------------------------------------------------
encodeEven:
; Encode even bits of a longword into MFM, regarding the right border
; of the previous MFM word in memory.
; d0 = even bits of this longworld will be encoded
; d3 = MFM data mask
; a0 = MFM destination pointer for this word
; -> d0 = MFM result
; Register a1 is preserved and d2 will be trashed!

                 and.l      d3,d0
                 move.l     d0,d2
                 eor.l      d3,d2
                 move.l     d2,d1
                 add.l      d2,d2
                 lsr.l      #1,d1
                 bset       #31,d1
                 and.l      d2,d1
                 or.l       d1,d0
                 btst       #0,-1(a0)
                 beq        .1
                 bclr       #31,d0
.1:              rts


;---------------------------------------------------------------------------
encodeLong:
; Encode a longword as MFM and write it to the buffer, including border
; checks for the previous and the next word.
; d0 = longword to encode
; a0 = MFM destination pointer
; -> a0 = new MFM pointer (+8)
; Register a1 is preserved and d2 will be trashed!

                 move.l     d0,-(sp)
                 lsr.l      #1,d0
                 bsr        encodeEven
                 move.l     d0,(a0)+
                 move.l     (sp)+,d0
                 bsr        encodeEven
                 move.l     d0,(a0)+
                 rts


;---------------------------------------------------------------------------
checkBorder:
; Fix the right MFM word's border bit.
; a0 = pointer to the MFM word on the right border

                 move.b     (a0),d0
                 btst       #0,-1(a0)
                 bne        .1
                 btst       #6,d0
                 bne        .2
                 or.b       #$80,d0                           ; set the right border bit
                 move.b     d0,(a0)
                 rts
.1:              and.b      #$7f,d0                           ; clear the right border bit
                 move.b     d0,(a0)
.2:              rts


;---------------------------------------------------------------------------
headerChksum:
; Calculate sector header checksum.
; a0 = Start of sector header in MFM buffer
; d3 = MFM mask
; -> d0 = checksum
; a0/a1 are preserved!

                 movem.l    d2/a0,-(sp)
                 moveq      #9,d2
                 bra        mfmchksum


;---------------------------------------------------------------------------
dataChksum:
; Calculate data block checksum.
; a0 = Start of sector header in MFM buffer
; d3 = MFM mask
; -> d0 = checksum
; a0/a1 are preserved!

                 movem.l    d2/a0,-(sp)
                 lea        56(a0),a0                         ; start of the data block
                 move.w     #255,d2

mfmchksum:
                 moveq      #0,d0
.1:              move.l     (a0)+,d1
                 eor.l      d1,d0
                 dbf        d2,.1

                 and.l      d3,d0
                 movem.l    (sp)+,d2/a0
                 rts


;---------------------------------------------------------------------------
wait_disk_dma:
; Wait until DMA transfer is finished. Reset DSKLEN.
; -> d0/N = timeout
; a0 and a1 are preserved, d2 is trashed!

                 move.w     #$0002,INTREQ(a6)
                 move.w     #DMA_TIMEOUT,d2

.1:              moveq      #16,d0
                 bsr        td_delay                          ; ~1ms
                 moveq      #2,d0
                 and.w      INTREQR(a6),d0
                 dbne       d2,.1

                 move.w     #$4000,DSKLEN(a6)
                 move.w     #$0002,INTREQ(a6)

                 tst.w      d2
                 rts



;---------------------------------------------------------------------------
td_seek:
; Step to the required cylinder and select the correct head.
; d0.w = track to seek
; a2 = CIAAPRA
; a3 = CIABPRB

                 cmp.w      #NUM_TRACKS,d0
                 bhs        .exit                             ; illegal track

                 movem.l    d2-d3,-(sp)
                 move.w     CurrentTrk(a4),d3
                 move.w     d0,d2
                 btst       #0,d2
                 bne        .1

	; select lower head
                 bset       #DSKSIDE,(a3)
                 bclr       #0,d3
                 bra        .2

	; select upper head
.1:              bclr       #DSKSIDE,(a3)
                 bset       #0,d3

.2:              cmp.w      d3,d2
                 beq        .done
                 bhi        .3

	; step outwards
                 bset       #DSKDIREC,(a3)
                 subq.w     #2,d3
                 bra        .4

.3:	; step inwards
                 bclr       #DSKDIREC,(a3)
                 addq.w     #2,d3

.4:              bsr        td_step
                 bra        .2

.done:
                 move.w     d2,CurrentTrk(a4)

                 move.w     #SETTLEDELAY,d0
                 bsr        td_delay

                 movem.l    (sp)+,d2-d3
.exit:
                 rts

                 IF         SEEK_ZERO=1
td_seekzero:
; Turn motor on. Seek track 0, reset CurrentTrk to 0.
; a2 = CIAAPRA
; a3 = CIABPRB
; -> d0/Z = error code (0=ok, 1=unformatted, 2=missingSectors, 3=badHeader)

                 bset       #DSKSIDE,(a3)                     ; select lower head: track 0
                 bset       #DSKDIREC,(a3)                    ; step outwards

.1:              moveq      #STEPDELAY,d0
                 bsr        td_delay                          ; wait until TRACK0 signal is valid

                 btst       #DSKTRACK0,(a2)
                 beq        .2

                 bsr        td_step
                 bra        .1

	; head is positioned over track 0 now; read it
.2:              clr.w      CurrentTrk(a4)
                 rts
                 ENDIF

;---------------------------------------------------------------------------
td_step:
; Step a track into selected direction.
; a2 = CIAAPRA
; a3 = CIABPRB

                 moveq      #STEPDELAY,d0
                 bsr        td_delay
                 bclr       #DSKSTEP,(a3)
                 nop
                 bset       #DSKSTEP,(a3)
                 rts


;---------------------------------------------------------------------------
td_delay:
; Wait for ca. count * 64us.
; d0.w = count
; a0 and a1 are preserved!

.1:              move.b     VHPOSR(a6),d1
.2:              cmp.b      VHPOSR(a6),d1
                 beq        .2
                 subq.w     #1,d0
                 bne        .1
                 rts

                 RSRESET
MFMbuffer:       rs.l       1                                 ;ChipMFMBuffer                      ; buffer for raw MFM track data
DataPointer:     rs.l       1
CurrentTrk:      rs.w       1                                 ; current track in MFM buffer (-1 = none)
TrackVars_Size:  rs.b       0



TrackVars:       dcb.b      TrackVars_Size,0
                 even
                 dc.b       "END!"


