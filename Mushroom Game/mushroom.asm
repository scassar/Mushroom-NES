; iNES Header
;    .inesprg 1   ; 1x 16KB bank of PRG code
;    .ineschr 1   ; 1x 8KB bank of CHR data
;    .inesmap 0   ; mapper 0 = NROM, no bank swapping
;    .inesmir 1   ; background mirroring (ignore for now)
;     I have put this comment to verify if sync worked with icloud  :)


; $0000-0800 - Internal RAM, 2KB chip in the NES
; $2000-2007 - PPU access ports
; $4000-4017 - Audio and controller access ports
; $6000-7FFF - Optional WRAM inside the game cart
; $8000-FFFF - Game cart ROM    

.segment "HEADER"
    .byte "NES" 
    .byte $1A 
    .byte $02 ;amount of PRG ROM space
    .byte $01 ;
    .byte $00
    .byte $00,$00,$00,$00    ;unused for this project (maybe google what this is used for)
    .byte $00,$00,$00,$00,$00

.segment "ZEROPAGE"
VAR: .RES 1     ;reserve 1 bit of memory
playerx:  .RES 1
playery:  .RES 1
playervy: .RES 1 ;user for the velocity of the player. (Negative is up)
gravity:   .RES 1 ; gravity constant
controller1: .RES 1
playeronground:  .RES 1
tmp: .RES 1
inair: .RES 1
.segment "STARTUP"    ;These will run on startup


RESET:    ; What we do when the system starts, and gets reset
    SEI     ;Disables interupts (6502 CPU command)
    CLD     ;Disables decimal mode


        LDX #$40     ;%01000000 for sound disable
        STX $4017    ;Address for the sound
        LDX #$00     ;reset x register
        STX $4010    ;disable PCM

        ; initalise the stack register
        LDX #$FF
        TXS 

    ;Clear PPU REgisters
    LDX #$00
    STX $2000
    STX $2001

    ;SET VARIABLES. We want to get the mushroom moving to the right. We want to move the X / Y position each time the screen refreshes. 

    ;Wait for Vblank

:
    BIT $2002       ;Test the address (PPU Vblank line) against this to be 1. Once the flag is set 
    BPL :-          ;Keep looping if we are in VLANK (PPU is currently rendering)

    ;CLEARING PPU memory
    TXA 

CLEARMEMORY:     ;Sets the first bytes. ;0000 - 07FF    (2k bytes). This is clearing the 2k memory that the CPU had access to. Looping multiple time swiping out the rages
    STA $0000, X
    STA $0100, X
    STA $0300, X
    STA $0400, X
    STA $0500, X
    STA $0600, X
    STA $0700, X      ; What this does is store the value of the Accumulator to $0000, but then offset by x (which is 0). As this loops it will continue to offset by X and fo 0x00, 0x01 etc.
        LDA #$FF
        STA $0200, X
        LDA #$00

    INX         ;x + 1
    CPX #$00     ;Compare X to 0, this is because X will wrap at 255 (since its an 8 bit register)
    BNE CLEARMEMORY


:
    BIT $2002       ;Test the address (PPU Vblank line) against this to be 1. Once the flag is set 
    BPL :-     

    ;Setting sprite range - Tell the PPU, what section OF CPU ram the sprites are at. 

    LDA #$02     ;Setting the significant byte (the lower byte is going to be 00 through to this)
    STA $4014    ;Talks to the PPU. Take everything from this range of $02XX to $02FF to draw sprites. You only need to give the higher byte address (we tell it to use $02 as top, so 00 - FF will be 256 bytes or 11111111 in binary)
                 ; After this, the PPU will think to look for the sprites at address 0200 hex or from 0200 to 02FF    (255 bytes)
                ; Tell the PPU to take all this data and mark it for use with the sprites
    NOP

      
    
    ; Load Pallete data 
    LDA #$3F     ;Address to tell the PPU where to look in memory for pallete data. Significant byte.
    STA $2006    ;Address where the PPU is told. 
    LDA $00     ;Lower byte of the address
    STA $2006

    LDX #$00


LOADPALETTES:     
    LDA PALETTEDATA, X     ;Basically, set X Register to 0 for counting again. Load the first byte value from memory address starting with the PALETTE DATA Label (we dont care where this is because its in our code)
    STA $2007             ;Store this against the PPU for taking in pallete data. The PPU will incremement when we read / write to the PPU (so it will consume all data )
    INX 
    CPX #$20
    BNE LOADPALETTES

    LDX #$00

LOADSPRITES:    
    LDA PLAYERSPRITE, X
    STA $0200, X 
    INX 
    CPX #$10 ;(16 times) 4 bytes per sprite, 4 sprites in total. Push thes in a loop into the PPU and it will increment by itself
    BNE LOADSPRITES

    ;Load background

LOADBACKGROUND: 
    LDA $2002    ;Read PPU status to reset high/low latch
    LDA #$20      ;Set the high byte for where to start placing the background data. This correlates with where on the screen the address is for the tile 
    STA $2006     ;Set this on the PPU
    LDA #$00      ;Set the low byte where to start generating background data 
    STA $2006     ;Set this 
    LDX #$00      ;reset counter

;      +----------------+-----------------+
;      I                I                 I
;      I   $2000        I     $2400       I
;      I                I                 I
;      I                I                 I
;      I IIIIIIIIIIIII  I IIIIIIIIIIIII   I
;      I                I                 I
;      I  $2800         I      $2C00      I
;      I                I                 I
;      I                I                 I
;      +----------------+-----------------+


; HERE we are loading 256 * 4 to load up the entire 1024kg of backkground data in the PPU. The trailing 64 bytes are the specification of each of the tiles selected colours. 
LOADBACKGROUNDP1:      ;We have to load in 2 loops because we can only dealing with 8 bits mean we can only loop to 256 and the counter will stop. We can solve this with a second iteration and keep loading the data
    LDA BACKGROUNDDATA, X
    STA $2007       ;Load all the background data into the PPU until its done
    INX 
    CPX #$00               ;Sets the equal flag once matching 00 in the accumulator
    BNE LOADBACKGROUNDP1   ;Branch not equal flag being set

LOADBACKGROUNDP2: 
    LDA BACKGROUNDDATA+256, X
    STA $2007
    INX 
    CPX #$00               ;Sets the equal flag once matching 00 in the accumulator
    BNE LOADBACKGROUNDP2
LOADBACKGROUNDP3: 
    LDA BACKGROUNDDATA+512, X
    STA $2007
    INX 
    CPX #$00               ;Sets the equal flag once matching 00 in the accumulator
    BNE LOADBACKGROUNDP3
LOADBACKGROUNDP4: 
    LDA BACKGROUNDDATA+768, X
    STA $2007
    INX 
    CPX #$00                ;Branch when we get to 256 - 16 = 240 bytes
    BNE LOADBACKGROUNDP4

;LOADBACKGROUNDPALETTEDATA
	LDA #$23	;$22D0    AGainm high byte then low byte
	STA $2006
	LDA #$D0
	STA $2006
	LDX #$00

LOADBACKGROUNDPALETTEDATA:;
	LDA BACKGROUNDPALETTEDATA, X
	STA $2007
	INX
	CPX #$20
	BNE LOADBACKGROUNDPALETTEDATA


INITALISESTATE:    ;Where we will be starting. This is where we inialise the X and Y. This will override whatever value we put in the sprite rom anyway
  LDA #$60   ; player x offset
  STA playerx

  LDA #$90   ; player y offset
  STA playery

  LDA #$01 
  STA inair

  LDA #$00
  STA playervy

  LDA #$02
  STA gravity

  LDA #$00
  STA playeronground

 ;RESET SCROLL
    LDA #$00
    STA $2005
    STA $2005
 

;ENABLE INTERUPS
    CLI

    LDA #%10010000        ;Geneate NMI when Vblank occurs. The first 1 in this byte does this. The second byte says use the 2nd section of the sprities for the background
    STA $2000               ;WHEN BLANK OCCURS CALLS NMI

    LDA #%00011110      ;Show sprites
    STA $2001


    INFLOOP:     ; The point of this is that it stops the rom from closing. Everything will be handled by interupts so this is just needed to keep things going
        
        JMP INFLOOP

NMI:      ; First Vector location and will be what is called by the NES when vblank is ready. We just need to finish all the updates before the PPU runs its next generation cycle to the screen. 

    LDA #$00   ; load 1 bytes of 0 in A   //Done every frame
    STA $2003  ; set the low byte (00) of the RAM address
    LDA #$02   ; load 1 byte of $02, 
    STA $4014  ; set the high byte (02) of the RAM address, start the transfer

    JSR DRAW
    JSR UPDATE

    RTI

DRAW: 

 JSR DrawPlayer

  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000
  LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  STA $2001
  LDA #$00        ;;tell the ppu there is no background scrolling
  STA $2005
  STA $2005


DrawPlayer:      ; So we can move the sprint by manipulating the space in memory that the location is stored at. The starting locations were bytes stored and run as part of load_sprites. When we load sprites we point to a running array of labels, thats how the PPU gets them.
                ; Reading straight from memory. But the values to be used are the actual number. We are just looping here now and moving right

  LDX #$00              ; start at 0 
  LDY #$00              ; start at 0

  ; We want to loop 4 times so we can render all elements of the sprite
DrawPlayerLoop:
  
  LDA $0203, x          ; load current x sprite position (2nd loop we check $0203 + 4 = $0207 (sprite 2 x location))
  CLC  
  LDA playerspriteoffset, y    ; add player x with sprite offset
  INY
  ADC playerx                  ;move the sprite    playerx = new location. its the stored location + controller = not a movement, but a static new value. 
  STA $0203, x          ; store into RAM address ($0203 + x)

  LDA $0200, x          ; load current y sprite position
  CLC
  LDA playerspriteoffset, y   ; add player y with sprite offset
  ADC playery
  INY
  STA $0200,x
  
  INX                   ; increment sprite offset coutner
  INX
  INX
  INX
  CPX #$20              ;We have done 16 loops (for all 4 sprites) Compare X 
  BNE DrawPlayerLoop

  RTS
    ;END drawPlayer

;UPDATE:
UPDATE: 

JSR LatchController
JSR PollController
JSR ReadLeft
JSR ReadRight
JSR ReadUp
JSR ReadDown

LDA inair
CLC
CMP #$01
CLC
BEQ SkipReadA     ;IF the play is nto on the ground, dont read A movement 

JSR ReadA

SkipReadA:
JSR UpdatePlayer    ;This will handle the natural positional changes each time the calculation fires. 
RTS


UpdatePlayer:   ;This is where all the bugs are at the moment. 
   ;fall down if in air
    
   ;We need to add collision detection again 
  LDX playerx     ;load player x position
  LDA playery
  CLC
  ADC #$07       ;Gets the bottom of the sprite location
  TAY

  JSR CheckCollide   
  BNE Collided         ;Code for collission. We want to stop changing movement once we hit something
  
   ;update our position and factor in negative decreasing velocity. We may have to skip the gravity amount below on the way up
   
   LDA inair
   CMP #$01
   BNE NotInAir
   
   LDA playery
   CLC
   ADC gravity
   CLC
   ADC playervy    ;Currently set to 9 for example. This needs to be decreased
   STA playery
   CLC
   
   ; decrement velocity
    LDA playervy
    ADC #$01
    STA playervy

  NotInAir: 
   RTS

  Collided:     ; Dont move the sprite anymore if we collided with the ground
   LDA #$00
   STA inair

    RTS


LatchController:
  LDA #$01
  STA $4016
  LDA #$00
  STA $4016       ; tell both the controllers to latch buttons
  RTS

;;;;;;;;;;;;;;;;;;
; Read controller input into byte vector
; 76543210
; ||||||||
; |||||||+- RIGHT button
; ||||||+-- LEFT Button
; |||||+--- DOWN Button
; ||||+---- UP Button
; |||+----- START Button
; ||+------ SELECT Button
; |+------- B Button
; +-------- A Button

PollController:
  LDX #$00   ; 8 buttons total
PollControllerLoop:
  LDA $4016  ; player 1 - A 
  LSR A      ; shift right
  ROL controller1    ; rotate left button vector in mem location $0003
  INX
  CPX #$08
  BNE PollControllerLoop
  RTS

ReadRight: 
  LDA controller1  ; controller1 1 - A button
  AND #%00000001  ; only look at bit 0
  BEQ ReadRightDone   ; branch to ReadADone if button is NOT pressed (0)
                  ; add instructions here to do something when button IS pressed (1)
  
  LDY playery     ;load player x position
  LDA playerx
  CLC
  ADC #$10    ;16 pixels to the right. 
  TAX

  JSR CheckCollide   
  BNE SkipCollide  ;Code for collission. We want to reverse the effect of altering the Y position above. BNE = Collided so do nothing

  LDA $0203       ; load sprite X position
  CLC             ; make sure the carry flag is clear
  LDA playerx 
  ADC #$02
  STA playerx

  LDA #$01
  STA inair
  CLC


ReadRightDone:        ; handling this button is done
  RTS
  
ReadLeft: 
  LDA controller1       ; controller1 1 - B button
  AND #%00000010  ; only look at bit 0
  BEQ ReadLeftDone   ; branch to ReadBDone if button is NOT pressed (0)
                  ; add instructions here to do something when button IS pressed (1)
  
  
  
  LDY playery     ;load player x position
  LDA playerx
  CLC
  TAX

  JSR CheckCollide   
  BNE SkipCollide  ;Code for collission. We want to reverse the effect of altering the Y position above. BNE = Collided so do nothing
  
  LDA $0203       ; load sprite X position
  CLC
  LDA playerx
  ADC #$FE      ;is this a negative number? 
  STA playerx

  LDA #$01
  STA inair
  CLC

ReadLeftDone:        ; handling this button is done
  RTS

ReadA:  
  LDA controller1  ; controller1 1 - A button
  AND #%10000000 ; only look at bit 0
  BEQ ReadADone   ; branch to ReadADone if button is NOT pressed (0)
                  ; add instructions here to do something when button IS pressed (1)

  ; Need to implement a way to stop from multiple jumps
  ; This means that at all times we are currently in the air
  LDA #$FD        ; Set negative velocity. This means if player location is 240, it becomes 240 + 1 + -16 yy. = 226 and upwards. 
  STA playervy    ;
  CLC

  LDA playery
  ADC #$FE
  STA playery
  CLC 

  LDA #$01    ;Set us back in the air
  STA inair 
  CLC       

ReadADone: 
  RTS

ReadUp: 
  LDA controller1       ; controller1 1 - Up button
  AND #%0001000  ; only look at bit 0
  BEQ ReadUpDone   ; branch to ReadBDone if button is NOT pressed (0)
                  ; add instructions here to do something when button IS pressed (1)
  LDA $0200       ; load sprite Y position
  CLC
  LDA playery
  ADC #$FC      ;is this a negative number? 
  STA playery
  CLC
  ;We want to set inair to true in this case
  LDA #$01
  CLC

  STA inair

ReadUpDone:        ; handling this button is done
  RTS

ReadDown: 
  LDA controller1       ; controller1 1 - Up button
  AND #%0000100  ; only look at bit 0
  BEQ ReadDownDone   ; branch to ReadBDone if button is NOT pressed (0)
                  ; add instructions here to do something when button IS pressed (1)
  ;Check collide

  LDX playerx     ;load player x position
  LDA playery
  CLC
  ADC #$07
  TAY
  
  JSR CheckCollide   
  BNE SkipCollide  ;Code for collission. We want to reverse the effect of altering the Y position above. BNE = Collided so do nothing
  
  LDA $0200       ; load sprite Y position
  CLC
  LDA playery
  ADC #$02        ;Do the movement
  STA playery

SkipCollide:

RTS

ReadDownDone:        ; handling this button is done
  RTS

; X/64 + Y/8 + 4
; X/8 AND %0111

CheckCollide:
    TXA             ;x/64   We do a few LSR /2, /4, /8,/ 16 each time till we get to 64. 
    LSR
    LSR
    LSR
    LSR
    LSR
    LSR
    STA tmp 
    TYA             ;y/8 * 4       /2/4/8 then 2 * 2 
    LSR
    LSR
    LSR
    ASL
    ASL
    CLC
    ADC tmp
    TAY    ;Byte Index

    TXA    ;X/8
    LSR
    LSR
    LSR
    AND #%0111      ;calculating
    TAX             ;bitmark index now stored in X

    LDA CollisionMap, y          ;Eg %10000000
    AND BitMask, x               ;Eg %10000000  (collided example)
    RTS                          ; Answer would be 1    BEQ = not collided   BNE = collided






;Ends the program, from here down is just
; DATA ONLY ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


CollisionMap:
    .byte %00000000, %00000000, %00000000, %00000000 
    .byte %00000000, %00000000, %00000000, %00000000 
    .byte %00000000, %00000000, %00000000, %00000000 
    .byte %00000000, %00000000, %00000000, %00000000 
    .byte %00000000, %00000000, %00000000, %00000000 
    .byte %00000000, %00000000, %00000000, %00000000 
    .byte %00000000, %00000000, %00000000, %00000000 
    .byte %00000000, %00000000, %00000000, %00000000 
    .byte %00000000, %00000000, %00000000, %00000000 
    .byte %00000000, %00000000, %00000000, %00000000 
    .byte %00001111, %00000000, %00000000, %00000000 
    .byte %00001111, %00000000, %00000000, %00000000 
    .byte %00001111, %00000000, %00000000, %00000000 
    .byte %00000000, %00000000, %00000000, %00000000 
    .byte %00000000, %00000000, %00000000, %00000000 
    .byte %00000000, %00000000, %00000000, %00000000 
    .byte %00000000, %00000011, %11000000, %00000000 
    .byte %00000000, %00000011, %11000000, %00000000 
    .byte %00000000, %00000000, %00100000, %00000000 
    .byte %00000000, %00000000, %0010000, %00000000 
    .byte %00000000, %00000000, %00000000, %00000000 
    .byte %00000000, %00000000, %00000000, %00000000 
    .byte %00000000, %00000000, %00000000, %00000000 
    .byte %00000000, %00000000, %00000011, %11110000 
    .byte %00000000, %00000000, %00000011, %11110000 
    .byte %00000000, %00000000, %00000011, %11110000 
    .byte %11111111, %11111111, %11111111, %11111111 
    .byte %11111111, %11111111, %11111111, %11111111 
    .byte %11111111, %11111111, %11111111, %11111111 
    .byte %11111111, %11111111, %11111111, %11111111 

BitMask:
    .byte %10000000
    .byte %01000000
    .byte %00100000
    .byte %00010000
    .byte %00001000
    .byte %00000100
    .byte %00000010
    .byte %00000001

PALETTEDATA:   ;This is a label to store the colour information for backgrounds, and for sprites. This is basically a list of 8 bit values that will map to colours in the PPU already. (refer to PPUT guide to know what to use)
	.byte $00, $0F, $00, $10, 	$00, $0A, $15, $01, 	$00, $29, $28, $27, 	$00, $34, $24, $14 	;background palettes. it goes index 0,1,2,3   . Location 1 is marked with code 55 when applying to 4 blocks
	.byte $31, $0F, $15, $30, 	$00, $0F, $11, $30, 	$00, $0F, $30, $27, 	$00, $3C, $2C, $1C 	;sprite palettes



PLAYERSPRITE:
;Y, SPRITE NUM, attributes, X
; 1st byte encodes the y position
; 2nd byte encodes the tile index loaded into the PPU 
; 3rd byte encodes any sprite attributes
;  76543210
;  |||   ||
;  |||   ++- Color Palette of sprite.  Choose which set of 4 from the 16 colors to use
;  |||
;  ||+------ Priority (0: in front of background; 1: behind background)
;  |+------- Flip sprite horizontally
;  +-------- Flip sprite vertically
; 4th byte encodes the x position   $0203 for X, $200 for Y 

    .byte $40, $00, %00000000, $40       ; Here we select from the character rom pallete for sprites, location hex $00 (first sprite)
	.byte $40, $01, %00000000, $48 
	.byte $48, $10, %00000000, $40       ; Y location and X location adjusted by X + 8 pixels as this is the size of the sprint H + W. However this is just indicative as we dont actually need this because we set the offsets later on based on the location
	.byte $48, $11, %00000000, $48       ;of tile 1


playerspriteoffset:    ;Maintain the offsets between the parts. This means when we render all the parts for the sprites, we base the location offset to the starting of X  
      ;x   y
  .byte $00, $00; (-8, -0)    (0,0)     Top left
  .byte $08, $00; (8,  -16)             Top right
  .byte $00, $08; (8 , -16)             bottom left
  .byte $08, $08; (-8, -8)              bottom right




BACKGROUNDDATA:	;512 BYTES  Starting from address $2100 in memory to $23BF?. STUDY THIS MORE AS BACKGROUNDS SUCK :D 
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00    ;32
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00    ;64;
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00    ; 96
	.byte $00,$00,$B1,$9F,$A6,$9D,$A9,$A7,$9F,$00,$00,$AE,$A9,$00,$00,$A7,$B3,$00,$00,$A1,$9B,$A7,$9F,$00,$00,$B9,$BA,$00,$00,$00,$00,$00    ;128
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00    ;160
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00    ;192
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00    ;224
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00    ;256

	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00    ;288
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00    ;320
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00    ;352
	.byte $00,$00,$00,$00,$8A,$8A,$8A,$8A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00    ;384
	.byte $00,$00,$00,$00,$8A,$8A,$8A,$8A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00    ;416
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00    ;448
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00    ;480
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00    ;512

    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00      ;512 Onwards
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$8A,$8A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00     
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$8A,$8A,$8A,$8A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00      
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$8A,$8A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$8A,$8A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00      
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$8A,$8A,$00,$00,$00,$00,$00,$00,$00,$00  
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00  
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00    

    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$8A,$8A,$8A,$8A,$8A,$8A,$00,$00,$00,$00     ;Final block 768
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$8A,$8A,$8A,$8A,$8A,$8A,$00,$00,$00,$00  
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$8A,$8A,$8A,$8A,$8A,$8A,$00,$00,$00,$00  
    .byte $8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A  
    .byte $8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A  
    .byte $8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A,$8A  

   

BACKGROUNDPALETTEDATA:	;32 bytes    %55 =   00110011  (groups of 2 to define what pallete to use). And its setting them all for all tiles so 32 of them. This bytes sit at the end of the bytes used by by the screen pattern table.
                        ; This is the mapping of tiles to the pallete of data to use 
                        ; These codes represent the order, in bit paits for each tle. For example, the first 4 tiles on the display are controlled by the first byte. This byte is broken up 11 11 11 01 etc. Find the code to assign each tile to a colour format. 
                        ; Need to find out how the colour is applied, or is that this is bitwise mapped again? I think it would be. This means the code here the 2 bits per block apply a bitwise to the number to do the colour? Lerts just apply 1 for now
	.byte $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55
	.byte $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55
    .byte $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55
    .byte $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55
    

.segment "VECTORS"  ;Definition for what to do if we get interups. 
    .word NMI     ;The word command sets the address for where these code bases are once they occur
    .word RESET

    ; special hardware interupts. 



.segment "CHARS"    ; links to a location elsewhere that simulates the address of accessing the character from for sprites and backgrounds. 
    .incbin "rom.chr"   ;needs to be exactly 8k in size. What is this doing? I assume this loads the 8k set of data. This is the 8kb of data that would be on the rom, and essentially acts as the holder for the pretend "CHAR.ROM" that would be on a catridge. 
                        ;There is no where to store it so looking at this, we should see the data for the sprites. 
                        ;This all gets loaded straight into the PPU memory store (or is it addresses only?) 