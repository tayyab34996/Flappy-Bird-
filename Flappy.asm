[org 0x0100]
 mov ax, 0x0013 
 int 0x10 
 mov ax, 0xA000 
 mov es, ax 
     jmp start
exit_msg: db 'Exit? (y/n)', 0  ;
game_ended_msg: db 'Game Ended. Press Key', 0
a_keeper: dw 0
rect_mov_count: dw 0
green_shades db 2, 10, 12, 28, 34 
upperPillarHeight dw 0 
lowerPillarHeight dw 0 
bird_bottom_corner: dw 0
bird_bottom_corner_end: dw 0
bird_top_corner: dw 0
bird_pos: dw 31180
pipesX: dw 319, 459
pipesY: dw 20 , 20
leftOverPipeWidth: dw 0
leftOverPipeY: dw 0
boolDrawBottomPipe: dw 0
intBottomPipeStart: dw 0
intPipeEndX: dw 0
collision_detected: dw 0
boolAdjusted: dw 0
boolFirstPipe: dw 0
isAnimating: dw 0
BirdDirection: db 'D'
bird_position_holder: dw 0
score :dw 0
game_end: dw 0
tickcount: dw 0
old_timer:dw 0,0
oldtimer : dw 0, 0
delay_threshold dw 1600   
timer_hooked db 0    
is_flapping :db 0  
upper_pillar_helper: dw 0
GameTitle: db 'Flappy Bird', 0
Instructions: db 'Press UP ARROW to move up,ESC to quit', 0
Instructions1: db 'If ESC is pressed, game stops', 0
Instructions2: db 'If y is pressed, it exits', 0
Instructions3: db 'else it continues from where it stops', 0
StartMessage: db 'Press any key to start...', 0
cscore: db 'Score: ' ,0
notes: dw 0x1FB4, 0x152F, 0x0A97  ; Frequencies for D3, A3, and A4
durations: dw 500, 500, 500         ; Durations in milliseconds
note_count: dw 3                    ; Number of notes
current_note: dw 0                  ; Index of the current note
; pcb: times *2 dw 0 ; Allocate space for 2 tasks (game + music)

pcb: dw 0, 0, 0, 0, 0 ; task0 regs[cs:pcb + 0]
dw 0, 0, 0, 0, 0 ;
current: db 0
background:
    push cs         ; Save code segment (CS) to stack
    pop ds          ; Set data segment (DS) to CS
    push es         ; Save extra segment (ES) to stack
    xor di, di      ; Clear DI (set to 0)
    mov cx, 320 * 200  ; Set loop counter (fills entire screen)
    mov al, 2       ; Set color value (2) for filling
    rep stosb       ; Fill screen with color 2
    pop es          ; Restore extra segment (ES)
    ret             ; Return from procedure

DisplayIntroduction:
    push cs         ; Save code segment (CS) to stack
    pop ds          ; Set data segment (DS) to CS
    push es         ; Save extra segment (ES) to stack
    call background ; Call the background procedure

    mov dh, 7       ; Set row position (7)
    mov dl, 3       ; Set column position (3)
    mov si, GameTitle ; Load address of GameTitle string
    call PrintStringAtPosition ; Print game title at (7,3)

    mov dh, 15      ; Set row position (15)
    mov si, Instructions ; Load address of Instructions string
    call PrintStringAtPosition ; Print instructions at row 15

    mov dh, 17      ; Set row position (17)
    mov si, StartMessage ; Load address of StartMessage string
    call PrintStringAtPosition ; Print start message at row 17

    pop es          ; Restore extra segment (ES)
    ret             ; Return from procedure


PrintStringAtPosition:
    push cs         ; Save code segment (CS) to stack
    pop ds          ; Set data segment (DS) to CS
    push es         ; Save extra segment (ES) to stack
    pusha           ; Save all registers to stack

    mov ah, 02h     ; BIOS function to set cursor position
    int 10h         ; Call interrupt 10h to move cursor

    mov ah, 0Eh     ; BIOS function for teletype output (prints a character)
    mov bh, 0       ; Page number (usually 0)
    mov bl, 15      ; Text color (white)

print_position_loop:
    lodsb           ; Load next character from SI into AL
    cmp al, 0       ; Check if null terminator (end of string)
    je done_position_print ; If null, exit loop
    int 10h         ; Print character in AL using teletype mode
    jmp print_position_loop ; Repeat for next character

done_position_print:
    popa            ; Restore all registers from stack
    pop es          ; Restore extra segment (ES)
    ret             ; Return from procedure

collision:
    cli                ; Disable interrupts (not necessary here but might be used for consistency)
    push cs 
    pop ds            ; Set DS to code segment (CS)
    pusha             ; Save all registers to stack

    cmp word [bird_pos], 150  ; Check if bird's position is greater than 150
    ja next_check            ; If above 150, continue checking
    mov word [collision_detected], 1  ; If not, set collision flag
    jmp get_out              ; Jump to exit

next_check:
    mov di, word [bird_bottom_corner_end] ; Load bird's bottom corner position
    add di, 320          ; Move down one row in video memory
    cmp di, 57600        ; Check if it exceeds screen boundary
    jb get_out           ; If within bounds, exit
    mov word [collision_detected], 1  ; Otherwise, set collision flag

get_out: 
    popa                 ; Restore all registers
    ret                  ; Return from procedure

ShowExitConfirmation:
    push cs 
    pop ds               ; Set DS to CS

    mov ah, 02h          ; Set cursor position function
    mov bh, 0            ; Page number (0)
    mov dh, 24           ; Row position
    mov dl, 15           ; Column position
    int 10h              ; Call BIOS interrupt

    mov ah, 0Eh          ; BIOS teletype function (prints a character)
    mov bh, 0            ; Page number
    mov bl, 2            ; Text color (green)
    mov si, exit_msg     ; Load address of exit message

print_message_text:
    lodsb                ; Load next character from SI into AL
    cmp al, 0            ; Check for null terminator (end of string)
    je wait_for_key1     ; If null, wait for user input
    int 10h              ; Print character
    jmp print_message_text ; Repeat for next character

wait_for_key1:
    mov ah, 0            ; BIOS keyboard input function
    int 16h              ; Wait for key press
    cmp al, 'y'          ; Check if 'y'
    je exit_game         ; If yes, exit game
    cmp al, 'Y'          ; Check if 'Y'
    je exit_game         ; If yes, exit game

end_erase:
    mov bx, 10           ; Number of lines to erase
    mov di, 60920        ; Starting position in video memory

exit_loop:
    mov cx, 88           ; Number of pixels per line
    mov al, 6            ; Background color
    rep stosb            ; Fill line with color
    dec bx               ; Decrease line count
    add di, 232          ; Move to the next line in memory
    cmp bx, 0            ; Check if all lines are erased
    jne exit_loop        ; If not, continue loop

    ret                  ; Return from procedure

exit_game:  
mov word[game_end],1

    mov al, 11111101b    
    in  al, 61h          
    and al, 11111100b    
    out 61h, al          
    mov al, 10110110b    
    out 43h, al          
    mov al, 0            
    out 42h, al          
	 mov al, 0           
    out 42h, al          
    ; Wait for a keypress
wait_for_key:
    mov ah, 0                      ; BIOS keyboard function
    int 16h                        ; Wait for key

    ; Clear the stack and prepare for a clean exit
reset_stack:
    cli                            ; Disable interrupts
    xor ax, ax                     ; AX = 0
    mov ss, ax                     ; Reset stack segment to 0
    mov sp, 0xFFFE                 ; Set stack pointer to a safe high value
    sti                            ; Re-enable interrupts

    ; Restore video mode to text mode 03h (80x25 color text)
    mov ax, 0x0003                 ; Set video mode 03h
    int 0x10                       ; Call BIOS to restore text mode

    ; Forcefully restore all critical interrupt vectors
    xor ax, ax                     ; AX = 0
    mov es, ax                     ; ES = 0 (interrupt vector table segment)
mov ax,[oldtimer]
mov bx,[oldtimer+2]
     mov word [es:8*4],ax 
     mov [es:8*4+2], bx
    mov ah, 0                      ; BIOS keyboard function
    int 16h                        ; Clear buffer with wait
    mov ah, 1
    int 16h                        ; Test for a keypress
    jz no_more_keys
clear_keys:
    mov ah, 0
    int 16h                        ; Read and discard key
    jmp no_more_keys
no_more_keys:

    mov al, 20h                    ; End-of-Interrupt command
    out 20h, al                    ; Send to PIC
    mov cx, 0FFFFh                 ; Arbitrary delay
delay_loop:
    loop delay_loop
jmp ll2
    ; Terminate program and return to DOS
    mov ax, 0x4C00                 ; DOS terminate program function
    int 0x21                       ; Exit program and return to DOS



upper_part:
    push cs 
    pop ds          ; Set DS to CS
    push es         ; Save extra segment (ES) to stack

    xor di, di      ; Set DI to 0 (starting memory position)
    mov al, 35h     ; Color value (hex 35)
    mov cx, 320 * 60 ; Number of pixels to fill (upper part)
    rep stosb       ; Fill memory with the color

    pop es          ; Restore ES
    ret            ; Return from procedure

medium_part:
    push cs 
    pop ds          ; Set DS to CS
    push es         ; Save ES to stack

    mov di, 320 * 60 ; Start at row 60
    mov al, 35h     ; Color value (hex 35)
    mov cx, 360 * 60 ; Number of pixels to fill (middle part)
    rep stosb       ; Fill memory with the color

    pop es          ; Restore ES
    ret             ; Return from procedure

lower_part:
    push cs 
    pop ds          ; Set DS to CS
    push es         ; Save ES to stack

    mov di, 320 * 120 ; Start at row 120
    mov al, 35h     ; Color value (hex 35)
    mov cx, 320 * 80 ; Number of pixels to fill (lower part)
    rep stosb       ; Fill memory with the color

    pop es          ; Restore ES
    ret             ; Return from procedure
sscore:
    push cs 
    pop ds          ; Set DS to CS
    push es         ; Save ES to stack
    pusha           ; Save all registers

    mov si, cscore  ; Load address of score string
    mov ah, 02h     ; BIOS function to set cursor position
    mov bh, 0       ; Page number
    mov dh, 24      ; Row position
    mov dl, 1       ; Column position
    int 10h         ; Call BIOS interrupt to move cursor

    mov ah, 0Eh     ; BIOS teletype function (prints a character)
    mov bh, 0       ; Page number
    mov bl, 15      ; Text color (white)

print_scoer:
    lodsb           ; Load next character from SI into AL
    cmp al, 0       ; Check for null terminator (end of string)
    je done_        ; If null, exit loop
    int 10h         ; Print character
    jmp print_scoer ; Repeat for next character

done_:
    popa            ; Restore all registers
    pop es          ; Restore ES
    ret             ; Return from procedure

cal_dig:
    call sscore     ; Call the function to display the score

    push cs 
    pop ds          ; Set DS to CS
    push es         ; Save ES to stack

    mov ax, [score] ; Load score value into AX
    cmp ax, 1000    ; Check if score is 1000
    je movv         ; If equal, jump to movv
    jne movv1       ; Otherwise, jump to movv1

movv:
    mov ax, 0       ; Reset score to 0 if it reaches 1000

movv1:
    xor cx, cx      ; Clear CX (digit counter)
    mov bx, 10      ; Set base 10 for division

store_digits:
    push cs 
    pop ds          ; Set DS to CS
    xor dx, dx      ; Clear DX before division
    div bx          ; Divide AX by 10 (remainder in DX)
    push dx         ; Store remainder (digit) on stack
    inc cx          ; Increment digit count
    cmp ax, 0       ; Check if AX is 0
    jnz store_digits ; If not, keep dividing

draw_score:
    push cs 
    pop ds          ; Set DS to CS

    mov ah, 02h     ; BIOS function to set cursor position
    mov bh, 0       ; Page number
    mov dh, 24      ; Row position
    mov dl, 8       ; Column position
    int 10h         ; Move cursor to (24,8)

print_digits:
    pop dx          ; Retrieve stored digit from stack
    add dl, 0x30    ; Convert number to ASCII
    mov al, dl      ; Move ASCII value to AL
    mov ah, 0Eh     ; BIOS teletype function
    mov bh, 0       ; Page number
    int 10h         ; Print character
    inc dl          ; Move cursor to the right
    loop print_digits ; Repeat for next digit

    pop es          ; Restore ES
    ret             ; Return from procedure
bird_body:
    push cs 
    pop ds          ; Set DS to CS
    push es         ; Save ES to stack
    pusha           ; Save all registers

    mov bx, 12      ; Height of the bird (number of rows)
    mov si, [bird_pos]  ; Load bird's initial position
    add si, 18      ; Move to the right by 18 pixels
    mov word [bird_top_corner], si  ; Store the top corner position
    sub si, 18      ; Reset position for body drawing

body:
    mov di, si      ; Copy position to DI (destination index)
    mov al, 14      ; Bird color (hex 14)
    mov cx, 18      ; Width of the bird
    rep stosb       ; Fill 18 pixels with bird color
    sub bx, 1       ; Decrease row counter
    add si, 320     ; Move to the next row (screen width is 320)
    cmp bx, 0       ; Check if all rows are drawn
    jnz body        ; If not, continue drawing

    push di         ; Save DI before modifying it
    mov di, [bird_pos]  ; Reset DI to bird's position
    add di, 1608    ; Move to the lower part of the bird
    mov al, 0xCC    ; Special marker color
    stosb           ; Store pixel
    inc di          ; Move right
    stosb           ; Store pixel
    inc di          ; Move right
    stosb           ; Store pixel
    pop di          ; Restore DI

    sub si, 320     ; Move one row up
    mov word [bird_bottom_corner], si  ; Store bottom-left corner
    add si, 18      ; Move right
    mov word [bird_bottom_corner_end], si  ; Store bottom-right corner
    sub si, 18      ; Reset to original position
    mov [bird_position_holder], si  ; Store bird's current position

    popa            ; Restore all registers
    pop es          ; Restore ES
    ret             ; Return from procedure

defDrawPipe:
    push bp
    mov bp, sp       ; Set up stack frame
    pusha           ; Save all registers

    mov cx, [bp+4]  ; Load pipe X position
    mov dx, 0       ; Reset Y coordinate
    mov ah, 0Ch     ; BIOS function for pixel plotting
    mov bx, [bp+6]  ; Load pipe Y position
    mov word [intBottomPipeStart], bx ; Store bottom pipe start position
    add word [intBottomPipeStart], 55 ; Offset for bottom pipe
    mov word [boolDrawBottomPipe], 0  ; Reset bottom pipe flag
    mov word [intPipeEndX], cx        ; Store pipe's end X coordinate
    add word [intPipeEndX], 40        ; Pipe width (40 pixels)

    cmp word [bp+8], 1   ; Check if it's a top pipe
    je drawTopPipe       ; If so, jump to draw top pipe

    cmp word [leftOverPipeWidth], 0  ; Check leftover pipe width
    jbe endDrawPipe                   ; If zero or negative, skip drawing

    mov cx, [leftOverPipeWidth]  ; Load leftover width
    mov word [intPipeEndX], cx   ; Store new pipe end position
    mov cx, 40                   ; Reset CX for width calculation
    sub cx, [leftOverPipeWidth]  ; Adjust width
    add bx, cx                   ; Adjust Y position
    mov cx, 0                     ; Reset CX
    dec word [leftOverPipeWidth]  ; Decrease leftover width

drawTopPipe:
    mov al, 2   ; Set pipe color

colorSelected:
    cmp cx, 320   ; Check if we are out of bounds
    jae endDrawPipe  ; If CX >= 320, exit
    cmp cx, 0
    jb endDrawPipe   ; If CX < 0, exit
    int 10h          ; Draw pixel

skipPipe:
    inc dx
    cmp dx, [bp+6]      ; Check if we've reached pipe height
    jbe drawTopPipe     ; Continue drawing if not

    cmp dx, [intBottomPipeStart] ; Check bottom pipe start
    ja notSkip            ; If past it, adjust position
    add dx, 54           ; Skip a section of pipe

notSkip:
    cmp dx, 180
    jb drawTopPipe   ; Continue drawing within limits

    inc cx
    mov dx, 0
    inc bx
    cmp cx, [intPipeEndX] ; Check if pipe width is drawn
    jb drawTopPipe

endDrawPipe:
    popa 
    pop bp
    ret 6   ; Clean up stack (returning from function)


; ---------------------------------
; Function to move pipe left
; ---------------------------------
movePipe:
    push cs 
    pop ds
    push bp
    mov bp, sp
    pusha

    inc word [rect_mov_count]   ; Increase movement counter
    cmp word [rect_mov_count], 325  ; If reached max, update score
    je incc
    jne noincc

incc:
    mov word [rect_mov_count], 0  ; Reset movement counter
    inc word [score]             ; Increase score
    call cal_dig                 ; Update displayed score

noincc:
    mov bx, [bp+4]   ; Load pipe X position
    dec word [bx]    ; Move pipe left by 1 pixel
    mov cx, [bx]     ; Copy new X position
    add cx, 41       ; Calculate rightmost X boundary
    mov word [boolAdjusted], 0  ; Reset adjustment flag

    cmp cx, 320
    jge adjustX    ; If pipe reaches the left, reset position
    jmp continueDrawing

adjustX:
    sub word cx, 320
    mov word [boolAdjusted], 1  ; Flag that X was adjusted

continueDrawing:
    mov dx, 0
    mov al, 35h  ; Pipe color
    mov ah, 0Ch  ; BIOS function to draw pixel
    mov si, [bp+6]  ; Load pipe Y position

drawLastColumnSky:
    int 10h        ; Draw sky pixel (erasing previous pipe position)
    cmp dx, [ds:si]
    jne continueDrawingSky  ; Continue if not hitting a pipe boundary

    cmp word [boolAdjusted], 0
    je notCheckForadjusted

    cmp cx, 41
    ja skipSkyColumn

notCheckForadjusted:
    cmp cx, 41
    jb continueDrawingSky

    add dx, 55  ; Skip pipe gap
    jmp drawLastColumnSky

continueDrawingSky:
    inc dx
    cmp dx, 180
    jb drawLastColumnSky

skipSkyColumn:
    sub cx, 41
    cmp cx, 0
    jne endMovePipe

    cmp word [pipesX], 0
    jne endMovePipeContinue

    mov word [boolFirstPipe], 1  ; Set first pipe flag

endMovePipeContinue:
    mov word [leftOverPipeWidth], 40
    mov cx, [ds:si]
    mov word [leftOverPipeY], cx
    mov word [bx], 320   ; Reset pipe position to rightmost part of screen

    push word [bp+6]
    call generateRandomNumber   ; Get a new random Y position for the pipe

endMovePipe:
    popa
    pop bp
    ret 2

generateRandomNumber:
    push cs 
    pop ds
    push bp
    mov bp, sp
    pusha

    mov ah, 0
    int 1Ah  ; BIOS function to get system timer (for randomness)
    mov ax, dx
    mov bx, 60  ; Limit range of random number
    mov dx, 0
    div bx      ; AX / 60, remainder in DX (random value)
    
    mov bx, [bp+4]  ; Get pointer to store random value
    mov word [bx], dx  ; Store random number
    add word [bx], 20  ; Add offset to ensure reasonable height

    popa
    pop bp
    ret 2  ; Clean up stack
; ---------------------------------
; Function to draw the ground with a gradient effect
; ---------------------------------
ground:
    push cs 
    pop ds
    push es

    mov di, 57600            ; Start at the bottom of the screen (Y = 180)
    mov cx, 320 * 10         ; Draw 10 rows of pixels (ground height)
    mov bx, 1                ; Index for gradient shades

.draw_gradient:
    mov al, [green_shades+bx]  ; Get the green shade color
    stosb                     ; Store the pixel color in video memory
    inc bx                    ; Move to the next shade

    cmp bx, 4                 ; Loop gradient colors (cycle through shades)
    jne .continue_gradient
    mov bx, 1                 ; Reset color cycling

.continue_gradient:
    loop .draw_gradient       ; Repeat for the entire ground area

    mov di, 60800             ; Move further down for solid color area
    mov al, 6                 ; Set a solid dark green color
    mov cx, 320 * 10          ; Draw 10 more rows
    rep stosb                 ; Fill the memory with the color

    pop es
    ret


; ---------------------------------
; Simple delay function (short wait time)
; ---------------------------------
delay:
    push cs 
    pop ds
    pusha
    mov cx, 64000     ; Arbitrary delay loop counter

delay1:
    loop delay1       ; Loop until CX becomes zero

    popa
    ret


; ---------------------------------
; Function to erase the bird from the screen
; ---------------------------------
remover_bird:
    push cs 
    pop ds
    pusha
    mov di, si        ; Load bird's current position
    mov al, 35h       ; Sky color (to erase bird)
    mov cx, 18        ; Bird width

    rep stosb         ; Fill bird area with sky color

    popa
    ret


; ---------------------------------
; Function to handle bird falling mechanics
; ---------------------------------
bird_falling:
    push cs 
    pop ds
    pusha
    mov ax, 0xA000  ; Load video memory segment
    mov es, ax

    push di
    mov di, [bird_top_corner]   ; Check top collision with pipes
    cmp byte [es:di], 2
    je exit_game_1              ; If collision detected, exit

    mov di, [bird_bottom_corner]   ; Check bottom collision with pipes
    cmp byte [es:di+320], 2
    je exit_game_1

    mov di, [bird_bottom_corner_end]  ; Check further below for collision
    cmp byte [es:di+320], 2
    je exit_game_1

    pop di  ; Restore DI (bird's position)

    mov si, word [bird_pos]  ; Get current bird position
    call remover_bird        ; Remove bird from old position

    add word [bird_pos], 320  ; Move bird down by 1 row (next screen line)
    call bird_body            ; Redraw bird in new position

    jmp outr  ; Continue normal execution

exit_game_1:
    mov word [collision_detected], 1  ; Set game over flag

pop di  ; Restore DI

outr:
    popa
    ret
; ---------------------------------
; Function to move the bird upwards
; ---------------------------------
moving_up:
    push cs 
    pop ds
    pusha

    mov ax, 0xA000       ; Load video memory segment
    mov es, ax

    push di
    mov di, [bird_top_corner]  ; Check if top of the bird collides with a pipe
    cmp byte [es:di-320], 2
    je exit_game_2

    mov di, [bird_bottom_corner_end] ; Check bottom collision
    cmp byte [es:di], 2
    je exit_game_2

    mov di, [bird_pos]   ; Check current position collision
    cmp byte [es:di-320], 2
    je exit_game_2

    pop di  ; Restore DI (bird position)

    sub word [bird_pos], 320   ; Move bird up one row (320 pixels)
    mov si, [bird_position_holder]
    call remover_bird          ; Clear the old position
    call bird_body             ; Redraw the bird in the new position

    jmp outrr  ; Continue normal execution

exit_game_2:
    mov word [collision_detected], 1  ; Set game over flag
    pop di  ; Restore DI

outrr:
    popa
    ret


; ---------------------------------
; Function to simulate bird falling when colliding
; ---------------------------------
bird_falling_collide:
    push cs 
    pop ds
    pusha

    mov si, word [bird_pos]  ; Get current position
    call remover_bird        ; Remove bird from old position

    add word [bird_pos], 320  ; Move bird down by 1 row
    call bird_body            ; Redraw bird

    popa
    ret


; ---------------------------------
; Function to remove extra bird pixels (used in collision recovery)
; ---------------------------------
remover_extra:
    push cs 
    pop ds
    pusha

    mov di, [bird_pos]  ; Get bird's current position
    mov cx, 12          ; Number of pixels to remove
    mov al, 35h         ; Sky color (to erase bird)

l2:
    stosb              ; Store sky color at bird's position
    add di, 319        ; Move to next row (adjust for screen width)
    loop l2            ; Repeat until all pixels are removed

    popa
    ret


; ---------------------------------
; Function to handle extra collision recovery
; ---------------------------------
collide_extra:
    push cs 
    pop ds
    pusha

l1:
    call remover_extra        ; Remove extra pixels
    add word [bird_pos], 1    ; Shift bird position slightly
    call delay
    call delay
    call bird_body            ; Redraw bird

    mov di, [bird_bottom_corner]  ; Check if collision is still happening
    cmp byte [es:di+320], 2
    je l1  ; Keep adjusting position if still colliding

    popa
    ret

Collider:
    mov al, 11111101b    ; Clear bit 1 (Speaker Gate)
    in  al, 61h          ; Read current state of port 61h
    and al, 11111100b    ; Clear speaker bits
    out 61h, al          ; Write back to port 61h
    mov al, 10110110b    ; Control word: Select Channel 2, Latch Command
    out 43h, al          ; Send control word to PIT command register
    mov al, 0            ; Send initial count low byte
    out 42h, al          ; Write to Channel 2 data port
    mov al, 0            ; Send initial count high byte
    out 42h, al
push cs 
pop ds
; ---------------------------------
; Function: iteration
; Handles bird falling and collision detection.
; ---------------------------------
iteration:
    pusha
    mov ax, 0xA000
    mov es, ax

    ; Check for collision on the right side of the bird
    mov di, [bird_bottom_corner_end]
    cmp byte [es:di+318], 2
    je handle_collision

    ; Check for collision directly below the bird
    mov di, [bird_bottom_corner]
    cmp byte [es:di+320], 2
    je handle_collision

    popa  ; Restore registers
    call delay
    call bird_falling_collide  ; Move the bird down

    ; Continue loop if bird hasn't reached ground (57600)
    cmp word [bird_bottom_corner_end], 57600
    jb iteration

    jmp finish_iteration

handle_collision:
    call collide_extra
    popa
    call Collider

finish_iteration:
    call delay
    call delay
    call exit_game
    ret


; ---------------------------------
; Function: HandleKeyPress
; Handles user key input for bird movement.
; ---------------------------------
HandleKeyPress:
    push cs 
    pop ds
    push ax
    push bx
    push cx
    push dx

    mov ah, 1          ; Check if a key is pressed
    int 0x16
    jz no_key_pressed  ; If no key is pressed, handle falling

    mov ah, 0          ; Read the key press
    int 0x16

    cmp ah, 0x01       ; ESC key pressed?
    je exit_game_press

    cmp ah, 0x48       ; Up arrow key?
    je handle_up

    cmp ah, 0xC8       ; Key release event?
    je handle_release

    jmp no_key_pressed ; Default case, continue as normal

exit_game_press:
    call ShowExitConfirmation
    jmp exit_HandleKeyPress

handle_up:
    mov word [is_flapping], 1  
    mov word [tickcount], 0    
    call moving_up             
    jmp exit_HandleKeyPress

handle_release:
    cmp byte [timer_hooked], 1 ; If timer is already hooked, skip
    je exit_HandleKeyPress
    mov byte [timer_hooked], 1 
    call hook_timer            
    mov word [is_flapping], 0  
    jmp exit_HandleKeyPress

no_key_pressed:
    call bird_falling

exit_HandleKeyPress:
    pop dx
    pop cx
    pop bx
    pop ax
    ret
; ---------------------------------
; Function: hook_timer
; Hooks a custom timer interrupt handler.
; ---------------------------------
hook_timer:
    push cs 
    pop ds
    cli                      ; Disable interrupts while modifying the IVT
    push ds

    xor ax, ax
    mov es, ax               ; Point ES to IVT (Interrupt Vector Table)

    ; Save the old timer interrupt vector (INT 8)
    mov ax, word [es:8*4]    ; Lower 16 bits of the original ISR
    mov word [old_timer], ax

    mov ax, word [es:8*4+2]  ; Upper 16 bits (segment) of the original ISR
    mov word [old_timer+2], ax

    ; Set the new timer interrupt handler
    mov ax, [delay_threshold]
    mov word [es:8*4], timr  ; Set offset of new handler
    mov word [es:8*4+2], cs  ; Set segment of new handler

    pop ds
    sti                      ; Re-enable interrupts
    ret


; ---------------------------------
; Function: timr (Timer Interrupt Service Routine)
; Custom interrupt handler that makes the bird fall periodically.
; ---------------------------------
timr:
    push cs 
    pop ds
    push ax
    push bx
    push cx
    push dx

    inc word [tickcount]        ; Increment tick count
    cmp word [tickcount], ax    ; Compare with delay threshold
    jne end_timer               ; If not reached, exit

    mov word [tickcount], 0     ; Reset tick counter
    call bird_falling           ; Apply gravity effect on bird
    mov byte [timer_hooked], 0  ; Mark timer as unhooked
    call unhook_timer           ; Restore original timer ISR

end_timer:
    mov al, 0x20
    out 0x20, al                ; Send EOI (End of Interrupt) to PIC
    pop dx
    pop cx
    pop bx
    pop ax
    iret                        ; Return from interrupt


; ---------------------------------
; Function: unhook_timer
; Restores the original timer interrupt handler.
; ---------------------------------
unhook_timer:
    push cs 
    pop ds
    cli                        ; Disable interrupts while modifying IVT
    push ds

    xor ax, ax
    mov es, ax                 ; Point ES to IVT

    ; Restore original timer interrupt vector
    mov ax, word [old_timer]
    mov word [es:8*4], ax

    mov ax, word [old_timer+2]
    mov word [es:8*4+2], ax

    pop ds
    sti                        ; Re-enable interrupts
    ret

ground_mover:
    push cs 
    pop ds
    pusha                      ; Save all registers
    push ds
    push es

    mov ax, 0xA000
    mov ds, ax                 ; Set DS to video memory
    mov di, 57600              ; Start position of ground
    mov si, 57602              ; Offset by 2 pixels for movement
    mov cx, 3198               ; Number of pixels to move

    rep movsb                  ; Move bytes (shifting ground pixels left)

    pop es
    pop ds
    popa                       ; Restore all registers
    ret

PrintStartScreen:
 call upper_part  
 call medium_part
 call lower_part
 call bird_body
 call ground
 ret

anime:
	call sscore

main_loop:
	call delay
	call delay
    call HandleKeyPress 
	call collision
	cmp word[collision_detected],1
	je Collider
	mov word[collision_detected],0
continue_loop:
mov ax,0xA000
mov ds,ax
    push pipesY ; y-coordinate address of pipe
    push pipesX ; x-coordinate address of pipe    
    call movePipe

    push pipesY+2 ; y-coordinate address of pipe
    push pipesX+2 ; x-coordinate address of pipe
    call movePipe
    push 1
    push word [pipesY] ; x-coordinate of pipe
    push word [pipesX] ; y-coordinate of pipe
    call defDrawPipe
    push 1
    push word [pipesY+2] ; x-coordinate of pipe
    push word [pipesX+2] ; y-coordinate of pipe
    call defDrawPipe
    push 0
    push word [leftOverPipeY]
    push 0
    call defDrawPipe
	call ground_mover
		;call music
jmp main_loop ; Repeat the main loop
	ret
sound:
    pusha                       ; Save all general-purpose registers
    push ds                     ; Save DS
    push es                     ; Save ES
    push ss                     ; Save SS
    mov ax, 0xA000              ; Video memory segment
    mov es, ax                  ; Use ES for potential screen updates (if needed)

infinite_sound_loop:
    mov al, 182                 ; Command to set frequency mode
    out 43h, al
    mov bx, 1193180             ; Base frequency of 1.19318 MHz
    div bx                      ; Calculate divisor for desired frequency
    out 42h, al                 ; Low byte of divisor to timer
    mov al, ah
    out 42h, al                 ; High byte of divisor to timer

    ; Enable the speaker
    in al, 61h                  ; Read speaker control register
    or al, 00000011b            ; Enable speaker
    out 61h, al

    ; Optional delay or loop (minimal)
    nop                         ; Use NOP or a small delay instead of an infinite loop
    nop

    ; Disable the speaker
    in al, 61h
    and al, 11111100b           ; Disable speaker
    out 61h, al
    jmp infinite_sound_loop

sound_exit:
    pop ss                      ; Restore SS
    pop es                      ; Restore ES
    pop ds                      ; Restore DS
    popa                        ; Restore all general-purpose registers
    ret                         ; Return to caller
prrint:
inc word [score]
call sound
jmp prrint
start:

 call DisplayIntroduction 
 mov ah,0
 int 16h
 cmp ah,0x01
 je exit_game
 call PrintStartScreen 
     xor ax, ax
     mov es, ax
     cli
	 mov ax, word[es:8*4]
	 mov word [oldtimer],ax
	 mov ax,word[es:8*4+2]
	 mov word[oldtimer+2],ax
     mov word [es:8*4], timer
     mov [es:8*4+2], cs
     sti

ll1:
     mov word [pcb+10+4], sound         ; Game logic thread
     mov [pcb+10+6], cs
     mov word [pcb+10+8], 0x0200    
     mov word [pcb+20+4], anime  ; Music thread
     mov [pcb+20+6], cs
     mov word [pcb+20+8], 0x0200         ; Flags
     mov word [current], 0               ; Start with the first task
	 jmp ll1
	 ll2:
	 cmp word[game_end] ,1
	 je exxxxit
	 exxxxit:
 mov ax, 0x4C00
int 0x21
timer:
	push cs 
pop ds
  push ax
 push bx
 mov bl, [cs:current] ; read index of current task ... bl
 mov ax, 10 ; space used by
 mul bl ; multiply to get
 mov bx, ax ; load start of
 pop ax ; read origina

 mov [cs:pcb+bx+2], ax ; space for current task's BX
 pop ax ; read original
 mov [cs:pcb+bx+0], ax ; space for current task's AX
 pop ax ; read original
 mov [cs:pcb+bx+4], ax ; space for current task
 pop ax ; read original
 mov [cs:pcb+bx+6], ax ; space for current task
 pop ax ; read original
 mov [cs:pcb+bx+8], ax ; space for current task
 inc byte [cs:current] ; update current task index...1
 cmp byte [cs:current], 3; is task index out of range
 jne skipreset ; no, proceed
 mov byte [cs:current], 0 ; yes, reset to task 0
 skipreset: 
 mov bl, [cs:current] ; read index of current task
 mov ax, 10 ; space used by
 mul bl ; multiply to get
 mov bx, ax ; load start of
 mov al, 0x20
 out 0x20, al ; send EOI to PIC
 push word [cs:pcb+bx+8] ; flags of new task...
 push word [cs:pcb+bx+6] ; cs of new task ...
 push word [cs:pcb+bx+4] ; ip of new task...
 mov ax, [cs:pcb+bx+0] ; ax of new task...pcb+10+0
 mov bx, [cs:pcb+bx+2] ; bx of new task...pcb+10+2
 iret ; return to new