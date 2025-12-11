; HELLO_NAME.COM - Test CP/M I/O
; Prints "Hello World", asks for name, prints "Hello, <name>!"
;
; CP/M BDOS functions:
;   2 = Console output (E = character)
;   9 = Print string (DE = address, terminated by '$')
;  10 = Read console buffer (DE = buffer address)
;       Buffer: byte 0 = max length, byte 1 = actual length, bytes 2+ = data
;
; Assemble at 0x0100 (CP/M TPA)

        ORG     0100H

START:
        ; Print "Hello, World!" using BDOS 9
        LD      DE,MSG_HELLO
        LD      C,9
        CALL    5

        ; Print "What is your name? "
        LD      DE,MSG_PROMPT
        LD      C,9
        CALL    5

        ; Read line using BDOS 10
        LD      DE,BUFFER
        LD      C,10
        CALL    5

        ; Print newline
        LD      DE,MSG_CRLF
        LD      C,9
        CALL    5

        ; Print "Hello, "
        LD      DE,MSG_GREET
        LD      C,9
        CALL    5

        ; Print the entered name character by character
        LD      A,(BUFFER+1)    ; Get actual length
        OR      A
        JR      Z,SKIP_NAME     ; Skip if empty

        LD      B,A             ; B = length counter
        LD      HL,BUFFER+2     ; HL = start of name

PRINT_LOOP:
        LD      E,(HL)          ; Get character
        LD      C,2             ; BDOS console output
        PUSH    HL
        PUSH    BC
        CALL    5
        POP     BC
        POP     HL
        INC     HL
        DJNZ    PRINT_LOOP

SKIP_NAME:
        ; Print "!\r\n"
        LD      DE,MSG_END
        LD      C,9
        CALL    5

        ; Return to CP/M
        RET

; Data section
MSG_HELLO:
        DB      'Hello, World!',0DH,0AH,'$'

MSG_PROMPT:
        DB      'What is your name? $'

MSG_CRLF:
        DB      0DH,0AH,'$'

MSG_GREET:
        DB      'Hello, $'

MSG_END:
        DB      '!',0DH,0AH,'$'

BUFFER:
        DB      32              ; Max length (32 characters)
        DB      0               ; Actual length (filled by BDOS)
        DS      33              ; Space for input + null

        END     START
