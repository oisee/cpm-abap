; HELLO.COM - Simple CP/M Hello World
; Assembles to 30 bytes
; Load address: 0x0100 (standard CP/M TPA)
;
; BDOS function 9: Print string
;   C = 9
;   DE = address of $-terminated string
;   CALL 0005H

        ORG     0100H

START:  LD      C,9         ; 0E 09       - BDOS function 9 = print string
        LD      DE,MSG      ; 11 08 01    - Address of message (0100 + 8 = 0108)
        CALL    0005H       ; CD 05 00    - Call BDOS
        RET                 ; C9          - Return to CP/M

MSG:    DB      'Hello, CP/M World!',0DH,0AH,'$'

        END     START

; Hex dump:
; 0E 09 11 08 01 CD 05 00 C9 48 65 6C 6C 6F 2C 20
; 43 50 2F 4D 20 57 6F 72 6C 64 21 0D 0A 24
