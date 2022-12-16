EXEC_RST_08:
    call print_char
    ret


EXEC_RST_10:
    push bc
    push de
    push hl
    ;call vdp_cursor_on
    call read_char
    push af
    ;call vdp_cursor_off
    pop af
    pop hl
    pop de
    pop bc
    ret

EXEC_RST_18:
    ld a,0
    ret
