OP_LSDSK:
    call ideif_prnt_devtable
    ret

OP_SELDSK:
    ;DE contains pointer
    push de
    pop hl
    ld a,(hl)
    sbc 69
    jr c,_OP_SELDSK_INVALID
    cp 4
    jr nc, _OP_SELDSK_INVALID
    call ideif_drv_sel
    ret

_OP_SELDSK_INVALID:
    ld hl,[_OP_SELDSK_INVALID_STR]
    call print_str
    LD DE,0x20
    LD BC,0x70
    CALL beep
    ret

OP_DIR:
    CALL fat_print_directory
    ret

OP_CD:
    call fat_cd_single
    ret

_OP_SELDSK_INVALID_STR:
    db 10,13,"Invalid drive letter",10,13,0

OP_FSEXEC:
    call fat_exec
    ret