.include "extern_symbols.s" ;include monitor symbols.
    org 0xB000  
;Testing code
    call ideif_init_drive

    ;testread sector
    LD A,1                  ;read 1 sector
    LD B,IDE_REG_SECTOR
    CALL ide_regwrite_8

    LD A,1                  ;read sector 0
    LD B,IDE_REG_SSECTOR
    CALL ide_regwrite_8

    LD A,0                  ;read cylinder 0
    LD B,IDE_REG_LCYL
    CALL ide_regwrite_8
    LD A,0                  
    LD B,IDE_REG_HCYL
    CALL ide_regwrite_8

    LD A,10100000b          ;read head 0
    LD B,IDE_REG_HEAD
    CALL ide_regwrite_8

    LD A,IDE_CMD_READSEC    ;send read command
    LD B,IDE_REG_CMDSTS
    CALL ide_regwrite_8

    LD HL,MEM_IDE_BUFFER    ;set read/write buffer start address
    call ide_readsector_256_inv ;read 256 words from device

    LD HL,MEM_IDE_BUFFER 
    LD B,32
    call dump_pretty

    JP PROMPT_BEGIN



.include "kdrv_ide8255.s" ;include ide interface driver.
.include "kdrv_ideif.s" ;include ide driver.
.include "prettydump.s" ;include monitor symbols.

delay_small:
    PUSH AF
    POP AF
    PUSH AF
    POP AF
    RET