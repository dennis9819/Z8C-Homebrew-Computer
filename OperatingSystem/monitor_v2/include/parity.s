; a contains data
; bc destroied
; carry is set if odd, reset if even
calc_parity:
    ld c,0  ;parity data
    ld b,8  ;bit counter
calc_parity_loop:
    rrca
    jr nc,calc_parity_loop_2    ;if not zero then skip increment
    inc c
calc_parity_loop_2:
    djnz calc_parity_loop
    ld a,c
    rra     ;carry is set to bit 0 of high-counter. 
            ;if bit1 is set -> odd # of 1s else even # of 1s
    ret