; prints A as BCD

print_bcd_byte_3:
    PUSH DE
    PUSH BC
    PUSH HL
    PUSH IX
    
    



print_bcd_done:
    POP IX
    POP HL
    POP BC
    POP DE
    RET