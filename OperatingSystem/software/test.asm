    .include "extern_symbols.s" ;include monitor symbols.
    org 0x8000  

    LD A,4
    LD B,5
    ADD A,b
    LD (0x8010),A
    JP PROMPT_BEGIN


