crystal=1843200/16 # in HZ
baudlist=[
    9600,
    19200,
    38400,
    57600,
    115200,
    230400,
    460800,
    921600
]

for baud in baudlist:
    prescaler=int(crystal/baud)
    print(baud)
    print(prescaler)
    print(bin(prescaler).replace("0b", ""))
