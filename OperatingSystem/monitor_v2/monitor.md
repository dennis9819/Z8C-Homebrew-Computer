# Z8C Monitor V2
Monitor V2 is a less bloated monitor for my Z80 homebrew computer

## System
* Z80 CPU @ 3.686411MHz
* 48k Ram
* TMS9918 VDP (not used for normal operations)
* 2x AY3-8901 
* Serial RS232 Ports

not implemented yet but already present in hardware:
* PS/2 Keyboard Interface
* RTC72421B real time clock

planned
* IDE interface
* 28J60 10Mbit ethernet interface
* dual joystick gameports


## Terminal
* `baud`: 19200
* `mode`: 8N1
* `parity`: none
* `temrinal`: vt102
* `flow`: none / RTS(not implemented yet)

## Commands
### View memory content
`?<addr> <len>`
* `addr`: word in hex -> start address of hexdump
* `len`: byte in hex -> length of hexdump

### Write memory content
`!<addr> <byte>,<byte>,...`
* `addr`: word in hex -> start address
* `byte`: byte in hex followed by space -> bytes to write to memory

### Run program
`$<addr>`
* `addr`: word in hex -> start address of program

### X-Modem
`x<addr>`
* `addr`: word in hex -> start address of recieved binary (at least 0x0420)

### Write IO port
`w<addr> <data>`
* `addr`: byte in hex -> IO port address
* `len`: byte in hex -> data

### Read IO port
`r<addr>`
* `addr`: byte in hex -> IO port address

