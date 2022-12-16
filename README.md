# Z8C-Homebrew-Computer

# Using the build tools
The Z8C uses the zmac macro assembler (http://48k.ca/zmac.html)

To easily develop for my Z80 homebrew system, I designed a simple
assempler development environment. This packs the following tasks into one script:
* zmac assembly
* object file creation (for xmodem upload)
* monitor file creation (creates monitor commands for ascii upload)
* symbol table include file generation
* eeprom programming for the minipro

## Prequisites
* zmac - self-compiled version included in ./Utility.
* minipro (https://gitlab.com/DavidGriffith/minipro)
* binutils (apt-get install binutils-multiarch, 
    pacman -S aarch64-linux-gnu-binutils )

## Building a project
`./build.sh <dir> <asm filename>`
* `dir`: Path to the location of the asm file
* `asm filename`: name of the assembly file without suffix

```
    # e.g.:
    cd ./Utility
    ./build.sh ../OperatingSystem/monitor_v2  
    ./build.sh ../OperatingSystem/software  
```
## Building a project with a properties file

`./build.sh <dir>`
* `dir`: Path to the location of the asm file

The directory must contain a file called `properties.env`
This file can contain project-specific options.

Example:
```
export OPT_GEN_SYMBOLTABLE=1
export OPT_GEN_MONFILE=0
export OPT_GEN_OBJFILE=1
export OPT_WRITEROM=1
export FILENAME=main
```

### Options
* `OPT_GEN_SYMBOLTABLE` if set to 1: Generates the symbols.s file
* `OPT_GEN_MONFILE` if set to 1: Generates the monitor file
* `OPT_GEN_OBJFILE` if set to 1: Generates the object file
* `OPT_WRITEROM` if set to 1: Starts eeprom programmer
* `FILENAME` same as the `<asm filename>` paramter of the build script
* `ROOT_DIR` overrides the project root path. required if git is not used
* `EEPROM_PART` sets EEPROM part for minipro. Default: `AT28C256`


# System design
The Z80 Hombrew computer is modular computer system, designed arround a custom backplane.

## Available modules
### Z8C CPU/COM Board
* Z80 ZPU @ 4MHz (6MHz planned)
* 64 KiB SRAM
* 32 KiB EEPROM (R/W switchable)
* Z80 CTC with external 1.8432MHz Clock switchable per channel
* Z80 SIO (Up to 115200 BAUD possible @ X16 SIO prescaler)
* 1 FTDI Connector
* 1 RS232 DSUB-9 Port
* 8 front-facing DIP switches (currently used for baud-rate setup)
* Power-On-Reset Circuit

```
Memory Layout
       Default ($00 = 0x00)  Mode 1 ($00 = 0x02)  Mode 2 ($00 = 0x01)             
$0000 +--------------------+--------------------+--------------------+
      | EEPROM Lower 16KiB | EEPROM Upper 16KiB | SRAM               |
$4000 +--------------------+--------------------+                    |
      | SRAM               | SRAM               |                    | 
      |                    |                    |                    |
      |                    |                    |                    |
$FFFF +--------------------+--------------------+--------------------+

Note: In default mode and mode 1, the MEMAQ pin (pin a5 on backplane) 
      can be used to inject an external memory device in the $0000-$3FFF 
      window.

IO Addresses:
$00 Memory register
$01 DIP Switches
$04 CTC (First address)
$08 SIO (First address)
```

### Z8C MIO Board
* Z80 PIO (Used for GPIO and non Z80 compliant interrupt handling)
* VT82C42 PS/2 controller
* AM5911A (only usable with CPU-Clock under 3 MHz)
* PFC8584 I2C Controller
* DS1307 RTC
* 8-Bit GPIO Port

//TODO: Add more Info

### Z8C Disk Interface Board
* Intel 82C55 based IDE Interface
* FDC9266 Floppy Disk Controller

//TODO: Add more Info
