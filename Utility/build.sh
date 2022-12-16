#!/bin/bash
# Z8C Monitor build script
# by Dennis Gunia 2022
#

FILENAME="$2"
EEPROM_PART="AT28C256"

ROOT_DIR="$(git rev-parse --show-toplevel)"
PROJECT_DIR="$1"

if [ -f $1/properties.env ]; then
	echo "Found $1/properties.env - Overriding local vars"
	. $1/properties.env
fi

# Start of build script
echo -e "Start building Z8C monitor...\n"
echo -e "Root Directory    : $ROOT_DIR"
echo -e "Project Directory : $PROJECT_DIR"
echo -e "ASM File          : $FILENAME.asm\n\n"

# Check path
if [ ! -d "$PROJECT_DIR" ]; then
	echo "Project directory not defined. Use ./build.sh <dir> <asm name>"
fi

if [ ! -f "$PROJECT_DIR/$FILENAME.asm" ]; then
	echo "ASM file $PROJECT_DIR/$FILENAME.asm not found. Use ./build.sh <dir> <asm name>"
fi

cd $PROJECT_DIR

# Build
CMD="$ROOT_DIR/Utility/zmac $FILENAME.asm -I include -L --oo hex,lst"
echo -e "[BUILD] Execute: $CMD"
$CMD
RC_BUILD=$?

if [ $RC_BUILD -gt 0 ]; then
	echo "[BUILD] Build failed! Exit."
	exit $RC_BUILD
else
	echo "[BUILD] Build successfull!"
fi

if [ $OPT_GEN_OBJFILE -gt 0 ]; then
	echo "[OBJ] OPT_GEN_OBJFILE set! Generating object file zout/$FILENAME.bin"
	CMD="objcopy --input-target=ihex --output-target=binary zout/$FILENAME.hex zout/$FILENAME.bin"
	echo -e "[OBJ]   Execute: $CMD"
	$CMD
	
	if [ $RC_BUILD -gt 0 ]; then
		echo "[OBJ]   Build failed! Exit."
		exit $RC_BUILD
	else
		echo "[OBJ]   Build successfull!"
		echo "[OBJ]   Binary size: $(stat -c %s zout/$FILENAME.bin) bytes"
	fi
else
	echo "[OBJ] OPT_GEN_OBJFILE not set! Skipping object file"
fi

if [ $OPT_GEN_MONFILE -gt 0 ]; then
	echo "[MON] OPT_GEN_MONFILE set! Generating monitor file zout/$FILENAME.mon"
	if [ -f zout/$FILENAME.mon ]; then
		echo "[MON]   Removing old .mon file"
		rm zout/$FILENAME.mon
	fi

	while read p; do
		LINE="!${p:3:4} $(echo ${p:9:-2} | sed 's/.\{2\}/& /g' | sed 's/\ $//')"
		echo -e "$LINE" >> zout/$FILENAME.mon
	done <<< "$(grep -e ':......00.*' zout/$FILENAME.hex )" 
	echo "[MON]   Generated $(cat zout/$FILENAME.mon | wc -l) lines!"
else
	echo "[OBJ] OPT_GEN_MONFILE not set! Skipping monitor file"
fi


if [ $OPT_GEN_SYMBOLTABLE -gt 0 ]; then
	echo "[SYM] OPT_GEN_SYMBOLTABLE set! Generating symbol file zout/symbols.s"
	echo "[SYM]   Building Symbol File"
	# generate call locations list
	echo -e ";This file is generated by the build script.\n;Do not make any changes here!\n" > zout/symbols.s 
	sed -n '/Symbol Table:/,$p' zout/$FILENAME.lst | \
		grep -v "Symbol Table:" | grep -v -e '^$' | \
		grep -v = | awk '{printf("%s equ 0x%s\n",$1,$2)}' \
		>> zout/symbols.s 
	echo "[SYM]   Generated Symbol file @ zout/symbols.s"
else
	echo "[SYM] OPT_GEN_SYMBOLTABLE not set! Skipping symbol file"
fi

if [ $OPT_WRITEROM -gt 0 ]; then
	echo "[PGM] OPT_WRITEROM set! Starting programmer for $EEPROM_PART"
	read -p "[PGM]   Programm EEPROM? (y/N) " -n 1 -r
	echo    
	if [[ $REPLY =~ ^[Yy]$ ]]
	then
		# programm EEPROM
		minipro -p $EEPROM_PART -w zout/$FILENAME.hex -s
	fi
else
	echo "[PGM] OPT_WRITEROM not set! Skipping programming"
fi

