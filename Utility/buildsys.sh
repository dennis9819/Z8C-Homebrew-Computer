#!/bin/bash
# Z8C build script for system
# by Dennis Gunia 2024
#

SYS_ROOT="./build/out"

mkdir $SYS_ROOT -p    # create destination directory
rm $SYS_ROOT/* -Rf    # clear destination directory

mkdir $SYS_ROOT/COMMANDS
mkdir $SYS_ROOT/TOOLS

# Build command line tools
./Utility/buildx.sh -i "./OperatingSystem/Applications/basic/main.asm" -c "$SYS_ROOT/COMMANDS/BASIC.COM"
./Utility/buildx.sh -i "./OperatingSystem/Applications/filedmp/main.asm" -c "$SYS_ROOT/COMMANDS/FILEDMP.COM"
./Utility/buildx.sh -i "./OperatingSystem/Applications/hellord/main.asm" -c "$SYS_ROOT/TOOLS/HELLORD.COM"

