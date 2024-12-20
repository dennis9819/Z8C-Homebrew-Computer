#!/bin/bash

## define functions
log() {
    echo "[$(date -Is)]" "$@"
}

help() {
    echo "Build tool for z8c computer system"
    echo ""
    echo "-h        : disply help page"
    echo "-i <file> : input file (main.s)"
    echo "-c <file> : output as .com file" 
    echo "-m <file> : output as .mon file" 
}

while getopts ":hi:c:m:" option; do
   case $option in
      h) help; exit;;
      i) VAR_INPUT=$OPTARG;;
      c) VAR_EXECUTABLE=$OPTARG;;
      m) VAR_MONITOR=$OPTARG;;
   esac
done

if [ -z $VAR_INPUT ]; then
    log "ERROR: Please use -i to specify input file"
    exit 1
fi

if ! [ -f $VAR_INPUT ]; then
    log "ERROR: $VAR_INPUT not found."
    exit 1
fi

ROOT_DIR="$(git rev-parse --show-toplevel)"

PROJ_PATH="$(dirname "$VAR_INPUT")"
FULLPATH="$(readlink -f "$VAR_INPUT")"
FILENAME="$(echo "$VAR_INPUT" | sed 's/^.*\///' | sed 's/\..*//')"
log "INFO: Selected main:    $FULLPATH"
log "INFO: Selected project: $PROJ_PATH"
log "INFO: Root directory:   $ROOT_DIR"

cd $PROJ_PATH
CMD="$ROOT_DIR/Utility/zmac $FULLPATH -I include -L --oo hex,lst"
log "INFO: Run build: $CMD"

$CMD
RC_BUILD=$?
if [ $RC_BUILD -gt 0 ]; then
	log "ERROR: Build failed! Exit."
	exit $RC_BUILD
else
	log "INFO: Build successfull!"
fi

rm .zout -Rf
mv zout .zout

## create object file
log "INFO: Generating object file .zout/$FILENAME.bin"
CMD="objcopy --input-target=ihex --output-target=binary .zout/$FILENAME.hex .zout/$FILENAME.bin"
log "INFO: Run exec: $CMD"
$CMD

## generate monitor file
log "INFO: Generating monitor file .zout/$FILENAME.mon"
while read p; do
    LINE="!${p:3:4} $(echo ${p:9:-2} | sed 's/.\{2\}/& /g' | sed 's/\ $//')"
    echo -e "$LINE" >> .zout/$FILENAME.mon
done <<< "$(grep -e ':......00.*' .zout/$FILENAME.hex )" 
log "INFO: Generated $(cat .zout/$FILENAME.mon | wc -l) lines!"


## generate symbol table
log "INFO: Generating symbol file .zout/symbols.s"
log "INFO: Building Symbol File"
# generate call locations list
echo -e ";This file is generated by the build script.\n;Do not make any changes here!\n" > .zout/symbols.s 
sed -n '/Symbol Table:/,$p' ".zout/$FILENAME.lst" | \
    grep -v "Symbol Table:" | grep -v -e '^$' | \
    sed 's/=/ /gm' | awk '{printf("%s equ 0x%s\n",$1,$2)}' \
    >> .zout/symbols.s 
log "INFO: Generated Symbol file @ .zout/symbols.s"

## generate .com
log "INFO: Generating symbol file .zout/$FILENAME.COM"
cp .zout/$FILENAME.bin .zout/$FILENAME.com
log "INFO: Generated executable file @ .zout/$FILENAME.com"

## echo generate gititgnore
echo "$FILENAME*" > .zout/.gitignore
cd -

if [ ! -z $VAR_EXECUTABLE ]; then
    log "INFO: Store executable to $VAR_EXECUTABLE"
    cp $PROJ_PATH/.zout/$FILENAME.com $VAR_EXECUTABLE
fi

if [ ! -z $VAR_MONITOR ]; then
    log "INFO: Store monitor script to $VAR_MONITOR"
    cp $PROJ_PATH/.zout/$FILENAME.mon $VAR_MONITOR
fi