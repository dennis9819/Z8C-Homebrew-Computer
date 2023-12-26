
BUILD_DIR="./build"

SDCC_BIN_DIR="./sdcc/bin"

MEMLOC_CODE="0x8000"
MEMLOC_DATA="0x5000"



build: clean
	@mkdir ${BUILD_DIR}
	@cd ${BUILD_DIR}
	echo "Build main.c"
	${SDCC_BIN_DIR}/sdcc \
		-mz80 --code-loc ${MEMLOC_CODE} \
		--data-loc ${MEMLOC_DATA} -o ${BUILD_DIR} main.c

clean:
	#rm -rf ${BUILD_DIR}