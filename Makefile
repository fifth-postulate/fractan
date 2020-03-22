.PHONY: all, clean

SOURCE_DIR=src
BUILD_DIR=docs/js
SOURCE=${SOURCE_DIR}/Fractan.elm
TARGET=${BUILD_DIR}/fractan.js
MINIFIED_TARGET=${BUILD_DIR}/fractan.min.js

all: ${MINIFIED_TARGET}
	@echo "finished"

${MINIFIED_TARGET}: ${TARGET} 
	uglifyjs $< --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" | uglifyjs --mangle --output=$@

${TARGET}: ${SOURCE} ${SOURCE_DIR}/*.elm
	elm make $< --optimize --output $@

clean:
	rm -f ${TARGET} ${MINIFIED_TARGET}⏎