
HERE = $(shell pwd -P)
CROSS = dockcross-linux-armv7
CROSS_PREFIX = arm-cortexa8_neon-linux-gnueabihf-
AS = ${HERE}/${CROSS} ${CROSS_PREFIX}as


${CROSS}:
	docker run --rm dockcross/linux-armv7a > ${CROSS}
	chmod +x ${CROSS}


tests: ${CROSS} testdata/test.pl
	AS="${AS}" perl testdata/test.pl -script tests.txt -show-command -show-output "../test-conversion.sh" testdata
