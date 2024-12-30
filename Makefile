##
# Run the tests to check that the conversion works.
#


HERE = $(shell pwd -P)
CROSS = dockcross-linux-armv7
CROSS_PREFIX = arm-cortexa8_neon-linux-gnueabihf-
AS = ${HERE}/${CROSS} ${CROSS_PREFIX}as


all: tests


${CROSS}:
	docker run --rm dockcross/linux-armv7a > ${CROSS}
	chmod +x ${CROSS}
	# Prepare the image, so that the execution time isn't in the first test.
	${HERE}/${CROSS} whoami


tests: ${CROSS} testdata/test.pl dirs
	AS="${AS}" perl testdata/test.pl -junitxml artifacts/junit.xml -script tests.txt -show-command -show-output "../test-conversion.sh" testdata

dirs:
	mkdir -p artifacts
