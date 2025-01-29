##
# Run the tests to check that the conversion works.
#


HERE = $(shell pwd -P)
CROSS32 = dockcross-linux-armv7
CROSS32_PREFIX = arm-cortexa8_neon-linux-gnueabihf-
CROSS64 = dockcross-linux-arm64
CROSS64_PREFIX = aarch64-unknown-linux-gnu-
AS = ${HERE}/${CROSS32} ${CROSS32_PREFIX}as
AS64 = ${HERE}/${CROSS64} ${CROSS64_PREFIX}as


all: tests


${CROSS32}:
	docker run --rm dockcross/linux-armv7a > ${CROSS32}
	chmod +x ${CROSS32}
	# Prepare the image, so that the execution time isn't in the first test.
	${HERE}/${CROSS32} whoami

${CROSS64}:
	docker run --rm dockcross/linux-arm64 > ${CROSS64}
	chmod +x ${CROSS64}
	# Prepare the image, so that the execution time isn't in the first test.
	${HERE}/${CROSS64} whoami


tests: ${CROSS32} ${CROSS64} testdata/test.pl dirs
	AS="${AS}" AS64="${AS64}" perl testdata/test.pl -no-riscos-names -junitxml artifacts/junit.xml -script tests.txt -show-command -show-output "../test-conversion.sh" testdata

dirs:
	mkdir -p artifacts
