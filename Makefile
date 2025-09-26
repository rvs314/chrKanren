test-files := $(wildcard tests/*.scm)
test-names := $(notdir $(basename $(test-files)))
chez-targets := $(appendprefix test-chez-, $(test-names))
racket-targets := $(appendprefix test-racket-, $(test-names))

test-all: test-chez test-racket

test-chez: $(chez-targets)

test-chez-%: tests/%.scm
	chezscheme --libdirs ".." --script $^

test-racket: $(racket-targets)

test-racket-%: tests/%.scm
	racket $^

clean:
	rm -rf ./compiled

.PHONY: test-all test-chez $(chez-targets) $(racket-targets) clean
