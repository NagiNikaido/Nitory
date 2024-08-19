## -*- indent-tabs-mode: t -*-

LISP = sbcl

.PHONY: build clean test

build:
	$(LISP) --load "nitory.asd" \
	        --eval '(ql:quickload "nitory")' \
	        --eval '(asdf:make "nitory")'

test:
	$(LISP) --load "nitory.asd" \
	        --eval '(ql:quickload "nitory")' \
	        --eval '(uiop:quit (if (asdf:test-system "nitory") 0 1))'

clean:
	rm -r build/
