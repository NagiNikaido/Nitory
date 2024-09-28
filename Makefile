## -*- indent-tabs-mode: t -*-

LISP = sbcl

.PHONY: build clean test bump bump-version

build:
	$(LISP) --disable-debugger \
		--load "nitory.asd" \
	        --eval '(ql:quickload "nitory")' \
	        --eval '(asdf:make "nitory")'

test:
	$(LISP) --disable-debugger \
		--load "nitory.asd" \
	        --eval '(ql:quickload "nitory")' \
	        --eval '(uiop:quit (if (asdf:test-system "nitory") 0 1))'

bump: bump-version
	git commit -a -m "Bump version to $$(eval a=$$(cat VERSION); echo $$a)"
	temp=$$(cat VERSION)
	temp="$${temp%\"}"
	temp="$${temp#\"}"
	git tag -a v$$temp

bump-version:
	./bin/bump_version ${v}

clean:
	rm -r build/
