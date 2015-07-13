#!/usr/bin/make -f

all: dist/atlona-driver

dist/%: derpy/bin/%.rkt
	@mkdir -p dist
	raco exe --orig-exe -o $@ $<

clean:
	rm -rf dist

# EOF
