.DEFAULT_GOAL := usage

usage:
	@ echo "make all # create distributions for all installed lisps"


lispworks:
	@ mkdir -p dist/lispworks
	@ cd src && lispworks -build deliver.lisp

ecl:
	@ mkdir -p dist/ecl
	@ cat collector/deliver-ecl.lisp|ecl

sbcl:
	@ mkdir -p dist/sbcl
	@ cat src/deliver.lisp|sbcl  --dynamic-space-size 20480 #--control-stack-size 2048  #--disable-debugger
ccl:
	@ mkdir -p dist/ccl || true
	@ cd src && cat deliver.lisp|ccl64

clisp:
	@ mkdir -p dist/clisp || true
	@ cat deliver.lisp|clisp

allegro:
	@ rm -rf dist/allegro kunabi
	@ mkdir -p dist/allegro
	@ cd src && cat deliver.lisp|allegro
	@ mv src/kunabi/* dist/allegro
	@ rm -rf src/kunabi

abcl:
	@ mkdir -p dist/abcl || true
	@ cat deliver.lisp|abcl

cmucl:
	@ cat deliver.lisp|/usr/cmucl/bin/lisp

bench: all
	rm -rf /tmp/m-a && mkdir /tmp/m-a
	KUNABI="/tmp/m-a/" time kunabi-sbcl s ~/ct-test > results/sbcl 2>&1
	rm -rf /tmp/m-a && mkdir /tmp/m-a
	KUNABI="/tmp/m-a/" time kunabi-lispworks s ~/ct-test > results/lispworks 2>&1
	rm -rf /tmp/m-a && mkdir /tmp/m-a
	KUNABI="/tmp/m-a/" time kunabi-allegro s ~/ct-test > results/allegro  2>&1
	rm -rf /tmp/m-a && mkdir /tmp/m-a
	KUNABI="/tmp/m-a/" time kunabi-ccl s ~/ct-test > results/ccl 2>&1
	rm -rf /tmp/m-a
	git add results && git commit -a -m "benchmark results" && git push

all: lispworks sbcl ccl allegro
