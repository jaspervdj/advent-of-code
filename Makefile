C_SRCS  = $(wildcard 20??/*.c)
HS_SRCS = $(wildcard 20??/*.hs)
# ML_SRCS = $(wildcard 20??/*.ml)
ML_SRCS = 

C_BINS  = $(C_SRCS:.c=.c.bin)
HS_BINS = $(HS_SRCS:.hs=.hs.bin)
ML_BINS = $(ML_SRCS:.ml=.ml.bin)

GHC 	  = stack exec ghc --
GHC_FLAGS = -Wall -ilib/hs -O2

.PHONY: all clean
.PRECIOUS: %.hs.depends

all: $(C_BINS) $(HS_BINS) $(ML_BINS)

clean:
	find . -name '*.bak' -delete
	find . -name '*.bin' -delete
	find . -name '*.cmi' -delete
	find . -name '*.cmx' -delete
	find . -name '*.depends' -delete
	find . -name '*.hi' -delete
	find . -name '*.o' -delete

%.c.bin: %.c
	gcc -Wall -O2 -Wpedantic -o $@ $<

%.hs.depends: %.hs
	$(GHC) $(GHC_FLAGS) -M -dep-suffix '' -dep-makefile $@ $<
	sed -ni 's#^.*:[[:space:]]*\(.*\)\.hs$$#$<.bin $<.d : \1.hs#p' $@

%.hs.bin: %.hs %.hs.depends
	$(GHC) $(GHC_FLAGS) $< -o $@

include $(HS_SRCS:.hs=.hs.depends)

%.ml.bin: %.ml
	ocamlopt -w A-24-4 -o $@ $<

.PHONY: %.clj.run
%.clj.run: %.clj
	java -cp lib/clj:$(shell clj -Spath) clojure.main $<

.PHONY: clj
clj:
	rlwrap java -cp lib/clj:$(shell clj -Spath) clojure.main
