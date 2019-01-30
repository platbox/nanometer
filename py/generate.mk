PY_FILES = $(wildcard $(CURDIR)/py/*_generate.py)
HRL_FILES = $(patsubst $(CURDIR)/py/%.py,$(CURDIR)/include/%.hrl,$(PY_FILES))
.PHONY: all

all: $(HRL_FILES)
	@:

$(CURDIR)/include/%.hrl: $(CURDIR)/py/%.py
	- python3 $< > $@.tmp && mv $@.tmp $@ || rm -f $@.tmp
