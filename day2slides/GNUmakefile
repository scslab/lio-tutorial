
MDFILE := $(word 1, $(basename $(wildcard *.md)))
L ?= $(MDFILE)

DEPS := $(wildcard *.jpg) $(wildcard *.png) $(wildcard *.svg)

all: $(L).html $(L)-slides.html
.PHONY: all

$(L).html: $(L).md $(DEPS) GNUmakefile
	@test -f $<
	pandoc -s -t html -o $@ $<
#	pandoc --data-dir=../pandoc -s -t html -o $@ $<

$(L)-slides.html: $(L).md $(wildcard ../pandoc/slidy/*) $(DEPS) GNUmakefile
	@test -f $<
	pandoc --data-dir=./pandoc --self-contained -s -t slidy -o $@ $<

CONFIG := $(PWD)/config
CONFIGENV = XDG_CONFIG_HOME="$(CONFIG)" \
		XDG_DATA_HOME="$(CONFIG)/share" \
		HOME="$(CONFIG)"
show: $(L)-slides.html
	$(CONFIGENV) xdg-open $< &
.PHONY: show

clean:
	rm -f $(L).html $(L)-slides.html *.o *.hi *~ .*~ \
		$(patsubst %.hs,%,$(wildcard *.hs))
.PHONY: clean
