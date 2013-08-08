
 R_FILES := $(wildcard *.R)
JS_FILES := $(wildcard *.js)

RH_FILES := $(addsuffix .h, $(R_FILES))
JSH_FILES := $(addsuffix .h, $(JS_FILES))

.PHONY : all

all : $(RH_FILES) $(JSH_FILES)
	
	
%.R.h: %.R
	bin2c -o $@ $<

%.js.h: %.js
	bin2c -o $@ $<
