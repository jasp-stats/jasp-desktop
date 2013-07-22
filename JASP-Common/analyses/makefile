
R_FILES := $(wildcard *.R)
RH_FILES := $(addsuffix .h, $(R_FILES))

.PHONY : all

all : $(RH_FILES)
	
	
%.R.h: %.R
	bin2c -o $@ $<
