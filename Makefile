all : omgwtf2_cob

omgwtf2_cob : omgwtf2_cob.cob constants.cpy casts.cpy atbas_cob.tab.c hacks.c
	cobc -x -C omgwtf2_cob.cob -debug -static -Wall -g
	gcc -m32 omgwtf2_cob.c atbas_cob.tab.c hacks.c -O0 -g -lcob -lgmp -o omgwtf2_cob -lncurses

atbas_cob.tab.c : atbas_cob.y omgwtf2_cob.h
	bison atbas_cob.y

.PHONY: clean

clean :
	rm omgwtf2_cob atbas_cob.tab.c omgwtf2_cob.c.l*.h omgwtf2_cob.c.h omgwtf2_cob.c

