################################################################################
## $Id$
################################################################################

EC = erlc
YECC = erlc

FLAGS =
CFLAGS = ${FLAGS}
YFLAGS = ${FLAGS}

%.beam: %.erl
	${EC} ${CFLAGS} $<

%.erl: %.yrl
	${YECC} ${YFLAGS} $<

all: knitter_scanner.beam knitter_parser.beam

clean:
	-${RM} *.beam
