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

all: knitter_scanner.beam knitter_parser.beam knitter_util.beam knitter.beam \
 knitter_port_kek.beam

clean:
	-${RM} *.beam
