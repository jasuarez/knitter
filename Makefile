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

all: knitter_scanner.beam knitter_parser.beam knitter_util.beam knitter.beam	\
	knitter_tr_simple.beam knitter_mesg.beam knitter_ans_simple.beam	\
	knitter_conv_null.beam

clean:
	-${RM} *.beam
