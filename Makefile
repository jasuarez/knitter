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
	knitter_tr_simple.beam knitter_mesg.beam knitter_ans_file.beam		\
	knitter_conv_null.beam knitter_ans.beam knitter_mesg_conv.beam		\
	knitter_tr_simple_impl.beam knitter_impl.beam				\
	knitter_conv_null_impl.beam knitter_error.beam

clean:
	-${RM} *.beam
	-${RM} knitter_parser.erl
