-module(knitter_scanner).
-author('$Author$').
-vsn('$Revision$').

-export([scan/1]).


scan(Input) -> scan(Input, [], 0).


scan(Input, Tokens, BracketBalance) ->
    case catch tokenize(io:get_chars(Input, '', 1)) of
	{'(', Line, Value} ->
	    scan(Input, [{'(', Line, Value} | Tokens], BracketBalance + 1);
	{')', _, _} when BracketBalance < 1 ->
	    exit({error, "Unbalanced brackets"});
	{')', Line, Value} when BracketBalance == 1 ->
	    lists:reverse([{'$eop', Line, '$eop'} | [{')', Line, Value} | Tokens]]);
	{')', Line, Value} when BracketBalance > 1 ->
	    scan(Input, [{')', Line, Value} | Tokens], BracketBalance - 1);
	{'$eop', _, _} ->
	    exit({error, "Unbalanced brackets"});
	{'EXIT', _} ->
	    exit({error, "Unbalanced brackets"});
	Token ->
	    scan(Input, [Token | Tokens], BracketBalance)
    end.


tokenize(eof) ->
    {'$eop', 1, '$eop'};
tokenize([]) ->
    {'$eop', 1, '$eop'};
tokenize("(") ->
    {'(', 1, $(};
tokenize(")") ->
    {')', 1, $)};
tokenize(" ") ->
    {whitespace, 1, 32};
tokenize("\"") ->
    {'"', 1, $"};
tokenize("\'") ->
    {apos, 1, $'};
tokenize(",") ->
    {',', 1, 44};
tokenize("#") ->
    {'#', 1, $#};
tokenize("\\") ->
    {backslash, 1, 92};
tokenize(":") ->
    {':', 1, $:};

tokenize("<") ->
    {special, 1, $<};
tokenize(">") ->
    {special, 1, $>};
tokenize("=") ->
    {special, 1, $=};
tokenize("+") ->
    {special, 1, $+};
tokenize("-") ->
    {special, 1, $-};
tokenize("*") ->
    {special, 1, $*};
tokenize("/") ->
    {special, 1, $/};
tokenize("&") ->
    {special, 1, $&};
tokenize("^") ->
    {special, 1, $^};
tokenize("~") ->
    {special, 1, $~};
tokenize("_") ->
    {special, 1, $_};
tokenize("@") ->
    {special, 1, $@};
tokenize("\$") ->
    {special, 1, $$};
tokenize("%") ->
    {special, 1, $%};
tokenize(".") ->
    {special, 1, $.};
tokenize("!") ->
    {special, 1, $!};
tokenize("?") ->
    {special, 1, $?};

tokenize("0") ->
    {numeric, 1, $0};
tokenize("1") ->
    {numeric, 1, $1};
tokenize("2") ->
    {numeric, 1, $2};
tokenize("3") ->
    {numeric, 1, $3};
tokenize("4") ->
    {numeric, 1, $4};
tokenize("5") ->
    {numeric, 1, $5};
tokenize("6") ->
    {numeric, 1, $6};
tokenize("7") ->
    {numeric, 1, $7};
tokenize("8") ->
    {numeric, 1, $8};
tokenize("9") ->
    {numeric, 1, $9};

tokenize("a") ->
    {alphabetic, 1, $a};
tokenize("b") ->
    {alphabetic, 1, $b};
tokenize("c") ->
    {alphabetic, 1, $c};
tokenize("d") ->
    {alphabetic, 1, $d};
tokenize("e") ->
    {alphabetic, 1, $e};
tokenize("f") ->
    {alphabetic, 1, $f};
tokenize("g") ->
    {alphabetic, 1, $g};
tokenize("h") ->
    {alphabetic, 1, $h};
tokenize("i") ->
    {alphabetic, 1, $i};
tokenize("j") ->
    {alphabetic, 1, $j};
tokenize("k") ->
    {alphabetic, 1, $k};
tokenize("l") ->
    {alphabetic, 1, $l};
tokenize("m") ->
    {alphabetic, 1, $m};
tokenize("n") ->
    {alphabetic, 1, $n};
tokenize("o") ->
    {alphabetic, 1, $o};
tokenize("p") ->
    {alphabetic, 1, $p};
tokenize("q") ->
    {alphabetic, 1, $q};
tokenize("r") ->
    {alphabetic, 1, $r};
tokenize("s") ->
    {alphabetic, 1, $s};
tokenize("t") ->
    {alphabetic, 1, $t};
tokenize("u") ->
    {alphabetic, 1, $u};
tokenize("v") ->
    {alphabetic, 1, $v};
tokenize("w") ->
    {alphabetic, 1, $w};
tokenize("x") ->
    {alphabetic, 1, $x};
tokenize("y") ->
    {alphabetic, 1, $y};
tokenize("z") ->
    {alphabetic, 1, $z};
tokenize("A") ->
    {alphabetic, 1, $A};
tokenize("B") ->
    {alphabetic, 1, $B};
tokenize("C") ->
    {alphabetic, 1, $C};
tokenize("D") ->
    {alphabetic, 1, $D};
tokenize("E") ->
    {alphabetic, 1, $E};
tokenize("F") ->
    {alphabetic, 1, $F};
tokenize("G") ->
    {alphabetic, 1, $G};
tokenize("H") ->
    {alphabetic, 1, $H};
tokenize("I") ->
    {alphabetic, 1, $I};
tokenize("J") ->
    {alphabetic, 1, $J};
tokenize("K") ->
    {alphabetic, 1, $K};
tokenize("L") ->
    {alphabetic, 1, $L};
tokenize("M") ->
    {alphabetic, 1, $M};
tokenize("N") ->
    {alphabetic, 1, $N};
tokenize("O") ->
    {alphabetic, 1, $O};
tokenize("P") ->
    {alphabetic, 1, $P};
tokenize("Q") ->
    {alphabetic, 1, $Q};
tokenize("R") ->
    {alphabetic, 1, $R};
tokenize("S") ->
    {alphabetic, 1, $S};
tokenize("T") ->
    {alphabetic, 1, $T};
tokenize("U") ->
    {alphabetic, 1, $U};
tokenize("V") ->
    {alphabetic, 1, $V};
tokenize("W") ->
    {alphabetic, 1, $W};
tokenize("X") ->
    {alphabetic, 1, $X};
tokenize("Y") ->
    {alphabetic, 1, $Y};
tokenize("Z") ->
    {alphabetic, 1, $Z};

tokenize([Elem]) ->
    {anyascii, Elem, 1}.
	    
    
