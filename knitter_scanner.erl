-module(knitter_scanner).
-author('$Author$').
-vsn('$Revision$').

-export([scan/1]).
-export([tokenize/1]).


scan(Input) -> scan(Input, []).

scan(Input, Tokens) ->
    case tokenize(io:get_chars(Input, '', 1)) of
	{'$eop', Line} ->
	    lists:reverse([{'$eop', Line} | Tokens]);
	Token -> scan(Input, [Token | Tokens])
    end.

tokenize(eof) ->
    {'$eop', 1};
tokenize([]) ->
    {'$eop', 1};
tokenize("(") ->
    {'(', 1};
tokenize(")") ->
    {')', 1};
tokenize(" ") ->
    {whitespace, 1};
tokenize("\"") ->
    {'"', 1};
tokenize("\'") ->
    {apos, 1};
tokenize(",") ->
    {',', 1};
tokenize("#") ->
    {'#', 1};
tokenize("\\") ->
    {backslash, 1};
tokenize(":") ->
    {':', 1};

tokenize("<") ->
    {special, '<', 1};
tokenize(">") ->
    {special, '>', 1};
tokenize("=") ->
    {special, '=', 1};
tokenize("+") ->
    {special, '+', 1};
tokenize("-") ->
    {special, '-', 1};
tokenize("*") ->
    {special, '*', 1};
tokenize("/") ->
    {special, '/', 1};
tokenize("&") ->
    {special, '&', 1};
tokenize("^") ->
    {special, '^', 1};
tokenize("~") ->
    {special, '~', 1};
tokenize("_") ->
    {special, '_', 1};
tokenize("@") ->
    {special, '@', 1};
tokenize("\$") ->
    {special, dollar, 1};
tokenize("%") ->
    {special, '%', 1};
tokenize(".") ->
    {special, '.', 1};
tokenize("!") ->
    {special, '!', 1};
tokenize("?") ->
    {special, '?', 1};

tokenize("0") ->
    {numeric, '0', 1};
tokenize("1") ->
    {numeric, '1', 1};
tokenize("2") ->
    {numeric, '2', 1};
tokenize("3") ->
    {numeric, '3', 1};
tokenize("4") ->
    {numeric, '4', 1};
tokenize("5") ->
    {numeric, '5', 1};
tokenize("6") ->
    {numeric, '6', 1};
tokenize("7") ->
    {numeric, '7', 1};
tokenize("8") ->
    {numeric, '8', 1};
tokenize("9") ->
    {numeric, '9', 1};

tokenize("a") ->
    {alphabetic, 'a', 1};
tokenize("b") ->
    {alphabetic, 'b', 1};
tokenize("c") ->
    {alphabetic, 'c', 1};
tokenize("d") ->
    {alphabetic, 'd', 1};
tokenize("e") ->
    {alphabetic, 'e', 1};
tokenize("f") ->
    {alphabetic, 'f', 1};
tokenize("g") ->
    {alphabetic, 'g', 1};
tokenize("h") ->
    {alphabetic, 'h', 1};
tokenize("i") ->
    {alphabetic, 'i', 1};
tokenize("j") ->
    {alphabetic, 'j', 1};
tokenize("k") ->
    {alphabetic, 'k', 1};
tokenize("l") ->
    {alphabetic, 'l', 1};
tokenize("m") ->
    {alphabetic, 'm', 1};
tokenize("n") ->
    {alphabetic, 'n', 1};
tokenize("o") ->
    {alphabetic, 'o', 1};
tokenize("p") ->
    {alphabetic, 'p', 1};
tokenize("q") ->
    {alphabetic, 'q', 1};
tokenize("r") ->
    {alphabetic, 'r', 1};
tokenize("s") ->
    {alphabetic, 's', 1};
tokenize("t") ->
    {alphabetic, 't', 1};
tokenize("u") ->
    {alphabetic, 'u', 1};
tokenize("v") ->
    {alphabetic, 'v', 1};
tokenize("w") ->
    {alphabetic, 'w', 1};
tokenize("x") ->
    {alphabetic, 'x', 1};
tokenize("y") ->
    {alphabetic, 'y', 1};
tokenize("z") ->
    {alphabetic, 'z', 1};
tokenize("A") ->
    {alphabetic, 'A', 1};
tokenize("B") ->
    {alphabetic, 'B', 1};
tokenize("C") ->
    {alphabetic, 'C', 1};
tokenize("D") ->
    {alphabetic, 'D', 1};
tokenize("E") ->
    {alphabetic, 'E', 1};
tokenize("F") ->
    {alphabetic, 'F', 1};
tokenize("G") ->
    {alphabetic, 'G', 1};
tokenize("H") ->
    {alphabetic, 'H', 1};
tokenize("I") ->
    {alphabetic, 'I', 1};
tokenize("J") ->
    {alphabetic, 'J', 1};
tokenize("K") ->
    {alphabetic, 'K', 1};
tokenize("L") ->
    {alphabetic, 'L', 1};
tokenize("M") ->
    {alphabetic, 'M', 1};
tokenize("N") ->
    {alphabetic, 'N', 1};
tokenize("O") ->
    {alphabetic, 'O', 1};
tokenize("P") ->
    {alphabetic, 'P', 1};
tokenize("Q") ->
    {alphabetic, 'Q', 1};
tokenize("R") ->
    {alphabetic, 'R', 1};
tokenize("S") ->
    {alphabetic, 'S', 1};
tokenize("T") ->
    {alphabetic, 'T', 1};
tokenize("U") ->
    {alphabetic, 'U', 1};
tokenize("V") ->
    {alphabetic, 'V', 1};
tokenize("W") ->
    {alphabetic, 'W', 1};
tokenize("X") ->
    {alphabetic, 'X', 1};
tokenize("Y") ->
    {alphabetic, 'Y', 1};
tokenize("Z") ->
    {alphabetic, 'Z', 1};

tokenize([Elem]) ->
    {anyascii, Elem, 1}.
	    
    
