%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% $Author$
%% $Revision$
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Nonterminals performative expression word character quotation commaExpression string stringchar ascii
   performative_1N white_expr_1N whit_commExp_1N stringchar_1N numeric_1N ascii_1N wordQuotStr.
Terminals alphabetic numeric special anyascii whitespace apos backslash '(' ')' '"' ',' '#' ':'.
Rootsymbol performative.
Endsymbol '$eop'.

performative	-> '(' word ')' 						: {knitter, [{'performative', '$2'}]}.
performative	-> '(' word performative_1N ')'					: {knitter, [{'performative', '$2'} | '$3']}.
performative_1N	-> whitespace ':' word whitespace expression			: [{list_to_atom('$3'), '$5'}].
performative_1N	-> whitespace ':' word whitespace wordQuotStr			: [{list_to_atom('$3'), '$5'}].
performative_1N	-> performative_1N whitespace ':' word whitespace expression	: '$1' ++ [{list_to_atom('$4'), '$6'}].
performative_1N	-> performative_1N whitespace ':' word whitespace wordQuotStr	: '$1' ++ [{list_to_atom('$4'), '$6'}].
expression	-> '(' word ')'							: [value_of('$1') | '$2'] ++ [value_of('$3')].
expression	-> '(' word white_expr_1N ')'					: [value_of('$1') | '$2'] ++ '$3' ++ [value_of('$4')].
wordQuotStr	-> word								: '$1'.
wordQuotStr	-> quotation							: '$1'.
wordQuotStr	-> string							: '$1'.
white_expr_1N	-> whitespace expression					: [value_of('$1') | '$2'].
white_expr_1N	-> whitespace wordQuotStr					: [value_of('$1') | '$2'].
white_expr_1N	-> white_expr_1N whitespace expression				: '$1' ++ [value_of('$2') | '$3'].
white_expr_1N	-> white_expr_1N whitespace wordQuotStr				: '$1' ++ [value_of('$2') | '$3'].
word		-> character							: ['$1'].
word		-> word character						: '$1' ++ ['$2'].
character	-> alphabetic							: value_of('$1').
character	-> numeric							: value_of('$1').
character	-> special							: value_of('$1').
character	-> ':'								: value_of('$1').
quotation	-> apos expression						: [value_of('$1') | '$2'].
quotation	-> apos commaExpression						: [value_of('$1') | '$2'].
quotation	-> apos wordQuotStr						: [value_of('$1') | '$2'].
commaExpression	-> ',' commaExpression '(' word ')'				: [value_of('$1') | '$2'].
commaExpression	-> ',' commaExpression '(' whit_commExp_1N ')'			: [value_of('$1') | '$2'] ++ [value_of('$3') | '$4'] ++ [value_of('$5')].
whit_commExp_1N -> whitespace commaExpression					: [value_of('$1') | '$2'].
whit_commExp_1N	-> whitespace wordQuotStr					: [value_of('$1') | '$2'].
whit_commExp_1N	-> whit_commExp_1N whitespace commaExpression			: '$1' ++ [value_of('$2') | '$3'].
whit_commExp_1N	-> whit_commExp_1N whitespace wordQuotStr			: '$1' ++ [value_of('$2') | '$3'].
string		-> '"' '"'							: [value_of('$1'), value_of('$2')].
string		-> '"' stringchar_1N '"'					: [value_of('$1') | '$2'] ++ [value_of('$3')].
string		-> '#' numeric '"'						: [value_of('$1') | '$2'] ++ [value_of('$3')].
string		-> '#' numeric '"' ascii_1N					: [value_of('$1') | '$2'] ++ [value_of('$3') | '$4'].
string		-> '#' numeric numeric_1N '"'					: [value_of('$1') | '$2'] ++ '$3' ++ [value_of('$4')].
string		-> '#' numeric numeric_1N '"' ascii_1N				: [value_of('$1') | '$2'] ++ '$3' ++ [value_of('$4') | '$5'].
stringchar_1N	-> stringchar							: ['$1'].
stringchar_1N	-> stringchar_1N stringchar					: '$1' ++ ['$2'].
numeric_1N	-> numeric							: value_of('$1').
numeric_1N	-> numeric_1N numeric						: '$1' ++ [value_of('$2')].
ascii_1N	-> ascii							: '$1'.
ascii_1N	-> ascii_1N ascii						: '$1' ++ '$2'.
stringchar	-> backslash ascii						: [value_of('$1') | '$2'].
stringchar	-> anyascii							: value_of('$1').
stringchar	-> alphabetic							: value_of('$1').
stringchar	-> numeric							: value_of('$1').
stringchar	-> special							: value_of('$1').
stringchar	-> whitespace							: value_of('$1').
stringchar	-> apos								: value_of('$1').
stringchar	-> '('								: value_of('$1').
stringchar	-> ')'								: value_of('$1').
stringchar	-> ','								: value_of('$1').
stringchar	-> '#'								: value_of('$1').
stringchar	-> ':'								: value_of('$1').
ascii		-> anyascii							: value_of('$1').
ascii		-> alphabetic							: value_of('$1').
ascii		-> numeric							: value_of('$1').
ascii		-> special							: value_of('$1').
ascii		-> whitespace							: value_of('$1').
ascii		-> apos								: value_of('$1').
ascii		-> backslash							: value_of('$1').
ascii		-> '('								: value_of('$1').
ascii		-> ')'								: value_of('$1').
ascii		-> '"'								: value_of('$1').
ascii		-> ','								: value_of('$1').
ascii		-> '#'								: value_of('$1').
ascii		->  ':'								: value_of('$1').

Erlang code.
value_of(Token) ->
	element(3, Token).