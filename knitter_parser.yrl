%%
%% $Author$
%% $Revision$
%%
%%
Nonterminals performative expression word character quotation commaExpression string stringchar ascii
   performative_1N white_expr_1N whit_commExp_1N stringchar_1N numeric_1N ascii_1N wordQuotStr.
Terminals alphabetic numeric special anyascii whitespace apos backslash '(' ')' '"' ',' '#' ':'.
Rootsymbol performative.
Endsymbol '$eop'.

performative	-> '(' word ')'.
performative	-> '(' word performative_1N ')'.
performative_1N	-> whitespace ':' word whitespace expression.
performative_1N	-> whitespace ':' word whitespace wordQuotStr.
performative_1N	-> performative_1N whitespace ':' word whitespace expression.
performative_1N	-> performative_1N whitespace ':' word whitespace wordQuotStr.
expression	-> '(' word ')'.
expression	-> '(' word white_expr_1N ')'.
wordQuotStr	-> word.
wordQuotStr	-> quotation.
wordQuotStr	-> string.
white_expr_1N	-> whitespace expression.
white_expr_1N	-> whitespace wordQuotStr.
white_expr_1N	-> white_expr_1N whitespace expression.
white_expr_1N	-> white_expr_1N whitespace wordQuotStr.
word		-> character.
word		-> character word.
character	-> alphabetic.
character	-> numeric.
character	-> special.
character	-> ':'.
quotation	-> apos expression.
quotation	-> apos commaExpression.
quotation	-> apos wordQuotStr.
commaExpression	-> ',' commaExpression '(' word ')'.
commaExpression	-> ',' commaExpression '(' whit_commExp_1N ')'.
whit_commExp_1N -> whitespace commaExpression.
whit_commExp_1N	-> whitespace wordQuotStr.
whit_commExp_1N	-> whit_commExp_1N whitespace commaExpression.
whit_commExp_1N	-> whit_commExp_1N whitespace wordQuotStr.
string		-> '"' '"'.
string		-> '"' stringchar_1N '"'.
string		-> '#' numeric '"'.
string		-> '#' numeric '"' ascii_1N.
string		-> '#' numeric numeric_1N '"'.
string		-> '#' numeric numeric_1N '"' ascii_1N.
stringchar_1N	-> stringchar.
stringchar_1N	-> stringchar_1N stringchar.
numeric_1N	-> numeric.
numeric_1N	-> numeric_1N numeric.
ascii_1N	-> ascii.
ascii_1N	-> ascii_1N ascii.
stringchar	-> backslash ascii.
stringchar	-> anyascii.
stringchar	-> alphabetic.
stringchar	-> numeric.
stringchar	-> special.
stringchar	-> whitespace.
stringchar	-> apos.
stringchar	-> '('.
stringchar	-> ')'.
stringchar	-> ','.
stringchar	-> '#'.
stringchar	-> ':'.
ascii		-> anyascii.
ascii		-> alphabetic.
ascii		-> numeric.
ascii		-> special.
ascii		-> whitespace.
ascii		-> apos.
ascii		-> backslash.
ascii		-> '('.
ascii		-> ')'.
ascii		-> '"'.
ascii		-> ','.
ascii		-> '#'.
ascii		->  ':'.
