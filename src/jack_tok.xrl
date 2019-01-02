Definitions.


Rules.

%%
%% Comments
//.*\n                   : {token, {comment}}.
/\*['\n\ra-zA-Z0-9\s*.\t,;\-:/]*\*/ : {token, {comment}}.

%% keywords
class                    : {token, {keyword, list_to_atom(TokenChars)}}.
constructor              : {token, {keyword, list_to_atom(TokenChars)}}.
function                 : {token, {keyword, list_to_atom(TokenChars)}}.
method                   : {token, {keyword, list_to_atom(TokenChars)}}.
field                    : {token, {keyword, list_to_atom(TokenChars)}}.
static                   : {token, {keyword, list_to_atom(TokenChars)}}.
var                      : {token, {keyword, list_to_atom(TokenChars)}}.
int                      : {token, {keyword, list_to_atom(TokenChars)}}.
char                     : {token, {keyword, list_to_atom(TokenChars)}}.
boolean                  : {token, {keyword, list_to_atom(TokenChars)}}.
void                     : {token, {keyword, list_to_atom(TokenChars)}}.
true                     : {token, {keyword, list_to_atom(TokenChars)}}.
false                    : {token, {keyword, list_to_atom(TokenChars)}}.
null                     : {token, {keyword, list_to_atom(TokenChars)}}.
this                     : {token, {keyword, list_to_atom(TokenChars)}}.
let                      : {token, {keyword, list_to_atom(TokenChars)}}.
do                       : {token, {keyword, list_to_atom(TokenChars)}}.
if                       : {token, {keyword, list_to_atom(TokenChars)}}.
else                     : {token, {keyword, list_to_atom(TokenChars)}}.
while                    : {token, {keyword, list_to_atom(TokenChars)}}.
return                   : {token, {keyword, list_to_atom(TokenChars)}}.

%% symbols
[{}()\[\].,;+\-*/&|<>=~] : {token, {symbol, list_to_atom(TokenChars)}}.

%% identifier
[_a-zA-Z][_a-zA-Z0-9]*   : {token, {identifier, list_to_atom(TokenChars)}}.

%% Positive Integer
[0-9]+                   : {token, {integerConstant, token_integer(TokenChars)}}.

%% string
["][^"\n]*["]           : {token, {stringConstant, token_string(TokenChars)}}.

%% Space to ignore
\t                       : skip_token.
\n                       : skip_token.
\r                       : skip_token.
\s                       : skip_token.

Erlang code.

token_integer(TokenChars) ->
    I = list_to_integer(TokenChars),
    Valid = I >= 0 andalso I < 32768,
    case Valid of
        true ->
            I;
        false ->
            erlang:error("input integer out of bounds")
    end.

token_string(TokenChars) ->
    L = string:length(TokenChars),
    string:substr(TokenChars, 2, L-2).