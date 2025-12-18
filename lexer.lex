structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val pos = ref 0
val eof = fn () => Tokens.EOF(!pos, !pos)

%%

%header (functor SimpleMLLexFun(structure Tokens: SimpleML_TOKENS));

digit   = [0-9];
alpha   = [A-Za-z];
id      = {alpha}({alpha}|{digit}|_)*;
ws      = [\ \t\r]+;

%%

\n                  => (pos := (!pos) + 1; lex());
{ws}                => (lex());

"val"               => (Tokens.VAL(!pos, !pos));
"fun"               => (Tokens.FUNC(!pos, !pos));
"let"               => (Tokens.LET(!pos, !pos));
"in"                => (Tokens.IN(!pos, !pos));
"end"               => (Tokens.END(!pos, !pos));
"datatype"          => (Tokens.DATATYPE(!pos, !pos));
"type"              => (Tokens.TYPE(!pos, !pos));
"of"                => (Tokens.OF(!pos, !pos));
"unit"              => (Tokens.UNIT(!pos, !pos));
"int"               => (Tokens.INT(!pos, !pos));
"real"              => (Tokens.REAL(!pos, !pos));
"bool"              => (Tokens.BOOL(!pos, !pos));
"string"            => (Tokens.STRING(!pos, !pos));
"list"              => (Tokens.LIST(!pos, !pos));
"true"              => (Tokens.TRUE(!pos, !pos));
"false"             => (Tokens.FALSE(!pos, !pos));
"if"                => (Tokens.IF(!pos, !pos));
"then"              => (Tokens.THEN(!pos, !pos));
"else"              => (Tokens.ELSE(!pos, !pos));
"not"               => (Tokens.NOT(!pos, !pos));
"andalso"           => (Tokens.ANDALSO(!pos, !pos));
"orelse"            => (Tokens.ORELSE(!pos, !pos));
"div"               => (Tokens.DIV(!pos, !pos));
"mod"               => (Tokens.MOD(!pos, !pos));
"("                 => (Tokens.LPAREN(!pos, !pos));
")"                 => (Tokens.RPAREN(!pos, !pos));
"["                 => (Tokens.LBRACKET(!pos, !pos));
"]"                 => (Tokens.RBRACKET(!pos, !pos));
"{"                 => (Tokens.LBRACE(!pos, !pos));
"}"                 => (Tokens.RBRACE(!pos, !pos));
"|"                 => (Tokens.PIPE(!pos, !pos));
","                 => (Tokens.COMMA(!pos, !pos));
"::"                => (Tokens.CONS(!pos, !pos));
":"                 => (Tokens.COLON(!pos, !pos));
";"                 => (Tokens.SEMICOLON(!pos, !pos));
"->"                => (Tokens.RARROW(!pos, !pos));
"#"                 => (Tokens.HASHTAG(!pos, !pos));
"_"                 => (Tokens.UNDERSCORE(!pos, !pos));
"="                 => (Tokens.EQ(!pos, !pos));
"<>"                => (Tokens.NEQ(!pos, !pos));
"<="                => (Tokens.LESS_EQ(!pos, !pos));
"<"                 => (Tokens.LESS_OP(!pos, !pos));
">="                => (Tokens.GREATER_EQ(!pos, !pos));
">"                 => (Tokens.GREATER_OP(!pos, !pos));
"+"                 => (Tokens.PLUS(!pos, !pos));
"-"                 => (Tokens.MINUS(!pos, !pos));
"*"                 => (Tokens.STAR(!pos, !pos));
"/"                 => (Tokens.SLASH(!pos, !pos));
"."                 => (Tokens.DOT(!pos, !pos));
"@"                 => (Tokens.AT(!pos, !pos));
"^"                 => (Tokens.CARAT(!pos, !pos));

{id}                => (Tokens.ID (yytext, !pos, !pos));
{digit}+            => (Tokens.INT_VAL ((valOf (Int.fromString yytext), !pos, !pos)));
{digit}+\.{digit}+  => (Tokens.REAL_VAL ((valOf (Real.fromString yytext), !pos, !pos)));
\"[^\"]*\"          => (Tokens.STRING_VAL (String.substring (yytext, 1, String.size yytext - 2), !pos, !pos));

.                   => (raise Fail ("Illegal character: " ^ yytext));
