type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end

fun getEnd yypos yytext = yypos + (String.size yytext)
fun getInt yypos string = case (Int.fromString string) of (SOME i) => i | NONE => ErrorMsg.impossible "could not convert string to int"

fun tokenPos yypos yytext = (yypos, getEnd yypos yytext)
fun tokenPosVal yypos yytext = (yytext, yypos, getEnd yypos yytext)
fun intToken yypos yytext = (getInt yypos yytext, yypos, getEnd yypos yytext)

%% 
%%

[ \t]+ => (continue());

"while" => (Tokens.WHILE (tokenPos yypos yytext));
"for" => (Tokens.FOR (tokenPos yypos yytext));
"to" => (Tokens.TO (tokenPos yypos yytext));
"break" => (Tokens.BREAK (tokenPos yypos yytext));
"let" => (Tokens.LET (tokenPos yypos yytext));
"in" => (Tokens.IN (tokenPos yypos yytext));
"end" => (Tokens.END (tokenPos yypos yytext));
"function" => (Tokens.FUNCTION (tokenPos yypos yytext));
"var" => (Tokens.VAR (tokenPos yypos yytext));
"type" => (Tokens.TYPE (tokenPos yypos yytext));
"array" => (Tokens.ARRAY (tokenPos yypos yytext));
"if" => (Tokens.IF (tokenPos yypos yytext));
"then" => (Tokens.THEN (tokenPos yypos yytext));
"else" => (Tokens.ELSE (tokenPos yypos yytext));
"do" => (Tokens.DO (tokenPos yypos yytext));
"of" => (Tokens.OF (tokenPos yypos yytext));
"nil" => (Tokens.NIL (tokenPos yypos yytext));

\n	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());

","	=> (Tokens.COMMA (tokenPos yypos yytext));
":"	=> (Tokens.COLON (tokenPos yypos yytext));
";"	=> (Tokens.SEMICOLON (tokenPos yypos yytext));
"("	=> (Tokens.LPAREN (tokenPos yypos yytext));
")"	=> (Tokens.RPAREN (tokenPos yypos yytext));
"["	=> (Tokens.LBRACE (tokenPos yypos yytext));
"]"	=> (Tokens.RBRACE (tokenPos yypos yytext));
"{"	=> (Tokens.LBRACK (tokenPos yypos yytext));
"}"	=> (Tokens.RBRACK (tokenPos yypos yytext));
"."	=> (Tokens.DOT (tokenPos yypos yytext));
"+"	=> (Tokens.PLUS (tokenPos yypos yytext));
"-"	=> (Tokens.MINUS (tokenPos yypos yytext));
"*"	=> (Tokens.TIMES (tokenPos yypos yytext));
"/"	=> (Tokens.DIVIDE (tokenPos yypos yytext));
"="	=> (Tokens.EQ (tokenPos yypos yytext));
"<>"	=> (Tokens.NEQ (tokenPos yypos yytext));
"<"	=> (Tokens.LT (tokenPos yypos yytext));
"<="	=> (Tokens.LE (tokenPos yypos yytext));
">"	=> (Tokens.GT (tokenPos yypos yytext));
">="	=> (Tokens.GE (tokenPos yypos yytext));
"&"	=> (Tokens.AND (tokenPos yypos yytext));
"|"	=> (Tokens.OR (tokenPos yypos yytext));
":="	=> (Tokens.ASSIGN (tokenPos yypos yytext));

[a-zA-Z][a-zA-Z0-9_]* => (Tokens.ID (tokenPosVal yypos yytext));
[0-9]+ => (Tokens.INT (intToken yypos yytext));

.       => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());

