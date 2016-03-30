type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

val commentNestingLevel = ref 0
val stringPos = ref 0
val string = ref ""

fun checkNesting pos = if !commentNestingLevel > 0 then ErrorMsg.error pos "Expected '*/' but found EOF" else ()
fun checkString pos = if !string <> "" then ErrorMsg.error pos "Expected '\"' but found EOF" else ()
fun eof() = let val pos = hd(!linePos) in checkNesting pos; checkString pos; Tokens.EOF(pos,pos) end

fun getEnd yypos yytext = yypos + (String.size yytext)
fun getInt yypos string = case (Int.fromString string) of (SOME i) => i | NONE => ErrorMsg.impossible "could not convert string to int"

fun tokenPos yypos yytext = (yypos, getEnd yypos yytext)
fun tokenPosVal yypos yytext = (yytext, yypos, getEnd yypos yytext)
fun intToken yypos yytext = (getInt yypos yytext, yypos, getEnd yypos yytext)
fun stringToken () = let val s = !string val pos = !stringPos in string := ""; Tokens.STRING (s, pos, (getEnd pos s)) end

%% 
%s COMMENT STRING;
%%

<INITIAL>"/*" => (YYBEGIN COMMENT; commentNestingLevel := !commentNestingLevel + 1; continue());
<COMMENT>"/*" => (commentNestingLevel := !commentNestingLevel + 1; continue());
<COMMENT>"*/" => (commentNestingLevel := !commentNestingLevel - 1; if !commentNestingLevel = 0 then YYBEGIN INITIAL else (); continue());
<COMMENT>. => (continue());

<INITIAL>"\"" => (YYBEGIN STRING; stringPos := yypos + 1; continue());
<STRING>"\"" => (YYBEGIN INITIAL; stringToken ());
<STRING>. => (string := !string ^ yytext; continue());

<INITIAL>[ \t]+ => (continue());

<INITIAL>"while" => (Tokens.WHILE (tokenPos yypos yytext));
<INITIAL>"for" => (Tokens.FOR (tokenPos yypos yytext));
<INITIAL>"to" => (Tokens.TO (tokenPos yypos yytext));
<INITIAL>"break" => (Tokens.BREAK (tokenPos yypos yytext));
<INITIAL>"let" => (Tokens.LET (tokenPos yypos yytext));
<INITIAL>"in" => (Tokens.IN (tokenPos yypos yytext));
<INITIAL>"end" => (Tokens.END (tokenPos yypos yytext));
<INITIAL>"function" => (Tokens.FUNCTION (tokenPos yypos yytext));
<INITIAL>"var" => (Tokens.VAR (tokenPos yypos yytext));
<INITIAL>"type" => (Tokens.TYPE (tokenPos yypos yytext));
<INITIAL>"array" => (Tokens.ARRAY (tokenPos yypos yytext));
<INITIAL>"if" => (Tokens.IF (tokenPos yypos yytext));
<INITIAL>"then" => (Tokens.THEN (tokenPos yypos yytext));
<INITIAL>"else" => (Tokens.ELSE (tokenPos yypos yytext));
<INITIAL>"do" => (Tokens.DO (tokenPos yypos yytext));
<INITIAL>"of" => (Tokens.OF (tokenPos yypos yytext));
<INITIAL>"nil" => (Tokens.NIL (tokenPos yypos yytext));

\n	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());

<INITIAL>","	=> (Tokens.COMMA (tokenPos yypos yytext));
<INITIAL>":"	=> (Tokens.COLON (tokenPos yypos yytext));
<INITIAL>";"	=> (Tokens.SEMICOLON (tokenPos yypos yytext));
<INITIAL>"("	=> (Tokens.LPAREN (tokenPos yypos yytext));
<INITIAL>")"	=> (Tokens.RPAREN (tokenPos yypos yytext));
<INITIAL>"["	=> (Tokens.LBRACE (tokenPos yypos yytext));
<INITIAL>"]"	=> (Tokens.RBRACE (tokenPos yypos yytext));
<INITIAL>"{"	=> (Tokens.LBRACK (tokenPos yypos yytext));
<INITIAL>"}"	=> (Tokens.RBRACK (tokenPos yypos yytext));
<INITIAL>"."	=> (Tokens.DOT (tokenPos yypos yytext));
<INITIAL>"+"	=> (Tokens.PLUS (tokenPos yypos yytext));
<INITIAL>"-"	=> (Tokens.MINUS (tokenPos yypos yytext));
<INITIAL>"*"	=> (Tokens.TIMES (tokenPos yypos yytext));
<INITIAL>"/"	=> (Tokens.DIVIDE (tokenPos yypos yytext));
<INITIAL>"="	=> (Tokens.EQ (tokenPos yypos yytext));
<INITIAL>"<>"	=> (Tokens.NEQ (tokenPos yypos yytext));
<INITIAL>"<"	=> (Tokens.LT (tokenPos yypos yytext));
<INITIAL>"<="	=> (Tokens.LE (tokenPos yypos yytext));
<INITIAL>">"	=> (Tokens.GT (tokenPos yypos yytext));
<INITIAL>">="	=> (Tokens.GE (tokenPos yypos yytext));
<INITIAL>"&"	=> (Tokens.AND (tokenPos yypos yytext));
<INITIAL>"|"	=> (Tokens.OR (tokenPos yypos yytext));
<INITIAL>":="	=> (Tokens.ASSIGN (tokenPos yypos yytext));

<INITIAL>[a-zA-Z][a-zA-Z0-9_]* => (Tokens.ID (tokenPosVal yypos yytext));
<INITIAL>[0-9]+ => (Tokens.INT (intToken yypos yytext));

<INITIAL>.       => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());

