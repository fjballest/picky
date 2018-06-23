%{
/*
 * Px grammar.
 */

#include "syshdr.h"
#include "p.h" 
#include "pam.h"

%}
%union {
	int	bval;
	char*	sval;
	long	ival;
	double	rval;
	Sym*	sym;
	List*	list;
	Stmt*	stmt;
	Type*	type;
	Env*	env;
}

%left	','
%left	OR AND
%left	EQ NE LE GE '<' '>' BADOP
%left	'+' '-'
%left	'*' '/' '%'
%left	POW
%left	DOTDOT

/*	|sed 's/%token//' | sed 's/[ 	]/\n/g' | sort -u |fmt -l 50|t+
 */
%token
	ARRAY CASE  CONSTS DEFAULT DO  ELSE
	FOR FUNCTION IF SWITCH NIL NOT OF
	PROCEDURE PROGRAM RECORD REF RETURN
	TYPES VARS WHILE LEN
%token	<ival>	INT CHAR
%token	<sval>	STR
%token	<rval>	REAL
%token	<bval>	TRUE FALSE
%token	<sym>	ID TYPEID 

/* not a token; used as a Stmt op */
%token	FCALL

%type	<stmt>	stmt body assignstmt casestmt forstmt ifstmt procstmt
%type	<stmt>	repeatstmt retstmt whilestmt case ifhdr nullstmt

%type	<type>	type newtype ordtype ptrtype rangetype funcret
%type	<type>	proctype functype recordtype arraytype recordfields
%type	<list>	args ids fields parms optparms stmts optargs cases swfields swcases swcase
%type	<sym>	field prochdr funchdr expr primary lvalue forcond cexpr parm lenarg
%type	<ival>	forop

%%
prog:
	proghdr decls
	{
		if(env->prev != nil)
			sysfatal("env stack not empty; missing popenv()");
		if(debug['P'])
			dumpprog(2, env->prog);
	}


proghdr:
|
	ID ID ';'
	{
		env->prog = newprog($2);
		sysfatal("'program' expected, found %s", $1->name);
	}
|
	PROGRAM ID ';'
	{
		env->prog = newprog($2);
	}
|
	PROGRAM ID error
	{
		env->prog = newprog($2);
		yyerror("';' missing after program name");
	}
|	error ';'
	{
		sysfatal("not a picky program");
	}

decls:
	decls decl
|
	decl

decl:
	CONSTS ':' consts
|
	TYPES ':' types
|
	VARS ':' vars
	{
		if(globalsok == 0)
			diag("global variables are not allowed");
	}
|
	procdef
|
	funcdef

consts:
	consts constdef
|
	constdef

types:
	types typedef
|
	typedef

vars:
	vars vardef
|
	vardef

constdef:
	ID '=' expr ';'
	{
		declconst($1, $3);
	}
|
	error ';'
	{
		diag("constant declaration expected");
		yyerrok;
	}

typedef:
	ID '=' type ';'
	{
		decltype($1, $3);
	}
|
	TYPEID '=' type ';'
	{
		decltype($1, $3);
	}
|
	error ';'
	{
		diag("type declaration expected");
		yyerrok;
	}


type:
	TYPEID
	{
		$$ = $1->type;
	}
|
	newtype

vardef:
	ID ':' TYPEID ';'
	{
		declvar($1, $3->type);
	}
|
	TYPEID ':' type ';'
	{
		diag("'%s' is a type name", $1->name);
	}
|
	ID ':' ID ';'
	{
		diag("type name expected; found '%s'", $3->name);
	}
|
	error ';'
	{
		diag("var declaration expected");
		yyerrok;
	}

procdef:
	prochdr ';'
	{
		declprogdone(env->prog);
		popenv();
	}
|
	prochdr optvars body
	{
		if(env->prog == nil)
			sysfatal("missing program declaration");
		env->prog->prog->stmt = $3;
		declprogdone(env->prog);
		popenv();
	}

optvars:
	vars
|
	/* empty */

prochdr:
	PROCEDURE ID
	{
		declproc($2);
	}
	procparms
	{
		$$ = env->prog;
	}

procparms:
	'(' optparms ')'
	{
		if(env->prog == nil)
			sysfatal("missing program declaration");
		env->prog->prog->parms = $2;
	}
|	/* empty */
	{
		diag("missing '()'");
	}

funcdef:
	funchdr ';'
	{
		declprogdone(env->prog);
		popenv();
	}
|
	funchdr optvars body
	{
		if(env->prog == nil)
			sysfatal("missing program declaration");
		env->prog->prog->stmt = $3;
		declprogdone(env->prog);
		popenv();
	}

funchdr:
	FUNCTION ID
	{
		declfunc($2);
	}
	'(' optparms ')' ':' funcret
	{
		if(env->prog == nil)
			sysfatal("missing program declaration");
		$$ = env->prog;
		$$->prog->parms = $5;
		$$->prog->rtype = $8;
	}

optparms:
	parms
|
	/*empty*/
	{
		$$ = newlist(Lsym);
	}

parms:
	parms ',' parm
	{
		$$ = $1;
		if($1 != nil)
			addsym($1, $3);
		yyerrok;
	}
|
	parms ';' parm
	{
		diag("',' expected; not ';'");
		$$ = $1;
		if($1 != nil)
			addsym($1, $3);
	}
|
	parm
	{
		$$ = newlist(Lsym);
		addsym($$, $1);
	}
|
	parms error
	{
		$$ = newlist(Lsym);
	}
|
	parms error parm
	{
	$$ = newlist(Lsym);
		yyerrok;
	}
|
	parms ',' error
	{
		$$ = newlist(Lsym);
	}
|
	error
	{
		$$ = nil;
	}

parm:
	ID ':' TYPEID
	{
		$$ = newparm($1, $3->type, 0);
	}
|
	ID ':' ID
	{
		diag("'%s' is not a type name", $3->name);
		$$ = newparm($1, tundef, 0);
	}
|
	REF ID ':' TYPEID
	{
		$$ = newparm($2, $4->type, 1);
	}
|
	REF ID ':' ID
	{
		diag("'%s' is not a type name", $4->name);
		$$ = newparm($2, tundef, 0);
	}
|
	TYPEID ':' TYPEID
	{
		diag("type name '%s' is an invalid parameter name", $1->name);
		$$ = newparm($1, $3->type, 0);
	}
|
	REF TYPEID ':' TYPEID
	{
		diag("type name '%s' is an invalid parameter name", $2->name);
		$$ = newparm($2, $4->type, 1);
	}

newtype:
	ordtype
|
	rangetype
|
	ptrtype
|
	proctype
|
	functype
|
	arraytype
|
	recordtype

ptrtype:
	'^' TYPEID
	{
		$$ = newtype(Tptr);
		$$->ref = $2->type;
	}
|
	'^' ID
	{
		Sym *ft;

		ft = decltype($2, nil);
		$$ = newtype(Tptr);
		$$->ref = ft->type;
	}

ids:
	ids ',' ID
	{
		$$ = $1;
		if($$ != nil)
			addsym($$, $3);
		yyerrok;
	}
|
	ID
	{
		$$ = newlist(Lsym);
		if($$ != nil)
			addsym($$, $1);
	}
|
	error
	{
		diag("identifier expected");
		$$ = nil;
	}
|
	ids error
	{
		$$ = nil;
	}
|
	ids error ID
	{
		$$ = $1;
		yyerrok;
	}
|
	ids ',' error
	{
		$$ = nil;
	}


ordtype:
	'('  ids ')'
	{
		$$ = newordtype($2);
	}

rangetype:
	TYPEID expr DOTDOT expr
	{
		$$ = newrangetype($1->type, $2, $4);
	}

arraytype:
	ARRAY '[' TYPEID ']' OF TYPEID
	{
		$$ = newarrytype($3->type, $6->type);
	}
	|
	ARRAY '[' expr DOTDOT expr ']' OF TYPEID
	{
		$$ = newarrytype(newrangetype(nil, $3, $5), $8->type);
	}

recordtype:
	RECORD
	{
		Type *t = newtype(Trec);
		pushenv();
		env->rec = t;
	}
	recordfields
	{
		$$ = $3;
	}

recordfields:
	'{' fields '}'
	{
		$$ = env->rec;
		$$->fields = $2;
		popenv();
		initrectype($$);
	}
|
	error '}'
	{
		$$ = env->rec;
		$$->fields = newlist(Lsym);
		popenv();
		initrectype($$);
	}


fields:
	fields field
	{
		$$ = $1;
		if($$ != nil)
			addsym($$, $2);
	}
|
	fields swfields
	{
		$$ = $1;
		if($$ != nil)
			appsyms($$, $2);
	}
|
	field
	{
		$$ = newlist(Lsym);
		if($$ != nil)
			addsym($$, $1);
	}

field:
	ID ':' TYPEID ';'
	{
		$$ = declfield($1, $3->type);
	}

swfields:
	SWITCH '(' ID ')' '{' swcases '}'
	{
		setswfield($6, $3);
		$$ = $6;
	}

swcases:
	swcases swcase
	{
		$$ = $1;
		if($$ != nil)
			appsyms($$, $2);
	}
|
	swcase
	{
		$$ = $1;
	}

swcase:
	CASE cexpr ':' fields
	{
		setswval($4, $2);
		$$ = $4;
	}

proctype:
	PROCEDURE
	{
		pushenv();
	}
	'(' parms ')'
	{
		$$ = newtype(Tproc);
		$$->parms = $4;
		popenv();
	}

functype:
	FUNCTION
	{
		pushenv();
	}
	'(' parms ')' ':' funcret
	{
		$$ = newtype(Tfunc);
		$$->parms = $4;
		$$->rtype = $7;
		popenv();
	}

funcret:
	TYPEID
	{
		$$ = $1->type;
	}
|
	ID
	{
		diag("type name expected; found '%s'", $1->name);
		$$ = tundef;
	}
body:
	'{' stmts '}'
	{
		$$ = newbody($2);
	}
|
	'{' '}'
	{
		diag("empty block");
		$$ = newbody(newlist(Lstmt));
	}

stmts:
	stmts stmt
	{
		$$ = $1;
		addstmt($$, $2);
	}
|
	stmt
	{
		$$ = newlist(Lstmt);
		addstmt($$, $1);
	}

stmt:
	assignstmt 
|
	procstmt 
|
	body 
|
	casestmt 
|
	repeatstmt 
|
	ifstmt 
|
	whilestmt 
|
	forstmt
|
	retstmt
|
	nullstmt
|
	error ';'
	{
		$$ = newstmt(0);
		diag("statement expected");
	}


nullstmt:
	';'
	{
		$$ = newstmt(';');
	}

retstmt:
	RETURN expr ';'
	{
		$$ = newstmt(RETURN);
		$$->expr = $2;
		if(env->prog == nil)
			sysfatal("missing program declaration");
		env->prog->prog->nrets++;
	}

repeatstmt:
	DO body WHILE '(' expr ')' ';'
	{
		$$ = newstmt(DO);
		$$->expr = $5;
		$$->stmt = $2;
		cpsrc($$, $2);
		checkcond($$, $$->expr);
	}

whilestmt:
	WHILE '(' expr ')' body
	{
		$$ = newstmt(WHILE);
		$$->expr = $3;
		$$->stmt = $5;
		$$->sfname = $3->fname;
		$$->lineno = $3->lineno;
		checkcond($$, $$->expr);
	}

forstmt:
	FOR '(' lvalue '=' expr ',' forcond ')' body
	{
		$$ = newfor($3, $5, $7, $9);
		$$->sfname = $3->fname;
		$$->lineno = $3->lineno;
	}

forcond:
	ID forop expr
	{
		$$ = newexpr(Sbinary, $2, newvarnode($1), $3);
	}

forop:
	'<'
	{
		$$ = '<';
	}
|
	'>'
	{
		$$ = '>';
	}
|
	LE
	{
		$$ = Ole;
	}
|
	GE
	{
		$$ = Oge;
	}

ifstmt:
	ifhdr body
	{
		$$ = $1;
		$$->thenarm = $2;
	}
|
	ifhdr body ELSE body
	{
		$$ = $1;
		$$->thenarm = $2;
		$$->elsearm = $4;
		if($4->op == '{')
			$4->op = ELSE;
	}
|
	ifhdr body ELSE ifstmt
	{
		$$ = $1;
		$$->thenarm = $2;
		$$->elsearm = $4;
	}

ifhdr:
	IF '(' expr ')'
	{
		$$ = newstmt(IF);
		$$->cond = $3;
		checkcond($$, $$->cond);
	}

assignstmt:
	lvalue '=' expr ';'
	{
		$$ = newassign($1, $3);
	}
|
	lvalue ':' '='
	{
		diag("unexpected ':'");
	}
	expr ';'
	{
		$$ = newstmt(';');
	}

procstmt:
	ID '(' optargs ')' ';'
	{
		$$ = newstmt(FCALL);
		$$->fcall = newfcall($1, $3, Tproc);
	}

optargs:
	args
|
	/*empty*/
	{
		$$ = newlist(Lsym);
	}

args:
	args ',' expr
	{
		$$ = $1;
		if($$ != nil)
			addsym($$, $3);
	}
|
	expr
	{
		$$ = newlist(Lsym);
		addsym($$, $1);
	}

casestmt:
	SWITCH '(' expr ')' '{' cases '}'
	{
		$$ = newswitch($3, $6);
	}
cases:
	cases case
	{
		$$ = $1;
		addstmt($$, $2);
	}
|
	case
	{
		$$ = newlist(Lstmt);
		addstmt($$, $1);
	}

case:
	CASE cexpr ':' stmts
	{
		$$ = newstmt(CASE);
		$$->expr = $2;
		$$->stmt = newbody($4);
		cpsrc($$, $$->stmt);
	}
|
	DEFAULT ':' stmts
	{
		$$ = newstmt(CASE);
		$$->stmt = newbody($3);
	}

cexpr:
	cexpr ',' cexpr
	{
		$$ = newexpr(Sbinary, ',', $1, $3);
	}
|
	primary
	{
		if(!evaluated($1))
			diag("case expression must be a constant");
		$$ = $1;
	}
|
	primary DOTDOT primary
	{
		$$ = newexpr(Sbinary, Odotdot, $1, $3);
		if(!evaluated($1))
			diag("case expression must be a constant");
		if(!evaluated($3))
			diag("case expression must be a constant");
	}
expr:
	primary
|
	expr '+' expr
	{
		$$ = newexpr(Sbinary, '+', $1, $3);
	}
|
	expr '-' expr
	{
		$$ = newexpr(Sbinary, '-', $1, $3);
	}
|
	expr '*' expr
	{
		$$ = newexpr(Sbinary, '*', $1, $3);
	}
|
	expr '/' expr
	{
		$$ = newexpr(Sbinary, '/', $1, $3);
	}
|
	expr AND expr
	{
		$$ = newexpr(Sbinary, Oand, $1, $3);
	}
|
	expr OR expr
	{
		$$ = newexpr(Sbinary, Oor, $1, $3);
	}
|
	expr EQ expr
	{
		$$ = newexpr(Sbinary, Oeq, $1, $3);
	}
|
	expr NE expr
	{
		$$ = newexpr(Sbinary, One, $1, $3);
	}
|
	expr '%' expr
	{
		$$ = newexpr(Sbinary, '%', $1, $3);
	}
|
	expr '<' expr
	{
		$$ = newexpr(Sbinary, '<', $1, $3);
	}
|
	expr '>' expr
	{
		$$ = newexpr(Sbinary, '>', $1, $3);
	}
|
	expr GE expr
	{
		$$ = newexpr(Sbinary, Oge, $1, $3);
	}
|
	expr LE expr
	{
		$$ = newexpr(Sbinary, Ole, $1, $3);
	}
|
	expr POW expr
	{
		$$ = newexpr(Sbinary, Opow, $1, $3);
	}
|
	expr BADOP expr
	{
		$$ = nil;
	}

primary:
	lvalue
|
	'+' primary
	{
		$$ = $2;
	}
|
	'-' primary
	{
		$$ = newexpr(Sunary, Ouminus, $2, nil);
	}
|
	INT
	{
		$$ = newint($1, Oint, nil);
	}
|
	CHAR
	{
		$$ = newint($1, Ochar, nil);
	}
|
	REAL
	{
		$$ = newreal($1, nil);
	}
|
	STR
	{
		$$ = newstr($1);
	}
|
	NIL
	{
		$$ = newexpr(Sconst, Onil, nil, nil);
	}
|
	TRUE
	{
		$$ = newint(1, Otrue, nil);
	}
|
	FALSE
	{
		$$ = newint(0, Ofalse, nil);
	}
|
	ID '(' optargs ')'
	{
		$$ = newfcall($1, $3, Tfunc);
	}
|
	'(' expr ')'
	{
		$$ = $2;
	}
|
	NOT primary
	{
		$$ = newexpr(Sunary, Onot, $2, nil);
	}
|
	TYPEID '(' args ')'
	{
		$$ = newaggr($1->type, $3);
	}
|
	LEN lenarg
	{
		if($2->type == Tundef)
			diag("argument '%s' of len is undefined", $2->name);
		if(tisatom($2->type))
			$$ = newint(1, Oint, nil);
		else
			$$ = newint(tlen($2->type), Oint, nil);
	}

lenarg:
	TYPEID
|
	ID
|
	ID '[' expr ']'
	{
		$$ = newexpr(Sbinary, '[', $1, $3);
	}
|
	ID '.' ID
	{
		$$ = fieldaccess($1, $3->name);
	}
|
	ID '^'
	{
		$$ = newexpr(Sunary, '^', $1, nil);
	}
	

lvalue:
	ID
	{
		$$ = newvarnode($1);
	}
|
	lvalue '[' expr ']'
	{
		$$ = newexpr(Sbinary, '[', $1, $3);
	}
|
	lvalue '.' ID
	{
		$$ = fieldaccess($1, $3->name);
	}
|
	lvalue '.' TYPEID
	{
		diag("'%s' is a type name", $3->name);
		$$ = $1;
	}
|
	lvalue '^'
	{
		$$ = newexpr(Sunary, '^', $1, nil);
	}
|
	TYPEID
	{
		diag("'%s' is a type name", $1->name);
		$$ = newexpr(Snone, 0, nil, nil);
	}

%%

static void
puterror(char *fn, int ln, char *name, char *fmt, va_list arg)
{
	char *s1, *s2;

	if(name != nil)
		s1 = smprint("%s:%d: at '%s'", fn, ln, name);
	else
		s1 = smprint("%s:%d", fn, ln);
	s2 = vsmprint(fmt, arg);
	fprint(2, "%s: %s\n", s1, s2);
	free(s1);
	free(s2);
	if(nerrors > 10){
		fprint(2, "too many errors\n");
		exits("errors");
	}
}

void
diag(char* fmt, ...)
{
	va_list arg;

	nerrors++;
	va_start(arg, fmt);
	puterror(fname, lineno, nil, fmt, arg);
	va_end(arg);
}

void
yyerror(char* fmt, ...)
{
	va_list arg;

	nerrors++;
	va_start(arg, fmt);
	if(strcmp(fmt, "syntax error") == 0 && sval[0] != 0)
		puterror(fname, lineno, sval, "syntax error", arg);
	else
		puterror(fname, lineno, nil, fmt, arg);
	va_end(arg);
}

void
symerror(Sym *s, char* fmt, ...)
{
	va_list arg;

	nerrors++;
	va_start(arg, fmt);
	puterror(s->fname, s->lineno, s->name, fmt, arg);
	va_end(arg); 
}

void
stmterror(Stmt *s, char* fmt, ...)
{
	va_list arg;

	nerrors++;
	va_start(arg, fmt);
	puterror(s->sfname, s->lineno, nil, fmt, arg);
	va_end(arg); 
}
