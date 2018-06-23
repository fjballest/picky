#include "syshdr.h"
#include "p.h"
#include "pam.h"
#include "y.tab.h"
#include "vers.h"

enum
{
	None = -2,
};

extern int yyparse(void);

int debug[256];
int globalsok;
static Biobuf *bin;
char sval[Maxsval];
static char *ap, *ep;

char *fname;
int lineno;
static long peekc = None;

static long
nextc(void)
{
	long c;

	if(peekc == Eof)
		return -1;
	if(peekc != None){
		c = peekc;
		peekc = None;
	}else{
		c = Bgetrune(bin);
		if(c == '\n')
			lineno++;
	}
	return c;
}

static void
putback(long c)
{
	peekc = c;
}


static long
Nextc(void)
{
	long c;
	int n;
	Rune r[2];

	c = nextc();
	n = runelen(c);
	if(ap < ep - n - 1){
		r[0] = (Rune)c;
		r[1] = 0;
		ap += runetochar(ap, r);
		*ap = 0;
	}
	return c;
}

static void
Putback(long c)
{
	int n;

	n = runelen(c);
	if(ap - n >= sval){
		ap -= n;
		*ap = 0;
	}
	putback(c);
}

static long
_yylex(void)
{
	long c;
	int isreal;
	Sym *s;
	static long savedtok = -1;

	if(savedtok != -1){
		c = savedtok;
		savedtok = -1;
		strcpy(sval, "..");	/* BUG: if savedtok != ".." */
		return c;
	}
Again:
	ap = sval;
	ep = sval + sizeof sval;
	*ap = 0;
	do
		c = nextc();
	while(isspace(c));
	if(c == Eof)
		return c;
	putback(c);
	Nextc(); /* save in sval */
	if(c >= Runeself || isalpha(c)){
		do{
			if(ep - ap < UTFmax)
				sysfatal("max word size exceeded");
			c = Nextc();
		}while(isalnum(c) || c == '_' || c > Runeself);
		Putback(c);
		s = keylookup(sval);
		yylval.sym = s;
		s->fname = fname;
		s->lineno = lineno;
		switch(s->stype){
		case Skey:
			return s->tok;
		case Stype:
			return TYPEID;
		default:
			return ID;
		}
	}
	if(isdigit(c)){
		do{
			c = Nextc();
		}while(isdigit(c));
		isreal = c == '.';
	Real:
		if(isreal){
			c = Nextc();
			if(c == '.'){
				savedtok = DOTDOT;
				yylval.ival = strtol(sval, nil, 10);
				return INT;
			}
			while(isdigit(c))
				c = Nextc();
		}
		if(c == 'E' || c == 'e'){
			isreal = 1;
			c = Nextc();
			if(c == '+' || c == '-')
				c = Nextc();
			while(isdigit(c))
				c = Nextc();
		}
		Putback(c);
		if(isreal){
			yylval.rval = strtod(sval, nil);
			return REAL;
		}
		yylval.ival = strtol(sval, nil, 10);
		return INT;
	}
	switch(c){
	case '[':
	case ']':
	case ',':
	case '(':
	case ')':
	case '{':
	case '}':
	case '+':
	case '^':
	case ':':
	case ';':
	case '%':
		return c;
	case '=':
		c = Nextc();
		if(c == '=')
			return EQ;
		Putback(c);
		return '=';
	case '*':
		c = Nextc();
		if(c == '*')
			return POW;
		Putback(c);
		return '*';
	case '\"':
		ap = sval;
		*ap = 0;
		for(;;){
			c = nextc();
			if(c == '\"' || c == Eof)
				break;
			if(ap < ep - 1){
				if(runelen(c) > 1)
					diag("rune '%C' does not fit in a char", (Rune)c);
				else if(c == '\n')
					diag("newline in string");
				else
					*ap++ = c;
			}else
				sysfatal("BUG: sval too small");
		}
		*ap = 0;
		if(c == Eof){
			diag("missing closing '\"'");
			return Eof;
		}
		s = strlookup(sval);
		yylval.sval = s->name;
		return STR;
	case '\'':
		c = Nextc();
		if(c == Eof){
			diag("missing closing \"'\"");
			return Eof;
		}
		if(Nextc() != '\'')
			diag("missing closing \"'\"");
		if(runelen(c) > 1)
			diag("rune '%C' does not fit in a char", (Rune)c);
		yylval.ival = c;
		return CHAR;
	case '.':
		c = Nextc();
		if(c == '.')
			return DOTDOT;
		Putback(c);
		if(isdigit(c)){
			isreal = 1;
			goto Real;
		}
		return '.';
	case '>':
		c = Nextc();
		if(c == '=')
			return GE;
		Putback(c);
		return '>';
	case '!':
		c = Nextc();
		if(c != '='){
			diag("missing '=' after '!'");
			Putback(c);
			return BADOP;
		}
		return NE;
	case '<':
		c = Nextc();
		if(c == '=')
			return LE;
		Putback(c);
		return '<';
	case '/':
		c = Nextc();
		if(c == '*'){
			do{
				c = nextc();
				if(c == '*'){
					c = nextc();
					if(c == '/')
						break;
				}
			}while(c != Eof);
			if(c == Eof){
				diag("missing end of comment");
				return Eof;
			}
			goto Again;
		}
		Putback(c);
		return '/';
	case '-':
		c = Nextc();
		if(c == '>')
			return '^';
		Putback(c);
		return '-';
	case Eof:
		peekc = Eof;
		return Eof;
	default:
		diag("bad character '%C'0x%ulx in input", (Rune)c, c);
		goto Again;
	}
}

long
yylex(void)
{
	long c;

	c = _yylex();
	if(debug['L'])
		switch(c){
		case OR: fprint(2,"OR\n"); break;
		case AND: fprint(2,"AND\n"); break;
		case EQ: fprint(2,"EQ\n"); break;
		case NE: fprint(2,"NE\n"); break;
		case LE: fprint(2,"LE\n"); break;
		case GE: fprint(2,"GE\n"); break;
		case '<': fprint(2,"LT\n"); break;
		case '>': fprint(2,"GT\n"); break;
		case '+': fprint(2,"PLUS\n"); break;
		case '-': fprint(2,"MINUS\n"); break;
		case '*': fprint(2,"STAR\n"); break;
		case POW: fprint(2,"POW\n"); break;
		case '/': fprint(2,"SLASH\n"); break;
		case '%': fprint(2,"MOD\n"); break;
		case '[': fprint(2,"LBRAC\n"); break;
		case '(': fprint(2,"LPAREN\n"); break;
		case '{': fprint(2,"LCURL\n"); break;
		case ']': fprint(2,"RBRAC\n"); break;
		case ')': fprint(2,"RPAREN\n"); break;
		case '}': fprint(2,"RCURL\n"); break;
		case '=': fprint(2,"ASSIGN\n"); break;
		case ',': fprint(2,"COMMA\n"); break;
		case ':': fprint(2,"COLON\n"); break;
		case ';': fprint(2,"SEMICOLON\n"); break;
		case '.': fprint(2,"DOT\n"); break;
		case '^': fprint(2,"ARROW\n"); break;
		case ARRAY: fprint(2,"ARRAY\n"); break;
		case CASE: fprint(2,"CASE\n"); break;
		case CHAR: fprint(2,"CHAR '%C'\n", (Rune)yylval.ival); break;
		case CONSTS: fprint(2,"CONSTS\n"); break;
		case DEFAULT: fprint(2,"DEFAULT\n"); break;
		case DO: fprint(2,"DO\n"); break;
		case DOTDOT: fprint(2,"DOTDOT\n"); break;
		case ELSE: fprint(2,"ELSE\n"); break;
		case FALSE: fprint(2,"FALSE\n"); break;
		case FOR: fprint(2,"FOR\n"); break;
		case FUNCTION: fprint(2,"FUNCTION\n"); break;
		case ID: fprint(2,"ID '%s'\n", yylval.sym->name); break;
		case IF: fprint(2,"IF\n"); break;
		case INT: fprint(2,"INT %ld\n", yylval.ival); break;
		case SWITCH: fprint(2,"SWITCH\n"); break;
		case NIL: fprint(2,"NIL\n"); break;
		case NOT: fprint(2,"NOT\n"); break;
		case OF: fprint(2,"OF\n"); break;
		case PROCEDURE: fprint(2,"PROCEDURE\n"); break;
		case PROGRAM: fprint(2,"PROGRAM\n"); break;
		case REAL: fprint(2,"REAL %2g\n", yylval.rval); break;
		case RECORD: fprint(2,"RECORD\n"); break;
		case REF: fprint(2,"REF\n"); break;
		case RETURN: fprint(2,"RETURN\n"); break;
		case STR: fprint(2,"STR \"%s\"\n", yylval.sval); break;
		case TRUE: fprint(2,"TRUE\n"); break;
		case TYPEID: fprint(2,"TYPEID '%s'\n", yylval.sym->name); break;
		case TYPES: fprint(2,"TYPES\n"); break;
		case VARS: fprint(2,"VARS\n"); break;
		case WHILE: fprint(2,"WHILE\n"); break;
		case Eof: fprint(2,"EOF\n"); break;
		case BADOP: fprint(2,"BADOP\n"); break;
		case LEN: fprint(2, "LEN\n"); break;
		default:
			sysfatal("bad tok %ld", c);
		}

	return c;
}

static Biobuf*
mkout(char *n)
{
	Biobuf *out;

	remove(n);
	out = Bopen(n, OWRITE);
	if(out == nil)
		sysfatal("%s: %r", n);
	return out;
}

static void
dumpstats(void)
{
	fprint(2, "%s stats:\n", argv0);
	fprint(2, "envsz:\t%d bytes\n", sizeof(Env));
	fprint(2, "symsz:\t%d bytes\n", sizeof(Sym));
	fprint(2, "typesz:\t%d bytes\n", sizeof(Type));
	fprint(2, "stmtsz:\t%d bytes\n", sizeof(Stmt));
	fprint(2, "nenvs:\t%uld\n", stats.nenvs);
	fprint(2, "menvs:\t%uld\t%uld bytes\n",
		stats.menvs, stats.menvs*sizeof(Env));
	fprint(2, "nsyms:\t%uld\t%uld bytes\n",
		stats.nsyms, stats.nsyms*sizeof(Sym));
	fprint(2, "nexpr:\t%uld\n", stats.nexpr);
	fprint(2, "nlists:\t%uld\t%uld bytes\n", stats.nlists,
		stats.nlists*(sizeof(List) + stats.mlist * sizeof(void*)));
	fprint(2, "mlist:\t%uld\n", stats.mlist);
	fprint(2, "nstmts:\t%uld\n", stats.nstmts);
	fprint(2, "nprogs:\t%uld\n", stats.nprogs);
	fprint(2, "ntypes:\t%uld\n", stats.ntypes);
	fprint(2, "nstrs:\t%uld\n", stats.nstrs);
	fprint(2, "nhash:\t%uld\n", stats.nhash);
	fprint(2, "nlink:\t%uld\n", stats.nlink);
	fprint(2, "mlink:\t%uld\n", stats.mlink);
	fprint(2, "\n");
}

/*
 * Would be called if we call nofaults().
 * This is currently used by pam, but not by pick.
 */
void
faulted(char *s)
{
	sysfatal(s);
}

static void
usage(void)
{
	fprint(2, "usage: %s [-gSDLEPv] [-o out] file\n", argv0);
	exits("usage");
}

void
main(int argc, char *argv[])
{
	int i, sflag;
	char *oname;
	Biobuf *out;

	oname = "out.pam";
	sflag = 0;
	ARGBEGIN{
	case 'D':	/* declarations */
	case 'L':	/* lex */
	case 'E':	/* expressions */
	case 'P':	/* program */
		debug[ARGC()]++;
		break;
	case 'S':
		sflag++;
		break;
	case 'g':
		globalsok = 1;
		break;
	case 'o':
		oname=EARGF(usage());
		break;
	case 'v':
		print("%s: version %s\n", argv0, VERS);
		exits(nil);
	default:
		usage();
	}ARGEND;
	if(argc != 1)
		usage();

	quotefmtinstall();
	fmtinstall('N', Nfmt);
	fmtinstall('T', Tfmt);
	fmtinstall('X', Xfmt);
	fmtinstall('I', Ifmt);
	fmtinstall('R', Rfmt);

	pushenv();	/* top-level */
	syminit();
	typeinit();
	builtininit();

	for(i = 0; i < argc; i++){
		fname = argv[i];
		lineno = 1;
		bin = Bopen(argv[i], OREAD);
		if(bin == nil)
			sysfatal("%s: %r\n", argv[i]);
		yyparse();
		Bterm(bin);
	}
	if(sflag)
		dumpstats();
	checkundefs();
	if(nerrors>0)
		exits("errors");
	out = mkout(oname);
	gen(out, oname);
	Bterm(out);
	chmodx(oname);
	exits(nil);
}
