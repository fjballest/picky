#include "syshdr.h"
#include "p.h"
#include "pam.h"
#include "y.tab.h"

double EPS = 0.000001;
Sym *bpred, *bsucc;
static Sym *bargval, *bargref;

/*
 * r -> real
 * f -> file
 * p -> pointer to any
 * s -> array[int] of char
 * c -> character
 * o -> ordinal
 * v -> lvalue
 * n -> node
 * Upcase means, by ref.
 */
static int barg[256] = {
	['b'] Tbool,
	['B'] Tbool,
	['c'] Tchar,
	['C'] Tchar,
	['f'] Tfile,
	['F'] Tfile,
	['p'] Tptr,
	['P'] Tptr,
	['r'] Treal,
	['R'] Treal,
};

static Sym*
xrealfn(Builtin *b, List *al, double (*fn)(double))
{
	Sym *n;
	double rval;

	if(evaluated(al->sym[0])){
		rval = al->sym[0]->rval;
		if((fn == log || fn == log10) && rval <= 0){
			diag("%s requires argument > 0", b->name);
			return nil;
		}
		if(fn == sqrt && rval < 0){
			diag("%s requires argument >= 0", b->name);
			return nil;
		}
		if((fn == asin || fn == acos) && (rval < -1+EPS || rval > 1-EPS)){
			diag("%s requires argument in [-1,1]", b->name);
			return nil;
		}
		if(fn == tan && cos(rval) < EPS){
			diag("value out of domain of %s", b->name);
			return nil;
		}
		if(fn == atan && (rval < -PI/2+EPS || rval > PI/2-EPS)){
			diag("%s requires argument in [-1,1]", b->name);
			return nil;
		}

		n = newreal(fn(rval), al->sym[0]->type);
		return n;
	}
	return nil;
}

static Sym*
xreal2fn(Builtin *b, List *al, double (*fn)(double, double))
{
	double r;

	USED(b);
	if(evaluated(al->sym[0]) && evaluated(al->sym[1])){
		r = fn(al->sym[0]->rval, al->sym[1]->rval);
		return newreal(r, al->sym[0]->type);
	}
	return nil;
}

static Sym*
xsin(Builtin *b, List *al)
{
	return xrealfn(b, al, sin);
}

static Sym*
xcos(Builtin *b, List *al)
{
	return xrealfn(b, al, cos);
}

static Sym*
xtan(Builtin *b, List *al)
{
	return xrealfn(b, al, tan);
}

static Sym*
xasin(Builtin *b, List *al)
{
	return xrealfn(b, al, asin);
}

static Sym*
xacos(Builtin *b, List *al)
{
	return xrealfn(b, al, acos);
}

static Sym*
xatan(Builtin *b, List *al)
{
	return xrealfn(b, al, atan);
}

static Sym*
xexp(Builtin *b, List *al)
{
	return xrealfn(b, al, exp);
}

static Sym*
xlog(Builtin *b, List *al)
{
	return xrealfn(b, al, log);
}

static Sym*
xlog10(Builtin *b, List *al)
{
	return xrealfn(b, al, log10);
}

static Sym*
xsqrt(Builtin *b, List *al)
{
	return xrealfn(b, al, sqrt);
}

static Sym*
xpow(Builtin *b, List *al)
{
	return xreal2fn(b, al, pow);
}

static Sym*
xatruntime(Builtin *b, List *al)
{
	USED(b);
	USED(al);
	return nil;
}

static Sym*
xpredsucc(Builtin *b, List *al, int inc)
{
	Sym *n, *arg;

	USED(b);
	arg = al->sym[0];
	if(evaluated(arg)){
		n = newint(arg->ival + inc, Oint, arg->type);
		if(n->ival < tfirst(n->type) || n->ival > tlast(n->type))
			diag("value out of range in call to pred/succ");
		return n;
	}
	return nil;
}

static Sym*
xpred(Builtin *b, List *al)
{
	return xpredsucc(b, al, -1);
}

static Sym*
xsucc(Builtin *b, List *al)
{
	return xpredsucc(b, al, +1);
}

static void
checkfargs(List *al)
{
	Type *t;

	if(al->nitems < 2)
		return;
	t = tderef(al->sym[1]->type);
	switch(t->op){
	case Tarry:
		if(tis(t->elem, Tchar))	/* strings are ok */
			return;
	case Tptr:
	case Tfile:
		diag("%ss cannot be used in read or write", topname[t->op]);
		break;
	}
}

static Sym*
xstdio(Builtin *b, List *al)
{
	Sym *n;

	switch(b->args[0]){
	case '<':
		addsym0(al, pstdin);
		break;
	case '>':
		addsym0(al, pstdout);
		break;
	default:
		return nil;
	}
	n = allocsym();
	n->stype = Sfcall;
	n->type = brtype(b, al);
	n->fargs = al;
	switch(b->id){
	case PBfeof:
		n->fsym = lookup("feof", Snone);
		break;
	case PBfeol:
		n->fsym = lookup("feol", Snone);
		break;
	case PBfpeek:
		n->fsym = lookup("fpeek", Snone);
		break;
	case PBfread:
		checkfargs(al);
		n->fsym = lookup("fread", Snone);
		break;
	case PBfreadln:
		checkfargs(al);
		n->fsym = lookup("freadln", Snone);
		break;
	case PBfreadeol:
		n->fsym = lookup("freadeol", Snone);
		break;
	case PBfwrite:
		checkfargs(al);
		n->fsym = lookup("fwrite", Snone);
		break;
	case PBfwriteln:
		checkfargs(al);
		n->fsym = lookup("fwriteln", Snone);
		break;
	case PBfwriteeol:
		n->fsym = lookup("fwriteeol", Snone);
		break;
	case PBfflush:
		n->fsym = lookup("fflush", Snone);
		break;
	default:
		sysfatal("file builtin bug: id %#ux", b->id);
	}
	return n;
}

static void
diagtypes(char *s, Type *t1, int top)
{
	diag("incompatible argument in call '%s'\n\t"
		"got %T; need %s", s, t1, topname[top]);
}

static Type*
mkbtype(Builtin *b)
{
	Type *t;
	int i;

	if(b->kind == Sproc)
		t = newtype(Tproc);
	else
		t = newtype(Tfunc);
	if(b->r != 0)
		if(b->r == 'o')
			t->rtype = tundef; /* ordinal actually */
		else
			t->rtype = newtype(b->r);

	/*
	 * result and argument types are not built for type checks
	 * they have exceptions and they are checked by bargcheck().
	 * Parameters are built only to know if they are ref or not,
	 * for setused().
	 */
	t->parms = newlist(Lsym);
	for(i = 0; b->args[i] != 0; i++)
		if(b->args[i] >= 'A' && b->args[i] <= 'Z')
			addsym(t->parms, bargref);
		else
			addsym(t->parms, bargval);
	return t;
}

/*
 * Generic argument check for builtins. see barg[]
 */
int
bargcheck(Builtin *b, List *al, char *as)
{
	int i, op;
	Sym *n;
	Type *t;

	if(as[0] == '<' || as[0] == '>')
		as++;
	for(i = 0; as[0] != 0 && i < al->nitems; i++){
		n = al->sym[i];
		if(n->type == nil || n->type == tundef){
			as++;
			continue;
		}
		switch(*as){
		case 's':
		case 'S':
			t = tderef(n->type);
			if((t->op != Tstr && t->op != Tarry) ||
			   !tis(t->idx, Tint) || !tis(t->elem, Tchar)){
				diag("argument to '%s' is not a string", b->name);
				return -1;
			}
			break;
		case 'o':
		case 'O':
			if(!tisord(n->type)){
				diag("argument to '%s' is not of ordinal type", b->name);
				return -1;
			}
			break;
		case 'v':
		case 'V':
		case 'n':
		case 'N':
			break;
		case 'r':
		case 'R':
		case 'f':
		case 'F':
		case 'p':
		case 'P':
		case 'c':
		case 'C':
			op = barg[*as];
			if(barg[*as] != 0 && !tis(n->type, op)){
				diagtypes(b->name, n->type, op);
				return -1;
			}
			break;
		default:
			sysfatal("xargcheck: bad type code '%c'", *as);
		}
		if(*as == 'v' || (*as >= 'A' && *as <= 'Z'))
			if(!islval(n)){
				diag("argument to '%s' not an l-lvalue", b->name);
				return -1;
			}
		as++;
	}
	if(i < al->nitems){
		diag("too many arguments in call to '%s'", b->name);
		return -1;
	}
	if(*as != 0){
		diag("too few arguments in call to '%s'", b->name);
		return -1;
	}
	return 0;
}

Type*
brtype(Builtin *b, List *al)
{
	switch(b->r){
	case 'r':
		return tcreal;
	case 'b':
		return tcbool;
	case '=':
		if(al->nitems > 0)
			return al->sym[0]->type;
		break;
	}
	return tundef;
}

static Builtin builtins[] = {
	{"acos",	PBacos,		Sfunc,	"r", 'r', xacos},
	{"asin",		PBasin,		Sfunc,	"r", 'r', xasin},
	{"atan",	PBatan,		Sfunc,	"r", 'r', xatan},
	{"close",	PBclose,	Sproc,	"f", 0, xatruntime},
	{"cos",		PBcos,		Sfunc,	"r", 'r', xcos},
	{"dispose",	PBdispose,	Sproc,	"P", 0, xatruntime},
	{"eof",		PBfeof,		Sfunc,	"<", 'b', xstdio},
	{"eol",		PBfeol,		Sfunc,	"<", 'b', xstdio},
	{"exp",		PBexp,		Sfunc,	"r", 'r', xexp},
	{"fatal",	PBfatal,	Sproc,	"s", 0, xatruntime},
	{"feof",	PBfeof,		Sfunc,	"f", 'b', xatruntime},
	{"feol",	PBfeol,		Sfunc,	"f", 'b', xatruntime},
	{"fpeek",	PBfpeek,	Sproc,	"fC", 0, xatruntime},
	{"fread",	PBfread,	Sproc,	"fV", 0, xatruntime},
	{"freadeol",	PBfreadeol,	Sproc,	"f", 0, xatruntime},
	{"freadln",	PBfreadln,	Sproc,	"fV", 0, xatruntime},
	{"frewind",	PBfrewind,	Sproc,	"f", 0, xatruntime},
	{"fwrite",	PBfwrite,	Sproc,	"fn", 0, xatruntime},
	{"fwriteln",	PBfwriteln,	Sproc,	"fn", 0, xatruntime},
	{"fwriteeol",	PBfwriteeol,	Sproc,	"f", 0, xatruntime},
	{"fflush",	PBfflush,	Sproc,	"f", 0, xatruntime},
	{"log",		PBlog,		Sfunc,	"r", 'r', xlog},
	{"log10",	PBlog10,	Sfunc,	"r", 'r', xlog10},
	{"new",		PBnew,		Sproc,	"P", 0, xatruntime},
	{"open",	PBopen,		Sproc,	"Fss", 0, xatruntime},
	{"peek",	PBfpeek,	Sproc,	"<C", 0, xstdio},
	{"pow",		PBpow,		Sfunc,	"rr", 'r', xpow},
	{"pred",	PBpred,		Sfunc,	"o", '=', xpred},
	{"read",	PBfread,	Sproc,	"<V", 0, xstdio},
	{"readeol",	PBfreadeol,	Sproc,	"<", 0, xstdio},
	{"readln",	PBfreadln,	Sproc,	"<V", 0, xstdio},
	{"sin",		PBsin,		Sfunc,	"r", 'r', xsin},
	{"sqrt",	PBsqrt,		Sfunc,	"r", 'r', xsqrt},
	{"stack",	PBstack,	Sproc,	"", 0, xatruntime},
	{"data",	PBdata,		Sproc,	"", 0, xatruntime},
	{"succ",	PBsucc,		Sfunc,	"o", '=', xsucc},
	{"tan",		PBtan,		Sfunc,	"r", 'r', xtan},
	{"write",	PBfwrite,	Sproc,	">n", 0, xstdio},
	{"writeln",	PBfwriteln,	Sproc,	">n", 0, xstdio},
	{"writeeol",	PBfwriteeol,	Sproc,	">", 0, xstdio},
	{"flush",	PBfflush,	Sproc,	">", 0, xstdio},
};

void
builtininit(void)
{
	int i;
	Sym *s;

	nofperr();
	bargval = defsym("$bav", Svar);
	bargval->op = Oparm;
	bargref = defsym("$bar", Svar);
	bargref->op = Orefparm;
	bargval->type = bargref->type = tundef;

	for(i = 0; i < nelem(builtins); i++){
		s = defsym(builtins[i].name, builtins[i].kind);
		s->prog = allocprog();
		s->prog->psym = s;
		s->prog->b = &builtins[i];
		s->type = mkbtype(&builtins[i]);
		s->prog->parms = s->type->parms;
		s->id = PAMbuiltin|builtins[i].id;
	}
	bpred = lookup("pred", Sfunc);
	bsucc = lookup("succ", Sfunc);
	assert(bpred != nil && bsucc != nil);
}
