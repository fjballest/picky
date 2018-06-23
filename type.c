#include "syshdr.h"
#include "p.h"
#include "pam.h"
#include "y.tab.h"

/* constant, universal types */
Type *tcchar, *tcbool, *tcreal, *tcint, *tcnil;
/* predefined types */
Type *tchar, *tbool, *treal, *tundef, *tint, *tfile;

char *topname[] = {
	[Tundef]	"undef",
	[Tint]		"int",
	[Tbool]		"bool",
	[Tchar]		"char",
	[Treal]		"float",
	[Tenum]		"enum",
	[Trange]	"range",
	[Tarry]		"array",
	[Trec]		"record",
	[Tptr]		"ptr",
	[Tfile]		"file",
	[Tproc]		"procedure",
	[Tfunc]		"function",
	[Tprog]		"program",
	[Tfwd]		"forward",
	[Tstr]		"str",
};

static ulong topsz[] = {
	[Tundef]	0,
	[Tint]		sizeof(u32int),
	[Tbool]		sizeof(u32int),
	[Tchar]		sizeof(u32int),
	[Treal]		sizeof(u32int),
	[Tenum]		sizeof(u32int),
	[Tfile]		sizeof(u32int),
	[Tptr]		sizeof(u64int),
	[Tproc]		sizeof(u64int),
	[Tfunc]		sizeof(u64int),
};

Type*
newtype(int op)
{
	static Type *els;
	static int nels;
	Type *t;

	stats.ntypes++;
	if(nels == 0){
		nels = Aincr;
		els = emalloc(sizeof(Type)*nels);
	}
	nels--;
	t = els++;
	t->op = op;
	t->sz = topsz[t->op];
	t->id = ~0;
	switch(op){
	case Tbool:
		t->last = 1;
		break;
	case Tchar:
		t->last = 255;
		break;
	case Tint:
		assert(sizeof t->first == 4);
		t->first = -0x7FFFFFFFU + 1;
		assert(sizeof t->last == 4);
		t->last = 0x7FFFFFFFU;
		break;
	}
	return t;
}

Type*
tderef(Type *t)
{
	while(t != nil && t->op == Trange)
		t = t->super;
	if(t == nil)
		return tundef;
	return t;
}

int
tis(Type *t, int op)
{
	t = tderef(t);
	return t->op == op;
}

int
tisatom(Type *t)
{
	if(t == nil)
		return 1;
	return t->op != Trec && t->op != Tarry && t->op != Tstr;
}

int
tisord(Type *t)
{
	t = tderef(t);
	return t->op == Tint || t->op == Tbool || t->op == Tchar ||
		t->op == Tenum;
}

int
tiscmp(Type *t)
{
	if(tisord(t))
		return 1;
	t = tderef(t);
	return t->op == Treal || (t->op == Tarry && tiscmp(t->elem)) ||
		t->op == Tstr || t->op == Trec || t->op == Tptr;
}

long
tfirst(Type *t)
{
	return t->first;
}

long
tlast(Type *t)
{
	return t->last;
}

ulong
tsz(Type *t)
{
	return t->sz;
}

ulong
tlen(Type *t)
{
	switch(t->op){
	case Tbool:
	case Tchar:
	case Tenum:
	case Trange:
	case Tarry:
	case Tstr:
		return t->last - t->first + 1;
	case Trec:
		return t->fields->nitems;
	default:
		return 0;
	}
}

Type*
newarrytype(Type *idx, Type *elem)
{
	Type *t;
	ulong sz;
	t = newtype(Tarry);
	if(idx == nil)
		idx = tundef;
	if(elem == nil)
		elem = tundef;
	t->idx = idx;
	t->elem = elem;
	t->first = idx->first;
	t->last = idx->last;
	sz = tlen(t->idx);
	if(sz <= 0 || sz >= Maxidx){
		diag("array size is too small or too large");
		idx->last = idx->first;
		sz = 1;
	}
	t->sz = sz * t->elem->sz;
	if(t->sz <= 0 || t->sz >= Maxidx){
		diag("array size is too small or too large");
		idx->last = idx->first;
		t->sz = t->elem->sz;
	}
	return t;
}

Type*
newordtype(List *nl)
{
	int i;
	Type *t;
	Sym *s;

	if(nl == nil)
		return tundef;
	assert(nl->kind == Lsym);
	t = newtype(Tenum);
	t->lits = nl;
	t->last = nl->nitems - 1;
	for(i = 0; i < nl->nitems; i++){
		checkdup(nl->sym[i]);
		s = nl->sym[i] = defssym(nl->sym[i], Sconst);
		s->op = Olit;
		s->type = t;
		s->ival = i;
	}
	return t;
}

static Sym*
mklitcast(Type *t, long ival)
{
	Sym *r;

	r = newexpr(Sconst, Olit, nil, nil);
	r->ival = ival;
	r->type = t;
	return r;
}

/*
 * Valid casts are float(int|float), int(ordinal|float), and ordinal(int).
 */
static Sym*
newcast(Type *t, Sym *n)
{
	Sym *r;
	Type *dummy;

	if(t == nil || n == nil)
		return badnode;
	if(tcompat(t, n->type, &dummy)){
		diag("useless type cast (operand and result are type-compatible)");
		return n;
	}

	if(tis(t, Treal) && tis(n->type, Treal)){
		if(evaluated(n))
			return newreal(n->rval, t);
	}else if(tis(t, Treal) && n->type->op == Tint){
		if(evaluated(n))
			return newreal((double)n->ival, t);
	}else if(t->op == Tint && tis(n->type, Treal)){
		if(evaluated(n)){
			return newint((long)n->rval, Oint, t);
		}
	}else if(t->op == Tint && tisord(n->type)){
		if(evaluated(n))
			return newint(n->ival, Oint, t);
	}else if(tisord(t) && n->type->op == Tint){
		if(evaluated(n)){			
			if(t->op != Tint && (n->ival < t->first || n->ival > t->last)){
				diag("value out of range in type cast");
				return badnode;
			}
			if(tis(t, Tint))
				return newint(n->ival, Oint, t);
			if(tis(t, Tchar))
				return newint(n->ival, Ochar, t);
			if(tis(t, Tbool))
				return newint(n->ival, n->ival?Otrue:Ofalse, t);
			return mklitcast(t, n->ival);
		}
	}else{
		diag("invalid type cast");
		return badnode;
	}

	r = newexpr(Sunary, Ocast, n, nil);
	r->type = t;
	return r;
}

Sym*
newaggr(Type *t, List *nl)
{
	int n, i;
	Type *et, *dummy;
	Sym *s, *r;
	Env *e;
	static int agen;

	if(nl->nitems == 0)
		return badnode;
	switch(t->op){
	case Tarry:
		n = t->last - t->first + 1;
		break;
	case Trec:
		n = t->fields->nitems;
		break;
	default:
		if(tisatom(t) && nl->nitems == 1)
			return newcast(t, nl->items[0]);
		diag("can't define aggregates for this type");
		return badnode;
	}
	if(nl->nitems < n){
		diag("not enough elements in aggregate");
		return badnode;
	}
	if(nl->nitems > n){
		diag("too many elements in aggregate");
		return badnode;
	}
	for(i = 0; i < nl->nitems; i++){
		s = nl->sym[i];
		if(t->op == Tarry)
			et = t->elem;
		else
			et = t->fields->sym[i]->type;
		if(!tcompat(et, s->type, &dummy)){
				diag("incompatible type in aggregate element\n"
				 "\t('%T' expected; got '%T')", et, s->type);
				return badnode;
		}
		if(!evaluated(s)){
			diag("aggregate element must be a constant");
			return badnode;
		}
	}
	r = newexpr(Sconst, Oaggr, nil, nil);
	r->vals = nl;
	r->type = t;
	r->name = smprint("$a%d", agen++);
	for(e = env; e->prev != nil; e = e->prev)
		;
	addsym(e->prog->prog->consts, r);
	return r;
}

static int
rvalchk(Sym *n)
{
	evalexpr(n);
	if(n->stype == Sconst)
		switch(n->op){
		case Ochar:
		case Oint:
		case Otrue:
		case Ofalse:
		case Olit:
			return 0;
		}
	return -1;
}

/*
 * Super may be nil, if we are building an implicit range type
 * for array indexes. In this case, we define a type sym for the range.
 */
Type*
newrangetype(Type *super, Sym *v0, Sym *v1)
{
	static int rgen;
	Type *t, *st;
	Sym *s;
	Env *e;
	char name[50];

	if(rvalchk(v0) < 0 || rvalchk(v1) < 0){
		diag("range limits are not definite constants");
		return tundef;
	}
	if(v0->ival >= v1->ival){
		diag("empty range");
		return tundef;
	}
	if(super != nil && super->op == Tenum)
		if(v0->ival < tfirst(super) || v1->ival > tlast(super))
			diag("range limits are off limits");
	if(!tcompat(v0->type, v1->type, &st))
		diag("types not compatible in range");
	if(super != nil && !tcompat(st, super, &st))
		diag("range value types not compatible with super type\n"
			"\t%T vs. %T\n", st, super);

	t = newtype(Trange);
	t->super = tderef(st);
	t->first = v0->ival;
	t->last = v1->ival;
	t->sz = t->super->sz;

	if(super == nil){
		seprint(name, name + sizeof name, "$range%d", rgen++);
		s = defsym(name, Stype);
		t->id = tgen++;
		s->type = t;
		t->sym = s;
		for(e = env; e->prev != nil; e = e->prev)
			;
		addsym(e->prog->prog->types, s);
	}
	return t;
}

void
initrectype(Type *t)
{
	int i;
	List *tl;
	Sym *s;
	char *n;
	Type *tp;
	extern Sym* findfield(Type*, char*, char*);

	tl = t->fields;
	t->sz = 0;
	for(i = 0; i < tl->nitems; i++){
		if(tl->sym[i]->swfield != nil){
			n = tl->sym[i]->swfield->name;
			s = findfield(t, n, "switch");
			if(s != tl->sym[i]->swfield){
				diag("'%s' is not a field to switch on", n);
				return;
			}
			if(tl->sym[i]->swfield->swfield != nil){
				symerror(tl->sym[i]->swfield,
					"can't switch on a variant field");
				return;
			}
			if(nerrors > 0)
				continue;
			assert(tl->sym[i]->swval != nil);
			if(!tis(s->type, Tenum)){
				symerror(tl->sym[i]->swfield,
					"switch field is not an enumerated type");
				return;
			}
			if(!tcompat(s->type, tl->sym[i]->swval->type, &tp)){
				symerror(tl->sym[i]->swfield,
					"case value of the wrong type");
				return;
			}
		}
		s = findfield(t, tl->sym[i]->name, "dup");
		if(s != tl->sym[i])
			symerror(tl->sym[i], "dup field '%s'", s->name);
		tl->sym[i]->addr = t->sz;
		t->sz += tl->sym[i]->type->sz;
	}
}

/*
 * Handle universal type compatibility. Return:
 * 0: it's this type, but not compatible;
 * 1: it's compatible;
 * -1: not this type at all.
 */
static int
tuniv(Type *t0, Type *t1, Type *u, int op, Type **tp)
{
	if(t0 == u){
		if(tis(t1, op)){
			*tp = t1;
			return 1;
		}
		return 0;
	}
	if(t1 == u){
		if(tis(t0, op)){
			*tp = t0;
			return 1;
		}
		return 0;
	}
	return -1;
}

/*
 * Types are compatible if they are the same.
 * Universal bool, int, char, real, nil, and str are compatible
 * with any isomorphic type (in which case *tp is set to the
 * non-universal type, if any).
 */
int
tcompat(Type *t0, Type *t1, Type **tp)
{
	int u;
	Type *t;

	if(t0 == tundef || t1 == tundef){
		*tp = tundef;
		return 1;
	}
	t0 = tderef(t0);
	t1 = tderef(t1);
	if(t0 == t1){
		*tp = t0;
		return 1;
	}
	*tp = tundef;
	if((u = tuniv(t0, t1, tcint, Tint, tp)) >= 0)
		return u;
	if((u = tuniv(t0, t1, tcchar, Tchar, tp)) >= 0)
		return u;
	if((u = tuniv(t0, t1, tcbool, Tbool, tp)) >= 0)
		return u;
	if((u = tuniv(t0, t1, tcreal, Treal, tp)) >= 0)
		return u;
	if((u = tuniv(t0, t1, tcnil, Tptr, tp)) >= 0)
		return u;
	if(tis(t0, Tstr) || tis(t1, Tstr)){
		if(tis(t0, Tstr))
			t = t1;
		else
			t = t0;
		if(tis(t, Tarry) && tis(t->idx, Tint) && tis(t->elem, Tchar) &&
		   tlen(t0) == tlen(t1)){
			*tp = t;
			return 1;
		}
	}
	return 0; 
}

int
tchkunary(Sym *n)
{
	Type *lt;

	if(n->stype == Snone)
		return -1;
	if(n->stype != Sunary)
		sysfatal("tchkunary: stype %d", n->stype);
	if(n->type != nil)
		return 0;
	if(n->left->stype == Snone || n->left->type == nil)
		return -1;

	lt = n->left->type;
	switch(n->op){
	case '+':
	case Ouminus:
		if(tis(lt, Tint) || tis(lt, Treal))
			n->type = lt;
		else{
			diag("'%c' requires a numeric argument", n->op);
			goto Fail;
		}
		n->type = lt;
		break;
	case Onot:
		if(tis(lt, Tbool))
			n->type = lt;
		else{
			diag("'not' requires a %s argument", topname[Tbool]);
			goto Fail;
		}
		n->type = lt;
		break;
	case Ocast:
		/* arg types checked and n's type set by newcast */
		return 0;
	case '^':
		if(!tis(lt, Tptr)){
			diag("'^' requires a pointer as an argument");
			goto Fail;
		}
		n->type = tderef(lt)->ref;
		break;
	default:
		sysfatal("bad unary op '%s'", opname(n->op));
	}
	assert(n->type != nil);
	return 0;
Fail:
	n->stype = Snone;
	n->type = tundef;
	return -1;
}

int
tchkbinary(Sym *n)
{
	Type *dummy;
	Type *lt, *rt;

	if(n->stype == Snone)
		return -1;
	if(n->stype != Sbinary)
		sysfatal("tchkbinary: stype %d", n->stype);
	if(n->type != nil)
		return 0;

	if(n->left->stype == Snone || n->right->stype == Snone)
		goto Fail;

	lt = n->left->type;
	rt = n->right->type;
	switch(n->op){
	case '+':
	case '-':
	case '*':
	case '/':
	case Opow:
		/* check only the left arg. tcompat will ensure rt is ok. */
		if(!tis(lt, Tint) && !tis(lt, Treal)){
			diag("'%s' requires numeric arguments", opname(n->op));
			goto Fail;
		}
	Lchk:
		n->type = lt;
		if(!tcompat(lt, rt, &n->type)){
			diag("incompatible argument types (%T and %T) for op '%s'",
				lt, rt,  opname(n->op));
			goto Fail;
		}
		break;
	case '%':
		if(!tis(lt, Tint) || !tis(rt, Tint)){
			diag("'%s' requires int arguments", opname(n->op));
			goto Fail;
		}
		goto Lchk;
	case Oand:
	case Oor:
		if(!tis(lt, Tbool) || !tis(rt, Tbool)){
			diag("'%s' requires bool arguments", opname(n->op));
			goto Fail;
		}
		goto Lchk;
	case '<':
	case '>':
	case Ole:
	case Oge:
		/* check only the left arg. tcompat will ensure rt is ok. */
		if(!tisord(lt) && !tis(lt, Treal)){
			diag("'%s' requires numeric or ordinal arguments", opname(n->op));
			goto Fail;
		}
	Bchk:
		if(!tcompat(lt, rt, &dummy)){
			diag("incompatible argument types for op '%s'",
				opname(n->op));
			goto Fail;
		}
		n->type = tbool;
		break;
	case Oeq:
	case One:
		if(!tiscmp(lt) || !tiscmp(rt)){
			diag("'%s' not defined for this type", opname(n->op));
			goto Fail;
		}
		goto Bchk;
	case '[':
		if(!tis(lt, Tarry) && !tis(lt, Tstr)){
			diag("using '[]' requires an %s", topname[Tarry]);
			goto Fail;
		}
		if(!tcompat(tderef(lt)->idx, rt, &n->type)){
			diag("index type is not compatible");
			goto Fail;
		}
		n->type = lt->elem;
		break;
	case '.':
		sysfatal("tchkbinary: findfield must check this");
	case Odotdot:
		if(!tisord(lt) || !tisord(rt)){
			diag("'%s' requires an ordinal type", opname(n->op));
			goto Fail;
		}
		goto Lchk;
	case ',':
		if(n->left->op != ',' && n->left->op != Odotdot && !tisord(lt)){
			diag("case requires ordinal values");
			goto Fail;
		}
		if(n->right->op != ',' && n->right->op != Odotdot && !tisord(rt)){
			diag("case requires ordinal values");
			goto Fail;
		}
		goto Lchk;
	default:
		sysfatal("tchkbinary: bad op %d", n->op);
	}
	return 0;
Fail:
	n->stype = Snone;
	n->op = 0;
	n->type = tundef;
	return -1;
}

int
tchkcall(Sym *fs, List *args)
{
	List *parms;
	int i;
	Type *dummy;

	if(fs->stype == Snone || args == nil)
		return -1;
	if(fs->stype != Sproc && fs->stype != Sfunc){
		diag("'%s' is not a %s or %s.",
			fs->name, topname[Tproc], topname[Tfunc]);
		return -1;
	}
	if(fs->type->op == Tundef)
		return -1;

	if(fs->type->op != Tproc && fs->type->op != Tfunc){
		diag("'%s' is not a %s or %s.",
			fs->name, topname[Tproc], topname[Tfunc]);
		return -1;
	}
	parms = fs->prog->parms;
	if(parms == nil){
		diag("'%s' not fully defined", fs->name);
		return -1;
	}
	if(parms->nitems < args->nitems){
		diag("too many arguments for '%s'", fs->name);
		return -1;
	}
	if(parms->nitems > args->nitems){
		diag("not enough arguments for '%s'", fs->name);
		return -1;
	}
	for(i = 0; i < parms->nitems; i++){
		if(!tcompat(parms->sym[i]->type, args->sym[i]->type, &dummy)){
			diag("argument '%s' for '%s': incompatible types\n"
				"\n\texpected %T\n\tfound %T",
				parms->sym[i]->name, fs->name,
				parms->sym[i]->type, args->sym[i]->type);
			return -1;
		}
		if(parms->sym[i]->op == Orefparm && !islval(args->sym[i])){
			diag("argument '%s' for '%s': ref requires an l-value\n",
				parms->sym[i]->name, fs->name);
			return -1;
		}
	}
	return 0;
}

static Type*
mktype(char *name, int t)
{
	Sym *s;

	s = defsym(name, Stype);
	s->type = newtype(t);
	s->type->sym = s;
	return s->type;
}

void
typeinit(void)
{
	tundef = newtype(Tundef);
	badnode->type = tundef;
	tcbool = newtype(Tbool);
	tcchar = newtype(Tchar);
	tcint = newtype(Tint);
	tcreal = newtype(Treal);
	tbool = mktype(topname[Tbool], Tbool);
	tcbool->sym = tbool->sym;
	tchar = mktype(topname[Tchar], Tchar);
	tcchar->sym = tchar->sym;
	tint = mktype(topname[Tint], Tint);
	tcint->sym = tint->sym;
	treal = mktype(topname[Treal], Treal);
	tcreal->sym = treal->sym;
	tcnil = mktype("$nil", Tptr);
	tfile = mktype(topname[Tfile], Tfile);
}

void
declstdtypes(List *tl)
{
	char *nms[] = {"bool", "char", "int", "float", "$nil", "file"};
	Type *tps[] = {tcbool, tcchar, tcint, tcreal, tcnil, tfile};
	Sym *s;
	int i;

	for(i = 0; i < nelem(nms); i++){
		s = lookup(nms[i], Stype);
		addsym(tl, s);
		tps[i]->id = s->type->id = tgen++;
	}
}

char*
enumname(Type *t, long v)
{
	t = tderef(t);
	if(v < 0 || v >= t->lits->nitems)
		return "BAD";
	return t->lits->sym[v]->name;
}

static int
fmtargs(Fmt *f, Type *t)
{
	int i;
	List *pl;

	fmtprint(f, "(");
	pl = t->parms;
	for(i = 0; i < pl->nitems; i++){
		if(i > 0)
			fmtprint(f, ", ");
		if(pl->sym[i]->op == Orefparm)
			fmtprint(f, "ref ");
		fmtprint(f, "%T", pl->sym[i]->type);
	}
	return fmtprint(f, ")");
}

/*
 * Detect variables used and not set. Must be called for
 * each local variable with the body stmt.
 *
 * sweep x depth-first:
 * - if(x is set) skip children & following siblings
 * - if(x is used) issue diag and return
 *
 * return -1: diag made; 1: set by statement; 0: nop
 */
enum{Used = -1, Nop = 0, Set = 1};

static int
isset(Sym *lval, Sym *v)
{
	if(lval == v)
		return Set;
	switch(lval->stype){
	case Sunary:
		return isset(lval->left, v);
	case Sbinary:
		if(lval->op == '.' || lval->op == '[')
			return isset(lval->left, v);
		return isset(lval->left, v) || isset(lval->right, v);
	}
	return Nop;
}

static int setusedfcall(Sym*, Sym*);

static int
isused(Sym *expr, Sym *v)
{
	if(expr == v){
		symerror(expr, "variable '%s' used before set", v->name);
		return Used;
	}
	switch(expr->stype){
	case Sunary:
		return isused(expr->left, v);
	case Sbinary:
		return isused(expr->left, v) || isused(expr->right, v);
	case Sfcall:
		return setusedfcall(expr, v);
	}
	return Nop;
}

static int
setusedfcall(Sym *fcall, Sym *v)
{
	List *args;
	List *parms;
	int i;

	if(fcall->fsym == nil)
		return Nop;
	parms = fcall->fsym->prog->parms;
	args = fcall->fargs;

	for(i = 0; i < parms->nitems; i++)
		if(parms->sym[i]->op == Oparm)
			if(isused(args->sym[i], v))
				return Used;
	for(i = 0; i < parms->nitems; i++)
		if(parms->sym[i]->op == Orefparm)
			if(isset(args->sym[i], v))
				return Set;
	return Nop;
}

int
setslval(Stmt *x, Sym *v)
{
	int i;

	if(x == nil || v == nil)
		return 0;
	switch(x->op){
	case 0:
	case ';':
	case FCALL:
	case RETURN:
		break;
	case '{':
	case ELSE:
		if(x->list != nil)
			for(i = 0; i < x->list->nitems; i++)
				if(setslval(x->list->stmt[i], v))
					return 1;
		break;
	case IF:
		return setslval(x->thenarm, v) || setslval(x->elsearm, v);
	case '=':
		if(isset(x->lval, v)){
			stmterror(x, "can't assign to control variable '%s'",
				v->name);
			return 1;
		}
		break;
	case DO:
		return setslval(x->stmt, v);
	case WHILE:
	case FOR:
		/* We assume loops are entered once,
		 * otherwise, initialization for arrays is not detected.
		 */
		return setslval(x->stmt, v);
	default:
		sysfatal("setslval op %d", x->op);
	}
	return 0;
}

int
setused(Stmt *x, Sym *v)
{
	int i, c, c1, c2;

	if(x == nil || v == nil)
		return Nop;
	c = Nop;
	switch(x->op){
	case 0:
	case ';':
		break;
	case '{':
	case ELSE:
		if(x->list != nil)
			for(i = 0; c == Nop && i < x->list->nitems; i++)
				c = setused(x->list->stmt[i], v);
		break;
	case IF:
		c = isused(x->cond, v);
		c1 = c2 = Nop;
		if(c == Nop)
			c1 = setused(x->thenarm, v);
		if(c == Nop)
			c2 = setused(x->elsearm, v);
		if(c1 == Used || c2 == Used)
			c = Used;
		else if(c1 == Set && c2 == Set)
			c = Set;
		break;
	case '=':
		c = isused(x->rval, v);
		if(c == Nop && isset(x->lval, v))
			c = Set;
		break;
	case FCALL:
		c = setusedfcall(x->fcall, v);
		break;
	case RETURN:
		c = isused(x->expr, v);
		break;
	case DO:
		c = setused(x->stmt, v);
		if(c == Nop)
			c = isused(x->expr, v);
		break;
	case WHILE:
	case FOR:
		/* We assume loops are entered once,
		 * otherwise, initialization for arrays is not detected.
		 */
		c = isused(x->expr, v);
		if(c == Nop)
			c = setused(x->stmt, v);
		break;
	default:
		sysfatal("setused op %d", x->op);
	}
	return c;
}

int
returnsok(Stmt *x, Type *rt)
{
	Type *dummy;
	int n1, n2;

	if(x == nil || rt == nil || rt == tundef)
		return -1;
	switch(x->op){
	case '{':
	case ELSE:
		if(x->list == nil || x->list->nitems == 0)
			return -1;
		return returnsok(x->list->stmt[x->list->nitems-1], rt);
	case IF:
		if(x->elsearm == nil)
			return -1;
		n1 = returnsok(x->thenarm, rt);
		if(n1 < 0)
			return -1;
		n2 = returnsok(x->elsearm, rt);
		if(n2 < 0)
			return -1;
		return n1 + n2;
	case RETURN:
		if(!tcompat(rt, x->expr->type, &dummy)){
			diag("return value of incompatible type\n"
				"\texpected %T\n\tfound %T",
				rt, x->expr->type);
			return -1;
		}
		return 1;
	}
	return -1;
}

#define P(s)	((s!=nil)?(s)->id:0UL)

int
Tfmt(Fmt *f)
{
	Type *t;
	int i;
	Sym *s, *sv;

	t = va_arg(f->args, Type*);
	if(t == nil)
		return fmtprint(f, "<nilt>");
	if(t->sym != nil && (f->flags&FmtSharp) == 0)
		return fmtprint(f, "%s", t->sym->name);
	if(t->op >= nelem(topname)){
		fprint(2, "bug: Tfmt: type op %d", t->op);
		abort();
	}
	fmtprint(f, "%s:%uld", topname[t->op], P(t));
	switch(t->op){
	case Tundef:
	case Tint:
	case Tbool:
	case Tchar:
	case Treal:
	case Tenum:
	case Tprog:
	case Tfwd:
	case Tfile:
		return 0;
	case Trange:
		if(tis(t->super, Tchar))
			fmtprint(f, " '%c'..'%c'", (char)t->first, (char)t->last);
		else if(tis(t->super, Tenum))
			fmtprint(f, " %s..%s", enumname(t->super, t->first),
				enumname(t->super, t->last));
		else
			fmtprint(f, " %d..%d", t->first, t->last);
		return fmtprint(f, " of %T", t->super);
	case Tarry:
		return fmtprint(f, " [%T] of %T", t->idx, t->elem);
	case Tstr:
		return fmtprint(f, " [0..%d] of %T", t->last, t->elem);
	case Trec:
		fmtprint(f, "{");
		if(t->fields != nil)
			for(i = 0; i < t->fields->nitems; i++){
				if(i > 0)
					fmtprint(f, " ");
				s = t->fields->sym[i]->swfield;
				sv = t->fields->sym[i]->swval;
				if(s != nil && sv != nil)
					fmtprint(f, "(%s=%s){", s->name, sv->name);
				fmtprint(f,"%N;", t->fields->sym[i]);
				if(s != nil && sv != nil)
					fmtprint(f, "}");
			}
		return fmtprint(f, "}");
	case Tptr:
		return fmtprint(f, " to %T", t->ref);
	case Tproc:
		return fmtargs(f, t);
	case Tfunc:
		fmtargs(f, t);
		return fmtprint(f, ":%T", t->rtype);
	default:
		fprint(2, "Tfmt BUG: op=%d\n", t->op);
		return fmtprint(f, "TBUG(%d)", t->op);
	}
}

/*
 * Checking undefined types is tricky, because they may have
 * inner components undefined and it would be easy to cross a nil
 * pointer due to a bug in the compiler.
 * By now, we check the only case that can happen in theory,
 * which is forward references for pointer types.
 */
static int
hasundefs(Type *t)
{
	if(t == nil)
		return 1;

	switch(t->op){
	case Tfwd:
	case Tundef:
		return 1;
	case Tptr:
		if(t->ref != nil)	/* nil has a nil ref */
		if(t->ref->op == Tundef || t->ref->op == Tfwd)
			return 1;
		break;
	}
	return 0;
}

void
checkundefs(void)
{
	int i;
	Prog *p;
	List *tl;
	Type *t;


	if(nerrors > 0)
		return;
	if(env == nil || env->prog == nil || env->prog->prog == nil)
		sysfatal("checkundefs");
	p = env->prog->prog;

	tl = p->types;
	assert(tl->kind == Lsym);
	for(i = 0; i < tl->nitems; i++){
		t = tl->sym[i]->type;
		if(hasundefs(t))
			symerror(tl->sym[i], "undefined type\n");
	}
}

