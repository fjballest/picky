#include "syshdr.h"
#include "p.h"
#include "pam.h"
#include "y.tab.h"

Env *env;			/* top of stack */
static Sym *strs[Nbighash];	/* strings and names */
static Sym *keys[Nhash];		/* keywords and top-level */
static int nnames;
Sym *badnode, *pstdin, *pstdout;
int nerrors;
ulong tgen;
Stats stats;

static char *stname[] =
{
	[Snone]	"unknown",
	[Skey]	"keyword",
	[Sstr]	"name",
	[Sconst] "constant",
	[Stype]	"type",
	[Svar]	"variable",
	[Sproc]	"procedure",
	[Sfunc]	"function",
	[Sunary] "unary",
	[Sbinary] "binary",
};

static char *opnames[256] =
{
	['+']	"+",
	['-']	"-",
	['*']	"*",
	['/']	"/",
	['%']	"%",
	['<']	"<",
	['>']	">",
	['=']	"=",
	[',']	",",
	['[']	"[]",
	['.']	".",
	['^']	"^",
	[Ole]	"<=",
	[Oge]	">=",
	[Odotdot] "..",
	[Oand]	"and",
	[Oor]	"or",
	[Oeq]	"==",
	[One]	"!=",
	[Opow]	"**",
	[Oint]	"ival",
	[Onil]	"nil",
	[Ochar]	"cval",
	[Oreal]	"rval",
	[Ostr]	"sval",
	[Otrue]	"true",
	[Ofalse] "false",
	[Onot]	"not",
	[Olit]	"lit",
	[Ocast]	"cast",
	[Ouminus] "-",
	[Oaggr]	"aggr",
};

char*
opname(int op)
{
	if(op < 0 || op >= nelem(opnames) || opnames[op] == nil)
		sysfatal("opname called for op %#x", op);
	return opnames[op];
}

static Sym*
oksym(Sym *s)
{
	if(s == nil)
		return badnode;
	return s;
}

static Type*
oktype(Type *t)
{
	if(t == nil)
		return tundef;
	return t;
}

static void
additem(List *s, void *new)
{
	if((s->nitems%Incr) == 0)
		s->items = erealloc(s->items, (s->nitems+Incr)*sizeof(void*));
	s->items[s->nitems++] = new;
	if(s->nitems > stats.mlist)
		stats.mlist = s->nitems;
}

static void
delitem(List *s, void *p)
{
	int i;

	for(i = 0; i < s->nitems; i++)
		if(s->items[i] == p){
			if(i < s->nitems - 1)
				memmove(&s->items[i], &s->items[i+1],
					sizeof p *(s->nitems - i -1));
			s->nitems--;
			return;
		}
	sysfatal("delitem: no item");
}

void
addstmt(List *l, Stmt *n)
{
	assert(l->kind == Lstmt);
	additem(l, n);
}

void
addsym(List *l, Sym *n)
{
	assert(l->kind == Lsym);
	additem(l, n);
}

void
appsyms(List *l1, List *l2)
{
	int i;

	if(l1 == nil || l2 == nil)
		return;
	assert(l1->kind == Lsym && l2->kind == Lsym);
	for(i = 0; i < l2->nitems; i++)
		additem(l1, l2->items[i]);
}

void
addsym0(List *l, Sym *n)
{
	assert(l->kind == Lsym);
	additem(l, nil);
	memmove(&l->sym[1], &l->sym[0], (l->nitems-1) * sizeof(Sym*));
	l->sym[0] = n;
}

List*
alloclist(void)
{
	static List *els;
	static int nels;

	stats.nlists++;
	if(nels == 0){
		nels = Aincr;
		els = emalloc(sizeof(List)*nels);
	}
	nels--;
	return els++;
}
	
List*
newlist(int lkind)
{
	List *l;

	l = alloclist();
	l->kind = lkind;
	return l;
}

static Env *freeenvs;
Env*
pushenv(void)
{
	Env *e;
	static ulong egen;

	stats.nenvs++;
	if(freeenvs == nil){
		stats.menvs++;
		e = emalloc(sizeof *e);
	}else{
		e = freeenvs;
		freeenvs = e->prev;
		memset(e, 0, sizeof *e);
	}
	e->id = ++egen;
	e->prev = env;
	if(env != nil)
		e->prog = env->prog;
	env = e;
	return e;
}

void
popenv(void)
{
	Env *e;

	assert(env != nil);
	e = env;
	env = e->prev;
	e->prev = nil;

	if(env == nil)
		sysfatal("pop of top level");
	e->prev = freeenvs;
	freeenvs = e;
}

static ulong
shash(char *s)
{
	ulong h;
	char c;

	h = 0;
	for(; *s != 0; s++){
		h = h * 3;
		c = *s;
		if(c >= 'A' && c <= 'Z')
			c = c - 'A' + 'a';
		h += c;
	}
	return h;
}

static Sym*
hlookup(Sym *s, char *n)
{
	ulong nl;

	stats.nhash++;
	nl = 0;
	for(; s != nil; s = s->hnext){
		if(strcmp(s->name, n) == 0)
			break;
		nl++;
	}
	stats.nlink += nl;
	if(nl > stats.mlink)
		stats.mlink = nl;
	return s;
}

Sym*
allocsym(void)
{
	static Sym *els;
	static int nels;

	stats.nsyms++;
	if(nels == 0){
		nels = Aincr;
		els = emalloc(sizeof(Sym)*nels);
	}
	nels--;
	return els++;
}

/*
 * create a Sym.
 * n is kept; do not deallocate.
 */
static Sym*
newsym(char *n, int kind)
{
	Sym *s;
	static ulong sgen;

	s = allocsym();
	s->id = ++sgen;
	s->name = n;
	s->stype = kind;
	s->fname = fname;
	s->lineno = lineno;
	return s;
}

/*
 * Return the symbol for a name or
 * a fake string symbol used to store the string.
 */
Sym*
strlookup(char *n)
{
	int h;
	Sym *s;

	h = shash(n)%Nbighash;
	s = hlookup(strs[h], n);
	if(s == nil){
		stats.nstrs++;
		s = newsym(estrdup(n), Sstr);
		s->hnext = strs[h];
		strs[h] = s;
	}
	return s;
}

/*
 * Identifier lookup.
 * If given Snone, it returns any symbol type.
 * Otherwise, it returns exactly the requested type or nil.
 */
Sym*
lookup(char *n, int kind)
{
	int h;
	Sym *s;
	Env *e;

	h = shash(n)%Nhash;
	for(e = env; e != nil; e = e->prev){
		s = hlookup(e->tab[h], n);
		if(s != nil){
			if(kind != Snone && kind != s->stype)
				return nil;
			return s;
		}
	}
	return nil;
}

/*
 * Return a keyword, an existing object, or a unique copy of n.
 */
Sym*
keylookup(char *n)
{
	int h;
	Sym *s;

	h = shash(n)%Nhash;
	s = hlookup(keys[h], n);
	if(s != nil)
		return s;
	s = lookup(n, Snone);
	if(s != nil)
		return s;
	return strlookup(n);
}

/*
 * Define an identifier with the given kind.
 * Define a keyword if kind is Skey
 */
Sym*
defsym(char *n, int kind)
{
	Sym *s;
	int h;
	Sym **tab;

	h = shash(n)%Nhash;
	s = newsym(estrdup(n), kind);
	if(kind == Skey)
		tab = keys;
	else
		tab = env->tab;
	s->hnext = tab[h];
	tab[h] = s;
	return s;
}

Sym*
defssym(Sym *s, int kind)
{
	int h;
	Sym *n, **tab;

	h = shash(s->name)%Nhash;
	n = newsym(s->name, kind);
	if(kind == Skey)
		tab = keys;
	else
		tab = env->tab;
	n->hnext = tab[h];
	tab[h] = n;
	n->fname = s->fname;
	n->lineno = s->lineno;
	return n;
}

int
checkdup(Sym *s)
{
	if(s != nil && s->stype != Sstr){
		diag("'%s' already defined as a %s", s->name, stname[s->stype]);
		return -1;
	}
	return 0;
}

int
evaluated(Sym *s)
{
	return s->stype == Sconst;
}

int
islval(Sym *n)
{
	switch(n->stype){
	case Svar:
		return 1;
	case Sunary:
		if(n->op == '^')
			return islval(n->left);
		break;
	case Sbinary:
		if(n->op == '[' || n->op == '.')
			return islval(n->left);
		break;
	}
	return 0;
}

static void
cpval(Sym *d, Sym *s)
{
	d->op = s->op;
	d->type = oktype(s->type);

	/* following fields are union, but
	 * we don't know how much to copy.
	 */
	d->tok = s->tok;
	d->ival = s->ival;
	d->rval = s->rval;
	d->vals = s->vals;
	d->left = s->left;
	d->right = s->right;
	d->fsym = s->fsym;
	d->fargs = s->fargs;
	d->rec = s->rec;
	d->field = s->field;
	d->prog = s->prog;
}

static Sym*
_newexpr(int k, int op, Sym *s1, Sym *s2)
{
	Sym *nd;

	stats.nexpr++;
	nd = allocsym();
	nd->stype = k;
	nd->op = op;
	nd->left = s1;
	nd->right = s2;
	if(s1 != nil && s1->stype != Snone){
		nd->fname = s1->fname;
		nd->lineno = s1->lineno;
	}else{
		nd->fname = fname;
		nd->lineno = lineno;
	}
	switch(k){
	case Snone:
		goto Fail;
	case Sunary:
		if(s1 == nil || tchkunary(nd) < 0)
			goto Fail;
		evalexpr(nd);
		break;
	case Sbinary:
		if(s1 == nil || s2 == nil || tchkbinary(nd) < 0)
			goto Fail;
		evalexpr(nd);
		break;
	case Sconst:
		switch(nd->op){
		case Oint:
			nd->type = tcint;
			break;
		case Oreal:
			nd->type = tcreal;
			break;
		case Ochar:
			nd->type = tcchar;
			break;
		case Otrue:
			nd->type = tcbool;
			nd->ival = 1;
			break;
		case Ofalse:
			nd->type = tcbool;
			nd->ival = 0;
			break;
		case Onil:
			nd->type = tcnil;
			break;
		case Ostr:
		case Olit:
		case Oaggr:
			/* type must be given by caller */
			break;
		default:
			sysfatal("newexpr: Sconst: op %d", nd->op);
		}
		break;
	default:
		sysfatal("newexpr: stype %d", k);
	}
	return nd;
Fail:
	return badnode;
}

static void
debugexpr(Sym *s)
{
	if(debug['E'])
		if(s->type != nil || s->type != tundef)
			fprint(2, " -> %N %T\n", s, s->type);
		else
			fprint(2, " -> %N\n", s);
}

Sym*
newexpr(int k, int op, Sym *s1, Sym *s2)
{
	Sym *s;

	if(debug['E'])
		fprint(2, "newexpr %s %s %N %N", stname[k], opname(op), s1, s2);
	s = _newexpr(k, op, s1, s2);
	debugexpr(s);
	return s;
}

void
checkcond(Stmt *s, Sym *c)
{
	if(nerrors>0 || c == nil)
		return;
	if(!tis(c->type, Tbool))
		stmterror(s, "condition must be bool");
}

/*
 * String handling is ugly, to say the least.
 * For each string length, we generate a builtin array[] of char
 * type with the required length.
 * This may happen in any environment, but types are defined
 * in the top-level.
 */
Type*
newstrtype(int len)
{
	Type *t;
	Sym *s;
	Env *e;
	char name[50];

	seprint(name, name + sizeof name, "$tstr%d", len);
	s = lookup(name, Stype);
	if(s != nil)
		return s->type;
	s = defsym(name, Stype);
	t = newtype(Tstr);
	t->idx = tcint;
	t->elem = tchar;
	t->sz = len * tchar->sz;
	t->first = 0;
	t->last = len-1;
	t->op = Tstr;
	s->type = t;
	t->sym = s;
	s->type->id = tgen++;
	for(e = env; e->prev != nil; e = e->prev)
		;
	addsym(e->prog->prog->types, s);
	return t;
}

Sym*
newstr(char *sval)
{
	Sym *nd;
	Env *e;
	static int sgen;

	nd = _newexpr(Sconst, Ostr, nil, nil);
	nd->sval = sval;
	nd->name = smprint("$s%d", sgen++);
	nd->type = newstrtype(utflen(sval));
	for(e = env; e->prev != nil; e = e->prev)
		;
	addsym(e->prog->prog->consts, nd);
	return nd;
}

Sym*
newint(long ival, int op, Type *t)
{
	Sym *s;

	if(debug['E'])
		fprint(2, "new%s %ld %T", opname(op), ival, t);
	s = _newexpr(Sconst, op, nil, nil);
	s->ival = ival;
	if(t != nil)
		s->type = t;
	debugexpr(s);
	return s;
}

Sym*
newreal(double rval, Type *t)
{
	Sym *s;

	if(debug['E'])
		fprint(2, "newreal %2.2f %T", rval, t);
	s = _newexpr(Sconst, Oreal, nil, nil);
	s->rval = rval;
	if(t != nil)
		s->type = t;
	debugexpr(s);
	return s;
}

Sym*
newvarnode(Sym *s)
{
	if(s == nil)
		return badnode;
	if(s->stype != Svar && s->stype != Sconst){
		diag("no variable or constant with name '%s'", s->name);
		return badnode;
	}
	return s;
}

Sym*
newfcall(Sym *f, List *args, int op)
{
	Sym *n;
	Type *t;
	Prog *prog;

	if(f == nil || args == nil)
		return badnode;
	if(debug['E'])
		fprint(2, "newfcall %s()", f->name);
	if((f->stype != Sproc && f->stype != Sfunc) || f->prog == nil){
		diag("'%s': is not a subprogram", f->name);
		goto Fail;
	}

	if(!tis(f->type, op)){
		diag("'%s' is not a %s", f->name, topname[op]);
		goto Fail;
	}
	t = nil;
	n = nil;
	prog = f->prog;
	if(prog->b != nil){
		if(bargcheck(prog->b, args, prog->b->args) < 0)
			goto Fail;
		if(prog->rtype != tundef){
			t = prog->rtype;
			if(t == tundef)
				t = args->sym[0]->type;
		}
		n = prog->b->fn(prog->b, args);
		if(n == nil)
			t = brtype(prog->b, args);
	}else if(tchkcall(f, args) < 0)
		goto Fail;
	else
		t = prog->rtype;

	if(n == nil){
		n = allocsym();
		n->stype = Sfcall;
		n->type = t;
		if(n->type == nil)
			n->type = tundef;
		n->fsym = f;
		n->fargs = args;
	}
	n->fname = f->fname;
	n->lineno = f->lineno;
	debugexpr(n);
	return n;
Fail:
	debugexpr(badnode);
	return badnode;
}

Sym*
findfield(Type *t, char *n, char *why)
{
	int i;
	Sym *s;

	if(t->op != Trec){
		diag("using '%s' requires a record", why);
		return nil;
	}
	if(t->fields == nil)
		return nil;
	for(i = 0; i < t->fields->nitems; i++){
		s = t->fields->sym[i];
		if(strcmp(s->name, n) == 0)
			return s;
	}
	diag("field '%s' does not exist", n);
	return nil;
}

void
setswfield(List *l, Sym *sw)
{
	int i;

	if(l == nil || sw == nil || env->rec == nil)
		return;
	if(sw == nil)
		return;
	assert(l->kind == Lsym);
	for(i = 0; i < l->nitems; i++)
		l->sym[i]->swfield = sw;
}

void
setswval(List *l, Sym *sw)
{
	int i;

	if(l == nil)
		return;
	assert(l->kind == Lsym);
	for(i = 0; i < l->nitems; i++)
		l->sym[i]->swval = sw;
}

Sym*
fieldaccess(Sym *lval, char *fld)
{
	Sym *n, *fs;

	if(lval == nil)
		return badnode;
	fs = findfield(lval->type, fld, ".");
	if(fs == nil)
		return badnode;
	n = allocsym();
	n->stype = Sbinary;
	n->op = '.';
	n->rec = lval;
	n->field = fs;
	n->type = n->right->type;
	n->fname = lval->fname;
	n->lineno = lval->lineno;
	return n;
}

Stmt*
newstmt(int op)
{
	static Stmt *els;
	static int nels;
	Stmt *s;

	stats.nstmts++;
	if(nels == 0){
		nels = Aincr;
		els = emalloc(sizeof(Stmt)*nels);
	}
	nels--;
	s = els++;
	s->op = op;
	s->sfname = fname;
	s->lineno = lineno;
	return s;
}

void
cpsrc(Stmt *to, Stmt *from)
{
	to->sfname = from->sfname;
	to->lineno = from->lineno;
}

Stmt*
newbody(List *l)
{
	Stmt *s;

	s = newstmt('{');
	s->list = l;
	if(l->nitems > 0)
		cpsrc(s, l->stmt[0]);
	return s;
}

Prog*
allocprog(void)
{
	static Prog *els;
	static int nels;

	stats.nprogs++;
	if(nels == 0){
		nels = Aincr;
		els = emalloc(sizeof(Prog)*nels);
	}
	nels--;
	return els++;
}

/*
 * NB: Eol is an abstraction. It represents \n, but it may
 * be \r in windows. pilib reports Eol in the first char of
 * the end-of-line sequence, and consumes any char left in
 * that sequence during readeol()
 */
Sym*
newprog(Sym *n)
{
	Sym *s, *ns;
	int i;
	static int once, pgen;
	static struct {
		char *name;
		int val;
	}chrs[] = {
		{"Eol", '\n'},
		{"Eof", 0xFF},
		{"Tab", '\t'},
		{"Esc",	0x1b},
		{"Nul", 0},
	};

	chrs[0].val = EOL[0];
	if(n->stype != Sstr)
		if(n->stype != Sproc && n->stype != Sfunc)
			diag("'%s' is already defined as a %s",
				n->name, stname[n->stype]);
		else if(n->prog != nil && n->prog->stmt != nil)
			diag("%s '%s' already defined", stname[n->stype], n->name);
		/* else XXX: check that header matches and return it */

	/* continue despite errors; for safety */
	s = defssym(n, Sproc);
	s->type = newtype(Tproc);
	s->prog = allocprog();
	s->prog->psym = s;
	s->prog->consts = newlist(Lsym);
	s->prog->procs = newlist(Lsym);
	s->prog->types = newlist(Lsym);
	s->prog->vars = newlist(Lsym);
	s->prog->rtype = tundef;

	if(once++ == 0){
		declstdtypes(s->prog->types);

		pstdin = defsym("stdin", Svar);
		pstdin->type = tfile;
		addsym(s->prog->vars, pstdin);

		pstdout = defsym("stdout", Svar);
		pstdout->type = tfile;
		addsym(s->prog->vars, pstdout);

		ns = defsym("Maxint", Sconst);
		ns->type = tcint;
		ns->op = Oint;
		ns->ival = tcint->last;
		addsym(s->prog->consts, ns);

		ns = defsym("Minint", Sconst);
		ns->type = tcint;
		ns->op = Oint;
		ns->ival = tcint->first;
		addsym(s->prog->consts, ns);

		ns = defsym("Maxchar", Sconst);
		ns->type = tcchar;
		ns->op = Ochar;
		ns->ival = tcchar->last;
		addsym(s->prog->consts, ns);

		ns = defsym("Minchar", Sconst);
		ns->type = tcchar;
		ns->op = Ochar;
		ns->ival = tcchar->first;
		addsym(s->prog->consts, ns);

		for(i = 0; i < nelem(chrs); i++){
			ns = defsym(chrs[i].name, Sconst);
			ns->op = Ochar;
			ns->ival = chrs[i].val;
			ns->type = tcchar;
			addsym(s->prog->consts, ns);
		}
	}else
		s->id = pgen++;
	return s;
}

Sym*
decltype(Sym *n, Type *t)
{
	Sym *s;
	Prog *p;

	if(env->prog == nil)
		sysfatal("missing program declaration");
	if(n->stype != Sstr && n->stype != Stype){
		diag("'%s' is already defined as a %s", n->name, stname[n->stype]);
		return nil;
	}else if(n->stype == Stype){
		if(n->type->op != Tfwd){
			diag("'%s' already defined", n->name);
			return nil;
		}
		if(debug['D']){
			t->id = tgen;
			fprint(2, "decltype: fwd %N with type %#T\n", n, t);
		}
		s = n;
		*s->type = *t;
	}else{
		s = defssym(n, Stype);
		if(t == nil){
			t = newtype(Tfwd);
			s->type = t;
		}else if(t->sym != nil){
			s->type = newtype(t->op);
			*s->type = *t;
		}else
			s->type = t;
	}
	s->type->sym = s;
	if(t->op != Tfwd){
		s->type->id = tgen++;
		p = env->prog->prog;
		addsym(p->types, s);
	}
	if(debug['D'])
		fprint(2, "decltype: id %uld: %N = %#T\n", s->type->id, n, s->type);
	return s;
}

Sym*
declproc(Sym *n)
{
	Sym *s;

	if(env->prog == nil)
		sysfatal("missing program declaration");
	s = newprog(n);
	addsym(env->prog->prog->procs, s);
	s->type->op = Tproc;
	s->stype = Sproc;
	pushenv();
	env->prog = s;
	return s;
}

Sym*
declfunc(Sym *n)
{
	Sym *s;

	if(env->prog == nil)
		sysfatal("missing program declaration");
	s = newprog(n);
	addsym(env->prog->prog->procs, s);
	s->type->op = Tfunc;
	s->stype = Sfunc;
	pushenv();
	env->prog = s;
	return s;
}

void
declprogdone(Sym *n)
{
	List *parms, *vars;
	int i, nr;
	Prog *p;

	if(debug['D'])
		fprint(2, "declfunc: %N\n", n);
	p = n->prog;
	if(n->stype != Sfunc){
		if(p->nrets > 0)
			symerror(n, "procedure with return statements");
		goto Vars;
	}
	parms = p->parms;
	for(i = 0; i < parms->nitems; i++)
		if(parms->sym[i]->op == Orefparm){
			symerror(parms->sym[i],
				"ref argument not allowed in function '%s'",
				n->name);
			break;
		}
	nr = returnsok(p->stmt, p->rtype);
	if(nr < 0)
		symerror(n, "function must end with a return statement");
	else if(p->nrets > nr)
		symerror(n, "function may return too soon");
Vars:
	vars = p->vars;
	for(i = 0; i < vars->nitems; i++)
		setused(p->stmt, vars->sym[i]);

}

static void
makevar(Sym *s, Type *t)
{
	if(env->prog == nil)
		sysfatal("missing program declaration");
	s->type = oktype(t);
	addsym(env->prog->prog->vars, s);
	if(env->prev != nil)
		s->op = Olvar;
	if(debug['D'])
		fprint(2, "declvar: %N\n", s);
}

Sym*
declvar(Sym *n, Type *t)
{
	Sym *s;

	checkdup(n);
	s = defssym(n, Svar);
	makevar(s, t);
	return s;
}

Sym*
declfield(Sym *n, Type *t)
{
	Sym *s;

	s = defssym(n, Svar);
	s->type = oktype(t);
	if(debug['D'])
		fprint(2, "declfield: %N = %#T\n", s, t);
	return s;
}

Sym*
declconst(Sym *n, Sym *expr)
{
	Sym *s;
	Env *e;

	if(env->prog == nil)
		sysfatal("missing program declaration");
	checkdup(n);
	expr = oksym(expr);
	if(!evaluated(expr)){
		diag("value for %s is not constant", n->name);
		return badnode;
	}
	if(expr->stype == Sconst && expr->op == Ostr && expr->name[0] == '$'){
		/* temporary string has now a name */
		for(e = env; e->prev != nil; e = e->prev)
			;
		delitem(e->prog->prog->consts, expr);
	}
	if(expr->stype == Sconst && expr->op == Oaggr && expr->name[0] == '$'){
		/* temporary aggr has now a name */
		for(e = env; e->prev != nil; e = e->prev)
			;
		delitem(e->prog->prog->consts, expr);
	}
	s = defssym(n, Sconst);
	cpval(s, expr);
	addsym(env->prog->prog->consts, s);
	if(debug['D'])
		fprint(2, "declconst: %N %N -> %N %T\n",
			n, expr, s, s->type);
	return s;
}

Sym*
newparm(Sym *n, Type *t, int byref)
{
	Sym *d;

	d = defssym(n, Svar);
	d->type = t;
	if(byref != 0)
		d->op = Orefparm;
	else
		d->op = Oparm;
	return d;
}

Sym*
caseexpr(Sym *vnd, Sym *e)
{
	if(e->stype == Sbinary)
		switch(e->op){
		case ',':
			e->left = caseexpr(vnd, e->left);
			e->right = caseexpr(vnd, e->right);
			e->op = Oor;
			e->type = e->left->type;
			return e;
		case Odotdot:
			e->op = Oand;
			e->left = newexpr(Sbinary, Oge, vnd, e->left);
			e->right= newexpr(Sbinary, Ole, vnd, e->right);
			e->type = e->left->type;
			return e;
		}
	return newexpr(Sbinary, Oeq, vnd, e);
}

Stmt*
newassign(Sym *lval, Sym *rval)
{
	Stmt *s;
	Type *dummy;

	s = newstmt('=');
	s->lval = lval;
	s->rval = rval;
	if(!tcompat(lval->type, rval->type, &dummy))
		diag("incompatible argument types (%T and %T) for assignment",
			lval->type, rval->type);
	else if(lval != badnode && !islval(lval))
		diag("left part of assignment must be an l-value");
	return s;
}

static int
dupvals(Sym *v1, Sym *v2)
{
	if(v1 == nil || v2 == nil)
		return 0;
	switch(v2->stype){
	case Sbinary:
		switch(v2->op){
		case ',':
		case Odotdot: /* aprox: check left and right boundaries */
			return dupvals(v1, v2->left) || dupvals(v1, v2->right);
		}
		return 0;
	case Sconst:
		break;
	default:
		return 0;
	}
	assert(v2->stype == Sconst);
	switch(v1->stype){
	case Sbinary:
		switch(v1->op){
		case ',':
			return dupvals(v1->left, v2) || dupvals(v1->right, v2);
		case Odotdot:
			return v1->left->ival >= v2->ival &&
				v1->right->ival <= v2->ival;
		}
		return 0;
	case Sconst:
		return v1->ival == v2->ival;
	}
	return 0;
}

static void
dupcases(List *cases)
{
	int i, j;

	for(i = 1; i < cases->nitems; i++)
		for(j = 0; j < i; j++)
			if(dupvals(cases->stmt[i]->expr, cases->stmt[j]->expr)){
				symerror(cases->stmt[j]->expr,
					"value used in previous case");
				return;
			}
}

Stmt*
newswitch(Sym *expr, List *cases)
{
	static int vgen;
	int nnil, i;
	Stmt *sx, *tx, *cx, **xp;
	Sym *vs;
	Sym *vnd, *te;
	char nm[50];

	if(nerrors > 0)
		return newstmt(';');
	sx = newbody(newlist(Lstmt));
	sx->sfname = expr->fname;
	sx->lineno = expr->lineno;
	seprint(nm, nm + sizeof nm, "$v%d", vgen++);
	vs = defsym(nm, Svar);
	makevar(vs, cases->stmt[0]->expr->type);
	vnd = newvarnode(vs);
	addstmt(sx->list, newassign(vnd, expr));
	additem(sx->list, nil);
	xp = &sx->list->stmt[1];
	nnil = 0;
	dupcases(cases);
	for(i = 0; i < cases->nitems; i++){
		cx = cases->stmt[i];
		if(cx->op != CASE)
			sysfatal("p.y bug: switch stmt is %d", cx->op);
		if(cx->expr == nil){
			nnil++;
			if(i < cases->nitems < 0)
				diag("default must be the last case");
			*xp = cx->stmt;
			break;
		}else{
			te = caseexpr(vnd, cx->expr);
			tx = cx->stmt;
			cx->op = IF;
			cx->cond = te;
			cx->thenarm = tx;
			cx->elsearm = nil;
			*xp = cx;
			xp = &cx->elsearm;
		}
	}
	if(nnil > 1)
		diag("only a default per switch");
	return sx;
}

Stmt*
newfor(Sym *lval, Sym *from, Sym *to, Stmt *body)
{
	Stmt *s, *ws, *ns;
	List *arg;

	s = newbody(newlist(Lstmt));
	addstmt(s->list, newassign(lval, from));
	ws = newstmt(FOR);
	ws->expr = to;
	ws->stmt = body;
	if(nerrors == 0)
		setslval(ws, lval);
	addstmt(s->list, ws);
	arg = newlist(Lsym);
	addsym(arg, lval);
	if(to->op == '>' || to->op == Oge)
		ns = newassign(lval, newfcall(bpred, arg, Tfunc));
	else
		ns = newassign(lval, newfcall(bsucc, arg, Tfunc));
	ws->incr = ns;
	return s;
}

static void
mkint(Sym *n, long v)
{
	n->stype = Sconst;
	n->op = Oint;
	n->ival = v;
}

static void
mkreal(Sym *n, double r)
{
	n->stype = Sconst;
	n->op = Oreal;
	n->rval = r;
}

static void
mkbool(Sym *n, int b)
{
	n->stype = Sconst;
	n->op = (b ? Otrue: Ofalse);
	n->ival = (b ? 1: 0);
}

static void
ueval(Sym *n)
{
	Sym *ln;

	assert(n->stype == Sunary);
	if(evaluated(n->left) == 0)
		return;
	ln = n->left;
	switch(n->op){
	case '+':
		cpval(n, ln);
		break;
	case Ouminus:
		if(tis(ln->type, Tint))
			mkint(n, - ln->ival);
		else
			mkreal(n, - ln->rval);
		break;
	case Onot:
		mkbool(n, !ln->ival);
		break;
	case Ocast:
		sysfatal("ueval: Ocast should do this");
	case '^':
		sysfatal("ueval: ^ arg can't be evaluated");
		break;
	default:
		sysfatal("bad unary op %d", n->op);
	}
}

static void
cancmp(Sym *ln, Sym *rn)
{
	assert((tisord(ln->type) && tisord(rn->type)) ||
			(tis(ln->type, Treal) && tis(rn->type, Treal)));
}

static void
beval(Sym *n)
{
	Sym *ln, *rn;

	assert(n->stype == Sbinary);
	if(evaluated(n->left) == 0 || evaluated(n->right) == 0)
		return;
	ln = n->left;
	rn = n->right;
	switch(n->op){
	case '+':
		if(tis(ln->type, Tint))
			mkint(n, ln->ival + rn->ival);
		else
			mkreal(n, ln->rval + rn->rval);
		break;
	case '-':
		if(tis(ln->type, Tint))
			mkint(n, ln->ival - rn->ival);
		else
			mkreal(n, ln->rval - rn->rval);
		break;
	case '*':
		if(tis(ln->type, Tint))
			mkint(n, ln->ival * rn->ival);
		else
			mkreal(n, ln->rval * rn->rval);
		break;
	case '/':
		if(tis(ln->type, Tint))
			if(rn->ival == 0){
				diag("divide by 0 in constant expression");
				mkint(n, 1);
			}else
				mkint(n, ln->ival / rn->ival);
		else
			if((float)rn->rval == 0.0){
				diag("divide by 0.0 in constant expression");
				mkreal(n, 1.0);
			}else
				mkreal(n, ln->rval / rn->rval);
		break;
	case '%':
		mkint(n, ln->ival % rn->ival);
		break;
	case Opow:
		if(tis(ln->type, Tint)){
			if(ln->ival == 2)
				n->ival = 1 << rn->ival;
			else
				n->ival = (int)pow((double)ln->ival,
							(double)rn->ival);
			mkint(n, n->ival);
		}else
			mkreal(n, pow(ln->rval, rn->rval));
		break;
	case Oand:
		mkbool(n, ln->ival && rn->ival);
		break;
	case Oor:
		mkbool(n, ln->ival || rn->ival);
		break;
	case '<':
		cancmp(ln, rn);
		if(tis(ln->type, Treal))
			mkbool(n, ln->rval < rn->rval);
		else
			mkbool(n, ln->ival < rn->ival);
		break;
	case '>':
		cancmp(ln, rn);
		if(tis(ln->type, Treal))
			mkbool(n, ln->rval > rn->rval);
		else
			mkbool(n, ln->ival > rn->ival);
		break;
	case Ole:
		cancmp(ln, rn);
		if(tis(ln->type, Treal))
			mkbool(n, ln->rval <= rn->rval);
		else
			mkbool(n, ln->ival <= rn->ival);
		break;
	case Oge:
		cancmp(ln, rn);
		if(tis(ln->type, Treal))
			mkbool(n, ln->rval >= rn->rval);
		else
			mkbool(n, ln->ival >= rn->ival);
		break;
	case Oeq:
		if(tis(ln->type, Trec) || tis(ln->type, Tarry) ||
		   tis(ln->type, Tstr))
			break;
		if(tis(ln->type, Treal))
			mkbool(n, ln->rval == rn->rval);
		else
			mkbool(n, ln->ival == rn->ival);
		break;
	case One:
		if(tis(ln->type, Trec) || tis(ln->type, Tarry) ||
		   tis(ln->type, Tstr))
			break;
		if(tis(ln->type, Treal))
			mkbool(n, ln->rval != rn->rval);
		else
			mkbool(n, ln->ival != rn->ival);
		break;
	case Odotdot:
	case ',':
	case '.':		/* can happen if we access an aggretate constant */
	case '[':		/* can happen if we access an aggretate constant */

		/*
		 * BUG: for aggregate constant field/member access we
		 * could evaluate the accessor and return the member.
		 * We are deferring until run-time instead.
		 */
		break;
	default:
		sysfatal("tchkbinary: bad op %d", n->op);
	}
}

/*
 * Try to evaluate the expression. If we can, the node
 * mutates to become a value, but the type is kept untouched.
 */
void
evalexpr(Sym *n)
{
	switch(n->stype){
	case Snone:
		break;
	case Sconst:
	case Svar:
		break;
	case Sunary:
		ueval(n);
		break;
	case Sbinary:
		beval(n);
		break;
	case Sfcall:
		sysfatal("evalexpr: newfcall should do this");
	default:
		sysfatal("evalexpr: stype %d", n->stype);
	}

}

void
syminit(void)
{
	int i;
	Sym *s;
	static struct{
		char *name;
		int tok;
	} keys[] = {
		{"False", FALSE},
		{"True", TRUE},
		{"and", AND},
		{"array", ARRAY},
		{"case", CASE},
		{"consts", CONSTS},
		{"default", DEFAULT},
		{"do", DO},
		{"else", ELSE},
		{"for", FOR},
		{"function", FUNCTION},
		{"if", IF},
		{"len", LEN},
		{"nil", NIL},
		{"not", NOT},
		{"of", OF},
		{"or", OR},
		{"procedure", PROCEDURE},
		{"program", PROGRAM},
		{"record", RECORD},
		{"ref", REF},
		{"return", RETURN},
		{"switch", SWITCH},
		{"types", TYPES},
		{"vars", VARS},
		{"while", WHILE},
	};

	for(i = 0; i < nelem(keys); i++){
		s = defsym(keys[i].name, Skey);
		s->tok = keys[i].tok;
	}

	badnode = newsym("$undefined", Snone);
}

#define P(s)	((s!=nil)?(s)->id:0UL)

int
Nfmt(Fmt *f)
{
	Sym *n;
	int i;

	n = va_arg(f->args, Sym*);
	if(n == nil)
		return fmtprint(f, "<nil>");
	switch(n->stype){
	case Snone:
		return fmtprint(f, "undef");
	case Skey:
	case Sstr:
		return fmtprint(f, "\"%q\"", n->name);
	case Sconst:
		if(n->name != nil)
			fmtprint(f, "%s=", n->name);
		switch(n->op){
		case Ochar:
			switch(n->ival){
			case '\n':
				return fmtprint(f, "Eol");
			case '\'':
				return fmtprint(f, "'");
			case '\t':
				return fmtprint(f, "Tab");
			case 0:
				return fmtprint(f, "Nul");
			case 0xFF:
				return fmtprint(f, "Eof");
			default:
				return fmtprint(f, "'%c'", (char)n->ival);
			}
		case Oint:
			return fmtprint(f, "%ld", n->ival);
		case Olit:
			if(tis(n->type, Tenum))
				return fmtprint(f, "%s", enumname(n->type, n->ival));
			return fmtprint(f, "%ld", n->ival);
		case Oreal:
			return fmtprint(f, "%#g", n->rval);
		case Ostr:
			return fmtprint(f, "\"%s\"", n->sval);
		case Oaggr:
			fmtprint(f, "%T(", n->type);
			if(n->vals != nil)
			for(i = 0; i < n->vals->nitems; i++){
				if(i > 0)
					fmtprint(f, ", ");
				fmtprint(f, "%N", n->vals->sym[i]);
			}
			return fmtprint(f, ")");
		case Otrue:
		case Ofalse:
		case Onil:
			return fmtprint(f, "%s", opname(n->op));
		default:
			fprint(2, "Nfmt called with op %d\n", n->op);
		}
		return fmtprint(f, "NCBUG(%d)", n->op);
	case Svar:
		switch(n->op){
		case Olvar:
			fmtprint(f, "%%");
			break;
		case Oparm:
			fmtprint(f, "$");
			break;
		case Orefparm:
			fmtprint(f, "&");
			break;
		}
		/* fall */
	case Stype:
		if(n->name != nil)
			fmtprint(f, "%s", n->name);
		else
			fmtprint(f, "<noname>");
		if(n->type != nil && (f->flags&FmtSharp) == 0)
			fmtprint(f, ": %T", n->type);
		return 0;
	case Sunary:
		if(f->flags&FmtSharp)
			return fmtprint(f, "%s", opname(n->op));
		return fmtprint(f, "%s(%N)", opname(n->op), n->left);
	case Sbinary:
		if(f->flags&FmtSharp)
			return fmtprint(f, "%s", opname(n->op));
		return fmtprint(f, "%s(%N, %N)", opname(n->op), n->left, n->right);
	case Sproc:
	case Sfunc:
		return fmtprint(f, "%s %s()", stname[n->stype], n->name);
	case Sfcall:
		if(f->flags&FmtSharp)
			return fmtprint(f, "%s()", n->fsym->name);
		fmtprint(f, "%s(", n->fsym->name);
		for(i = 0; i < n->fargs->nitems; i++){
			if(i > 0)
				fmtprint(f, ", ");
			fmtprint(f, "%N", n->fargs->sym[i]);
		}
		return fmtprint(f, ")");
	default:
		fprint(2, "Nfmt called with stype %d\n", n->stype);
		return fmtprint(f, "NBUG(%d)", n->stype);
	}
}

static void
tabs(Fmt *f, int lvl)
{
	while(lvl-- > 0)
		fmtprint(f, "\t");
}

static int xlvl;

int
Xfmt(Fmt *f)
{
	Stmt *x;
	int i;

	x = va_arg(f->args, Stmt*);
	if(0 && x != nil)
		fmtprint(f, "%q:%d", x->sfname, x->lineno);
	tabs(f, xlvl);
	if(x == nil)
		return fmtprint(f, "<nilstmt>");
	xlvl++;
	switch(x->op){
	case DO:
		fmtprint(f, "dowhile(%N){\n", x->expr);
		if(f->flags&FmtSharp)
			break;
		x = x->stmt;
		goto Block;
	case WHILE:
		fmtprint(f, "while(%N){\n", x->expr);
		x = x->stmt;
		goto Block;
	case FOR:
		/* Does not print x->incr; */
		fmtprint(f, "for(%N){\n", x->expr);
		x = x->stmt;
		goto Block;
	case CASE:
		fmtprint(f, "case %N {\n", x->expr);
		x = x->stmt;
		goto Block;
	case '{':
	case ELSE:
		fmtprint(f, "{\n");
	Block:
		if(x->list != nil)
			for(i = 0; i < x->list->nitems; i++)
				fmtprint(f, "%X\n", x->list->stmt[i]);
		tabs(f, xlvl-1);
		fmtprint(f, "}");
		break;
	case FCALL:
		assert(x->op == FCALL);
		fmtprint(f, "%N", x->fcall);
		break;
	case '=':
		fmtprint(f, "%N = %N", x->lval, x->rval);
		break;
	case IF:
		fmtprint(f, "if(%N)\n", x->cond);
		if(x->thenarm->op == '{')
			xlvl--;
		fmtprint(f, "%X\n", x->thenarm);
		if(x->thenarm->op == '{')
			xlvl++;
		if(x->elsearm != nil){
			tabs(f, xlvl-1);
			fmtprint(f, "else\n");
			xlvl--;
			fmtprint(f, "%X\n", x->elsearm);
			xlvl++;
		}
		break;
	case ';':
		fmtprint(f, "nop\n");
		break;
	case 0:
		fmtprint(f, "undef\n");
		break;
	case RETURN:
		fmtprint(f, "return %N\n", x->expr);
		break;
	default:
		fprint(2, "Xfmt called with op %d\n", x->op);
		fmtprint(f, "XBUG(%d)", x->op);
	}
	xlvl--;
	return 0;
}

static void
fdtabs(int fd, int lvl)
{
	while(lvl-- > 0)
		fprint(fd, "\t");
}

static int plvl;

void
dumpprog(int fd, Sym *s)
{
	int i;
	Prog *p;

	fdtabs(fd, plvl);
	p = s->prog;
	if(p == nil){
		fprint(fd, "<nullprog>\n");
		return;
	}
	if(p->rtype == tundef)
		fprint(fd, "prog: %N\n", s);
	else
		fprint(fd, "prog: %N: %T\n", s, p->rtype);
	plvl++;
	if(p->parms != nil && p->parms->nitems > 0){
		fdtabs(fd, plvl);
		fprint(fd, "parms:\n");
		for(i = 0; i < p->parms->nitems; i++){
			s = p->parms->sym[i];
			fdtabs(fd, plvl+1);
			if(s->op == Orefparm)
				fprint(fd, "ref %N\n", s);
			else
				fprint(fd, "val %N\n", s);
		}
		fprint(fd, "\n");
	}
	if(p->consts != nil && p->consts->nitems > 0){
		fdtabs(fd, plvl);
		fprint(fd, "consts:\n");
		for(i = 0; i < p->consts->nitems; i++){
			s = p->consts->sym[i];
			fdtabs(fd, plvl+1);
			fprint(fd, "%N\n", s);
		}
		fprint(fd, "\n");
	}
	if(p->types != nil && p->types->nitems > 0){
		fdtabs(fd, plvl);
		fprint(fd, "types:\n");
		for(i = 0; i < p->types->nitems; i++){
			s = p->types->sym[i];
			fdtabs(fd, plvl+1);
			fprint(fd, "%s = %#T\n", s->name, s->type);
		}
		fprint(fd, "\n");
	}
	if(p->vars != nil && p->vars->nitems > 0){
		fdtabs(fd, plvl);
		fprint(fd, "vars:\n");
		for(i = 0; i < p->vars->nitems; i++){
			fdtabs(fd, plvl+1);
			fprint(fd, "%N\n", p->vars->sym[i]);
		}
		fprint(fd, "\n");
	}
	if(p->procs != nil)
		for(i = 0; i < p->procs->nitems; i++){
			s = p->procs->sym[i];
			dumpprog(fd, s);
		}
	fdtabs(fd, plvl);
	fprint(fd, "stmt:\n");
	xlvl = plvl;
	fprint(fd, "%X\n\n", p->stmt);
	xlvl = 0;
	plvl--;
}

void
dumpenv(int fd, Env *e, int recur)
{
	int i;
	Sym *s;

	if(env == nil){
		fprint(fd, "nilenv\n");
		return;
	}
	fprint(fd, "env %uld:\n", P(e));
	for(i = 0; i < Nhash; i++)
		for(s = env->tab[i]; s != nil; s = s->hnext)
			fprint(fd, "\t%uld: %N\n", P(s), s);
	if(env->prog != nil)
		dumpprog(fd, env->prog);
	fprint(fd, "\n");

	if(recur && e->prev != nil){
		fprint(fd, "prev ");
		dumpenv(fd, e->prev, recur);
	}
	
}

