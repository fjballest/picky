#include "syshdr.h"
#include "p.h"
#include "pam.h"
#include "y.tab.h"

typedef struct Hdr Hdr;
typedef struct Ldef Ldef;
typedef struct Luse Luse;

/*
 * p.out header, followed by:
 * - symbol table
 * - pc/sp table
 * - pc/line table
 * - constant and values
 * - code
 */
struct Hdr
{
	Sym *entry;
	ulong ntypes;
	ulong nobjs;
	ulong npcs;
	ulong nprocs;
	ulong ntext;
};

struct Ldef
{
	ulong	addr;
	Luse*	uses;
};

struct Luse
{
	Luse*	next;
	Code*	code;
	ulong	np;
};

static Hdr hdr;
static char *oname;
static Biobuf *out;
static ulong addr;
static ulong paddr;
static ulong xaddr;
static ulong xprocid;
static Ldef *labels;
static int nlabels;
static Code *xcode;

static uint opcodes[256] =
{
	['+']	ICadd,
	['-']	ICsub,
	['*']	ICmul,
	['/']	ICdiv,
	['%']	ICmod,
	['<']	IClt,
	['>']	ICgt,
	['=']	ICsto,
	['[']	ICidx,
	['^']	ICindir,
	[Ole]	ICle,
	[Oge]	ICge,
	[Oand]	ICand,
	[Oor]	ICor,
	[Oeq]	ICeq,
	[One]	ICne,
	[Opow]	ICpow,
	[Oint]	ICdata,
	[Onil]	ICdata,
	[Ochar]	ICdata,
	[Oreal]	ICdata,
	[Ostr]	ICdata,
	[Otrue]	ICdata,
	[Ofalse] ICdata,
	[Onot]	ICnot,
	[Olit]	ICdata,
	[Ocast]	ICcast,
	[Ouminus]	ICminus,
	[Oaggr]	ICdata,
};


static int
mklbl(int *lp)
{
	Ldef *l;

	if((nlabels%Aincr) == 0)
		labels = erealloc(labels, (nlabels+Aincr)* sizeof *labels);
	l = &labels[nlabels];
	memset(l, 0, sizeof *l);
	l->addr = xaddr;
	*lp = nlabels++;
	return *lp;
}

static ulong
uselbl(int ln)
{
	static Luse *els;
	static int nels;
	Luse *u;
	Ldef *l;

	l = &labels[ln];
	if(nels == 0){
		nels = Aincr;
		els = emalloc(sizeof(Luse)*nels);
	}
	nels--;
	u = els++;
	u->code = xcode;
	u->np = xcode->np;
	u->next = l->uses;
	l->uses = u;
	return l->addr;
}

static void
setlbl(int ln, ulong addr)
{
	Luse *u;
	Ldef *l;

	l = &labels[ln];
	for(u = l->uses; u != nil; u = u->next)
		u->code->p[u->np] = addr;
}

static void
mapl(List *cl, void (*f)(Sym*))
{
	int i;

	if(cl != nil)
		for(i = 0; i < cl->nitems; i++)
			f(cl->sym[i]);
}

static void
mapxl(List *cl, void (*f)(Stmt*))
{
	int i;

	if(cl != nil)
		for(i = 0; i < cl->nitems; i++)
			f(cl->stmt[i]);
}

static Pcent*
allocpc(void)
{
	static Pcent *els;
	static int nels;

	if(nels == 0){
		nels = Aincr;
		els = emalloc(sizeof(Pcent)*nels);
	}
	nels--;
	return els++;
}

static void
addpc(Stmt *s)
{
	Pcent *e;
	static char *lfname;
	static ulong llno;

	e = allocpc();
	e->st = s;
	e->pc = xaddr;
	if(xcode->pcs == nil)
		xcode->pcs = e;
	else
		xcode->pcstl->next = e;
	xcode->pcstl = e;
	if(lfname == nil || s->lineno != llno || strcmp(lfname, s->sfname) != 0)
		hdr.npcs++;
	lfname = s->sfname;
	llno = s->lineno;
}

static void
addnode(Sym *nd)
{
	Pcent *e;

	e = allocpc();
	e->nd = nd;
	e->pc = xaddr;
	if(xcode->pcs == nil)
		xcode->pcs = e;
	else
		xcode->pcstl->next = e;
	xcode->pcstl = e;
}

static void
oprint(char *fmt, ...)
{
	va_list arg;

	va_start(arg, fmt);
	if(Bvprint(out, fmt, arg) < 0)
		sysfatal("%s: %r", oname);
	va_end(arg);
}

#define ROUND(s, sz)	(((s)+((sz)-1))&~((sz)-1))

static void
genentry(Sym *s)
{
	hdr.entry = s;
}

static void
emithdr(void)
{
	oprint("#!/bin/pam\n");
	oprint("entry %uld\n", hdr.entry->id);
}

static void
genconst(Sym *s)
{
	s->addr = addr;
	if(tis(s->type, Tstr))
		addr += tcchar->sz * utflen(s->sval);
	else
		addr += tsz(s->type);
	hdr.nobjs++;
}

/*
 * BUG: There's a possible source of bugs here.
 * For records and the like, we emitconst() if we don't have a name
 * or an address, but it should be done depending on the symbol type,
 * and not on which name or address it has.
 * This must be reworked.
 */
static void
emitconst(Sym *s)
{
	int i;
	Sym *c;

	if(s->type->id >= hdr.ntypes)
		sysfatal("emitconst: tid for %T not defined", s->type);
	oprint("%s %uld %#ulx", s->name ? s->name : "_", s->type->id, s->addr);
	if(tis(s->type, Treal))
		oprint(" %e", s->rval);
	else if(tis(s->type, Trec) || tis(s->type, Tarry) || tis(s->type, Tstr)){
		if(tis(s->type, Tstr) ||
		   (tis(s->type, Tarry) && tis(s->type->idx, Tint) &&
		    tis(s->type->elem, Tchar)))
			oprint(" %q", s->sval);
		else{
			oprint(" %d %q %d\n", s->vals->nitems, s->fname, s->lineno);
			for(i = 0; i < s->vals->nitems; i++){
				c = s->vals->sym[i];
				if(c->name == nil || c->addr == 0)
					emitconst(c);
				else
					oprint("%s %uld %#ulx\n",
						c->name, c->type->id, c->addr);
			}
			return;
		}
	}else
		oprint(" %ld", s->ival);
	oprint(" %q %d\n", s->fname, s->lineno);
}

static void
genvar(Sym *s)
{
	genconst(s);
}

static void
genlvar(Sym *s)
{
	s->addr = paddr;
	paddr += tsz(s->type);
	assert(s->addr < paddr);
}

static void
emitvar(Sym *s)
{
	oprint("%s %uld %#ulx -", s->name, s->type->id, s->addr);
	oprint(" %q %d\n", s->fname, s->lineno);
}

static void
genparm(Sym *s)
{
	s->addr = paddr;
	if(s->op == Orefparm)
		paddr += 8;
	else
		paddr += tsz(s->type);
}

static char
tfmt(Type *t)
{
	t = tderef(t);
	switch(t->op){
	case Tbool:
	case Tchar:
	case Tint:
	case Tenum:
	case Tfile:
	case Tptr:
	case Tarry:
	case Tstr:
		return *topname[t->op];
	case Treal:
		return 'r';
	case Trec:
		return 'R';
	case Tproc:
		return 'X';
	case Tfunc:
		return 'F';
	default:
		sysfatal("tfmt bug: op %d", t->op);
	}
	return '?';
}

static void
emitfield(Sym *s)
{
	oprint("%s %uld %#ulx\n", s->name, s->type->id, s->addr);
}

static void
emittype(Sym *s)
{
	int i;
	Type *t, *st;
	char c;

	t = s->type;
	c = tfmt(t);
	oprint("%uld %s %c", s->type->id, s->name, c);
	oprint("  %ld %ld %ld %ld", tfirst(t), tlast(t), tlen(t), tsz(t));
	switch(t->op){
	case Tarry:
	case Tstr:
		oprint(" %ld\n", t->elem->id);
		break;
	case Tenum:
		oprint(" 0\n");
		for(i = 0; i < t->lits->nitems; i++)
			oprint("%s\n", t->lits->sym[i]->name);
		break;
	case Trec:
		oprint(" 0\n");
		mapl(t->fields, emitfield);
		break;
	case Tptr:
		if(t->ref == nil)
			oprint(" 0\n");
		else
			oprint(" %uld\n", t->ref->id);
		break;
	case Trange:
		oprint(" 0\n");
		if(c == 'e'){
			st = tderef(t);
			for(i = tfirst(t); i <= tlast(t); i++)
				oprint("%s\n", st->lits->sym[i]->name);
		}
		break;
	default:
		oprint(" 0\n");
	}
}

static void
genlit(Sym *n)
{
	static int lgen;
	char nm[50];
	Sym *cs;

	seprint(nm, nm+sizeof nm, "$lit%d", lgen++);
	cs = defsym(nm, Sconst);
	declconst(cs, n);
	n->addr = addr;
	addr += tsz(n->type);
	hdr.nobjs++;
}

static void
emit32(u32int o)
{
	if(xcode->np + Incr >= xcode->ap){
		xcode->ap += 1024;
		xcode->p = erealloc(xcode->p, sizeof(ulong)*xcode->ap);
	}
	xcode->p[xcode->np++] = o;
	xaddr++;
}

static void
emitr(float r)
{
	emit32(*(u32int*)&r);
}

static void
emitda(u64int addr)
{
	emit32(addr&0xFFFFFFFF);
	emit32(addr>>32);
}

static void
genjmp(int op, int l)
{
	emit32(op);
	emit32(uselbl(l));
}

static void
genop(int op, ulong arg)
{
	emit32(op);
	if(hasarg(op))
		emit32(arg);
}

static void genexpr(Sym*);

static void
genlval(Sym *nd)
{
	switch(nd->stype){
	case Svar:
		addnode(nd);
		switch(nd->op){
		case Orefparm:
			genop(ICarg, nd->addr);
			genop(ICindir, 8);	/* de-reference */
			/*
			 * XXX: generate checks for ranges.
			 */
			break;
		case Oparm:
			genop(ICarg, nd->addr);
			break;
		case Olvar:
			genop(IClvar, nd->addr);
			break;
		default:
			genop(ICdaddr, nd->addr);
		}
		break;
	case Sconst:
		addnode(nd);
		if(nd->op != Ostr && nd->op != Oaggr)
			sysfatal("genlval: const: op %d", nd->op);
		genop(ICdaddr, nd->addr);
		break;
	case Sunary:
		addnode(nd);
		switch(nd->op){
		case '^':
			genexpr(nd->left);
			genop(ICptr, 0);
			break;
		default:
			sysfatal("genlval: unary: op %d", nd->op);
		}
		break;
	case Sbinary:
		addnode(nd);
		switch(nd->op){
		case '.':
			genlval(nd->rec);
			if(nd->field->addr != 0)
				genop(ICfld, nd->field->addr);
			break;
		case '[':
			genexpr(nd->right);
			genlval(nd->left);
			genop(ICidx, nd->left->type->id);
			break;
		default:
			sysfatal("genlval: binary: op %d", nd->op);
		}
		break;
	default:
		sysfatal("genlval: stype %d", nd->stype);
	}
}

static void
genunary(Sym *nd)
{
	switch(nd->op){
	case '+':
		genexpr(nd->left);
		break;
	case Ouminus:
	case Onot:
		genexpr(nd->left);
		if(tis(nd->left->type, Treal))
			emit32(opcodes[nd->op]|ITreal);
		else
			emit32(opcodes[nd->op]);
		break;
	case Ocast:
		genexpr(nd->left);
		if(tis(nd->left->type, Treal))
			genop(opcodes[nd->op]|ITreal, nd->type->id);
		else
			genop(opcodes[nd->op], nd->type->id);
		break;
	case '^':
		genexpr(nd->left);
		genop(ICptr, 0);
		genop(ICindir, tsz(nd->left->type->ref));
		break;
	default:
		sysfatal("bad unary op %d", nd->op);
	}
}


static void
genbinary(Sym *nd)
{
	switch(nd->op){
	case '+':
	case '-':
	case '*':
	case '/':
	case '%':
	case Opow:
	case '<':
	case '>':
	case Ole:
	case Oge:
	case Oand:
	case Oor:
		genexpr(nd->right);
		genexpr(nd->left);
		if(tis(nd->left->type, Treal))
			genop(opcodes[nd->op]|ITreal, 0);
		else
			genop(opcodes[nd->op], 0);
		break;
	case Oeq:
	case One:
		if(tisatom(nd->left->type)){
			genexpr(nd->right);
			genexpr(nd->left);
			if(tisord(nd->left->type))
				genop(opcodes[nd->op], 0);
			else if(tis(nd->left->type, Tptr))
				genop(opcodes[nd->op]|ITaddr, 0);
			else
				genop(opcodes[nd->op]|ITreal, 0);
			break;
		}else{
			genlval(nd->left);
			genlval(nd->right);
			if(nd->op == Oeq)
				genop(ICeqm, tsz(nd->left->type));
			else
				genop(ICnem, tsz(nd->left->type));
			break;
		}
		break;
	case '.':
	case '[':
		genlval(nd);
		genop(ICindir, tsz(nd->type));
		break;
	default:
		sysfatal("tchkbinary: bad op %d", nd->op);
	}
}

static void
gencall(Sym *n)
{
	List *args;
	List *parms;
	Sym *f, *arg, *parm;
	int i;

	f = n->fsym;
	args = n->fargs;
	parms = f->prog->parms;
	for(i = args->nitems-1; i >= 0; i--){
		arg = args->sym[i];
		parm = parms->sym[i];
		if(parm->op == Orefparm)
			genlval(arg);
		else{
			genexpr(arg);
			/*
			 * XXX: generate checks for ranges.
			 */
		}
	}
	switch(f->id){
	case PAMbuiltin|PBfread:
	case PAMbuiltin|PBfreadln:
	case PAMbuiltin|PBfwrite:
	case PAMbuiltin|PBfwriteln:
		genop(ICpush, args->sym[1]->type->id);
		break;
	case PAMbuiltin|PBopen:
		genop(ICpush, args->sym[2]->type->id);
		genop(ICpush, args->sym[1]->type->id);
		break;
	case PAMbuiltin|PBfatal:
		genop(ICpush, args->sym[0]->type->id);
		break;
	case PAMbuiltin|PBnew:
		genop(ICpush, args->sym[0]->type->id);
		break;
	}
	addnode(n);
	genop(ICcall, f->id);
}

static void
genexpr(Sym *nd)
{
	switch(nd->stype){
	case Sfcall:
		gencall(nd);
		break;
	case Sconst:
		addnode(nd);
		switch(nd->op){
		case Ochar:
		case Oint:
		case Otrue:
		case Ofalse:
		case Olit:
			genop(ICpush, nd->ival);
			break;
		case Onil:
			genop(ICdata, 8);
			emitda(0ULL);
			break;
		case Oreal:
			emit32(ICpush|ITreal);
			emitr(nd->rval);
			break;
		case Ostr:
		case Oaggr:
			genop(ICdaddr, nd->addr);
			genop(ICindir, tsz(nd->type));
			break;
		default:
			sysfatal("genexpr: Sconst: op %d", nd->op);
		}
		break;
	case Svar:
		genlval(nd);
		genop(ICindir, tsz(nd->type));
		break;
	case Sunary:
		genunary(nd);
		break;
	case Sbinary:
		genbinary(nd);
		break;
	default:
		sysfatal("genexpr: stype %d", nd->stype);
	}
}

static void
gencode(Stmt *x)
{
	int lbl, elbl, saved;

	addpc(x);
	switch(x->op){
	case ';':
		break;
	case DO:
		mklbl(&lbl);
		setlbl(lbl, xaddr);
		gencode(x->stmt);
		genexpr(x->expr);
		genjmp(ICjmpt, lbl);
		break;
	case WHILE:
	case FOR:
		mklbl(&lbl);
		setlbl(lbl, xaddr);
		genexpr(x->expr);
		genjmp(ICjmpf, mklbl(&elbl));
		gencode(x->stmt);
		if(x->incr != nil){ /* FOR */
			if(x->expr->op == Oge || x->expr->op == Ole){
				saved = x->expr->op;
				x->expr->op = Oeq;
				genexpr(x->expr);
				x->expr->op = saved;
				genjmp(ICjmpt, elbl);
			}
			gencode(x->incr);
		}
		genjmp(ICjmp, lbl);
		setlbl(elbl, xaddr);
		break;
	case '{':
	case ELSE:
		mapxl(x->list, gencode);
		break;
	case FCALL:
		gencall(x->fcall);
		break;
	case '=':
		if(islval(x->rval)){
			genlval(x->rval);
			genlval(x->lval);
			genop(ICstom, x->lval->type->id);
		}else{
			genexpr(x->rval);
			genlval(x->lval);
			genop(ICsto, x->lval->type->id);
		}
		break;
	case IF:
		genexpr(x->cond);
		genjmp(ICjmpf, mklbl(&lbl));
		gencode(x->thenarm);
		if(x->elsearm != nil)
			genjmp(ICjmp, mklbl(&elbl));
		setlbl(lbl, xaddr);
		if(x->elsearm != nil){
			gencode(x->elsearm);
			setlbl(elbl, xaddr);
		}
		break;
	case RETURN:
		if(x->expr != nil)
			genexpr(x->expr);
		genop(ICret, xprocid);
		break;
	case SWITCH:
	case CASE:
		sysfatal("gencode: FOR/SWITCH/CASE must be handled by frontend");
	default:
		sysfatal("gencode: op %d", x->op);
	}
}

static void
emitcode(Code *c)
{
	int i, first;
	u32int addr, arg, ir, ic;
	Pcent *e;

	addr = c->addr;
	e = c->pcs;
	for(i = 0; i < c->np; i++){
		for(; e != nil && e->pc <= addr+i && e->st != nil; e = e->next)
			oprint("# %R\n", e->st);
		oprint("%05x\t%I", addr+i, c->p[i]);
		ir = c->p[i];
		ic = IC(ir);
		if(hasarg(c->p[i])){
			arg = c->p[++i];
			if(IT(ir) == ITreal && ic != ICcast)
				oprint("\t%e", *(float*)&arg);
			else
				oprint("\t%#010ux", arg);
		}else
			oprint("\t");
		first = 0;
		for(; e != nil && e->pc <= addr+i && e->nd != nil; e = e->next)
			if(e->nd != nil){
				if(first++ == 0)
					oprint("\t#");
				oprint(" %#N;", e->nd);
			}
		oprint("\n");
		if(ic == ICdata)
			for(; arg > 0; arg -= 4){
				i++;
				oprint("%05x\t%#ux\n", addr+i, c->p[i]);
			}
	}
}

static void
genproc(Sym *s)
{
	Prog *p;
	List *xl;
	int i;

	p = s->prog;
	hdr.nprocs++;
	s->addr = xaddr;
	xcode = &p->code;
	xprocid = s->id;
	xcode->addr = xaddr;
	paddr = 0;
	for(i = p->parms->nitems-1; i >= 0; i--)
		genparm(p->parms->sym[i]);
	p->parmsz = paddr;
	paddr = 0;
	mapl(p->vars, genlvar);
	p->varsz = paddr;
	xl = p->stmt->list;
	if(xl->stmt[xl->nitems-1]->op != RETURN)
		addstmt(xl, newstmt(RETURN));
	gencode(p->stmt);
}

static void
emitproc(Sym *s)
{
	Prog *p;

	p = s->prog;
	if(p == nil || p->parms == nil || p->vars == nil)
		return;
	oprint("%uld %s %#05ulx", s->id, s->name, s->addr);
	oprint(" %d %d %uld", p->parms->nitems, p->vars->nitems, tsz(p->rtype));
	oprint(" %uld %uld", p->parmsz, p->varsz);
	oprint(" %q %d\n", s->fname, s->lineno);
	mapl(p->parms, emitvar);
	mapl(p->vars, emitvar);
}

static void
emittext(Sym *s)
{
	Prog *p;

	p = s->prog;
	if(p == nil || p->parms == nil || p->vars == nil)
		return;
	xcode = &p->code;
	oprint("# %s()\n", s->name);
	emitcode(xcode);
}

static void
emitpcs(Sym *s)
{
	Pcent *pc;
	char *lfname;
	ulong llno;
	Stmt *st;

	lfname = nil;
	llno = 0;
	for(pc = s->prog->code.pcs; pc != nil; pc = pc->next){
		st = pc->st;
		if(st == nil)
			continue;
		if(lfname != nil &&
		   strcmp(st->sfname, lfname) == 0 && st->lineno == llno)
			continue;
		lfname = st->sfname;
		llno = st->lineno;
		oprint("%05ulx\t%q\t%uld\n", pc->pc, lfname, llno);
	}
}

void
gen(Biobuf *bout, char *nm)
{
	Prog *p;
	Sym *s;


	oname = nm;
	out = bout;
	s = lookup("main", Sproc);
	if(s == nil || s->prog == nil || s->stype != Sproc)
		sysfatal("missing declaration of procedure 'main'");
	if(s->prog->parms != nil && s->prog->parms->nitems > 0)
		sysfatal("procedure 'main' may not have parameters");
	genentry(s);
	if(env->prog == nil)
		sysfatal("missing program");
	p = env->prog->prog;
	hdr.ntypes = tgen;
	if(p->consts != nil)
		mapl(p->consts, genconst);
	if(p->vars != nil)
		mapl(p->vars, genvar);
	if(p->procs != nil)
		mapl(p->procs, genproc);
	hdr.ntext = xaddr;
	emithdr();
	oprint("types %uld\n", hdr.ntypes);

	mapl(p->types, emittype);
	oprint("vars %uld\n", hdr.nobjs);
	if(p->consts != nil)
		mapl(p->consts, emitconst);
	if(p->vars != nil)
		mapl(p->vars, emitvar);
	oprint("procs %uld\n", hdr.nprocs);
	mapl(p->procs, emitproc);
	oprint("text %uld\n", hdr.ntext);
	mapl(p->procs, emittext);
	oprint("pcs %uld\n", hdr.npcs);
	mapl(p->procs, emitpcs);
	Bflush(out);
}

int
Rfmt(Fmt *f)
{
	Stmt *x;

	x = va_arg(f->args, Stmt*);
	if(0 && x != nil)
		fmtprint(f, "%q:%d", x->sfname, x->lineno);
	if(x == nil)
		return fmtprint(f, "<nilstmt>");
	switch(x->op){
	case DO:
		fmtprint(f, "dowhile(%N)", x->expr);
		break;
	case FOR:
		fmtprint(f, "for(%N)", x->expr);
		break;
	case WHILE:
		fmtprint(f, "while(%N)", x->expr);
		break;
	case CASE:
		fmtprint(f, "case %N", x->expr);
		break;
	case '{':
		fmtprint(f, "{...}");
		break;
	case FCALL:
		assert(x->op == FCALL);
		fmtprint(f, "%N", x->fcall);
		break;
	case '=':
		fmtprint(f, "%N = %N", x->lval, x->rval);
		break;
	case IF:
		fmtprint(f, "if(%N)", x->cond);
		break;
	case ELSE:
		fmtprint(f, "else");
		break;
	case ';':
		fmtprint(f, "nop");
		break;
	case 0:
		fmtprint(f, "undef");
		break;
	case RETURN:
		fmtprint(f, "return %N", x->expr);
		break;
	default:
		fprint(2, "Rfmt called with op %d\n", x->op);
		fmtprint(f, "RBUG(%d)", x->op);
	}
	return 0;
}
