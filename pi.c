#include "syshdr.h"
#include "pam.h"
#include "pilib.h"
#include "vers.h"

/*
 * See pam.h for a description.
 */


enum
{
	Poison = 42,
};

int debug[256];

static int waitforwindows = 1;

static ulong entry;
static ulong ninstr;
Tent *tents;
ulong ntents;
Vent *vents;
ulong nvents;
Pent *pents;
static ulong npents;
static Pc *pcs;
static ulong npcs;
static ulong ntext;
static u32int* text;
static Biobuf *bin;
static char *fname;
static ulong lineno;
static char *binln;
static char *toks[Ntoks];
static int ntoks;
static int sflag;
uchar *stack;
static uchar *globend, *stackend, *maxsp;


/*
 * Registers:
 *	program counter
 *	procedure id
 *	stack pointer
 *	frame pointer
 *	(local) var pointer
 *	argument pointer
 */

u32int pc, procid;
uchar *sp, *fp, *vp, *ap;

Pc*
findpc(u32int pc)
{
	int i;

	for(i = 0; i < npcs; i++)
		if(pcs[i].pc >= pc){
			if(i > 0)
				i--;
			return &pcs[i];
		}
	return nil;
}

void
done(char *sts)
{
	ulong tot;

	flushall();
	if(sflag){
		fprint(2, "%uld instructions executed\n", ninstr);
		tot = globend-stack + maxsp-globend + sizeof(u32int)*ntext;
		tot += nheap;
		fprint(2, "%uld bytes used", tot);
		fprint(2, " (%uld code +", sizeof(u32int)*ntext);
		fprint(2, " %uld data +", globend-stack);
		fprint(2, " %uld heap +", nheap);
		fprint(2, " %uld stack)\n", maxsp-globend);
	}
	if(sts == nil)
		undisposed();
	if(waitforwindows)
		finishwait();
	exits(sts);
}

void
panic(char *fmt, ...)
{
	va_list arg;
	char buf[1024], *s;
	Pc *pe;

	flushall();
	va_start(arg, fmt);
	s = buf;
	pe = findpc(pc);
	if(pe != nil)
		s = seprint(s, buf+sizeof buf, "%s:%uld: ", pe->fname, pe->lineno);
	s = vseprint(s, buf+sizeof buf, fmt, arg);
	seprint(s, buf+sizeof buf, "\npc=%#ux, sp=%#ulx, fp=%#ulx",
		pc, sp-stack, fp-stack);
	fprint(2, "%s\n", buf);
	va_end(arg);
	done("panic");
}

/*
 * This is called upon faults, after calling nofaults().
 * Does not return.
 */
void
faulted(char *s)
{
	extern void xstack(void);

	if(strstr(s, "interrupt") != 0){
		if(pc > 0)
			xstack();
		panic("program interrupted");
	}else
		panic("run time violation: %s", s);
}

u32int
pop32(void)
{
	if(sp - stack < 4)
		panic("stack underflow");
	sp -= 4;
	return *(u32int*)sp;
}

u32int
top32(void)
{
	if(sp - stack < 4)
		panic("stack underflow");
	return *(u32int*)(sp-4);
}

u64int
pop64(void)
{
	if(sp - stack < 8)
		panic("stack underflow");
	sp -= 8;
	return *(u64int*)sp;
}

float
popr(void)
{
	if(sp - stack < 4)
		panic("stack underflow");
	sp -= 4;
	return *(float*)sp;
}

void*
popn(int n)
{
	if(sp - stack < n)
		panic("stack underflow");
	sp -= n;
	return sp;
}

void*
popdaddr(void)
{
	return (uchar*)pop64();
}

void
push8(u32int c)
{
	if(sp + 1 > stackend)
		panic("stack overflow");
	*sp++ = (uchar)c;
}

void
push32(u32int c)
{
	if(sp + 4 > stackend)
		panic("stack overflow");
	*(u32int*)sp = c;
	sp += 4;
	if(sp > maxsp)
		maxsp = sp;
}

void
push64(u64int c)
{
	u32int *p;

	if(sp + 8 > stackend)
		panic("stack overflow");
	p = (u32int*)sp;
	p[0] = c & 0xFFFFFFFF;
	p[1] = c>>32 & 0xFFFFFFFF;
	sp += 8;
	if(sp > maxsp)
		maxsp = sp;
}

void
pushr(float r)
{
	if(sp + 4 > stackend)
		panic("stack overflow");
	*(u32int*)sp = *(u32int*)&r;
	sp += 4;
	if(sp > maxsp)
		maxsp = sp;
}

void
pushdaddr(void *p)
{
	push64((u64int)p);
}

static void
pushdata(Vent *v)
{
	long n, i;
	float r;
	char *s;
	Rune ch;
	Tent *t;

	t = tfetch(v->tid);
	switch(t->fmt){
	case 'i':
	case 'e':
	case 'b':
	case 'c':
	case 'f':
		n = strtol(v->val, nil, 0);
		push32(n);
		break;
	case 'r':
		r = strtod(v->val, nil);
		pushr(r);
		break;
	case 'p':
		n = strtol(v->val, nil, 0);
		push64(n);
		break;
	case 'a':
	case 'R':
		if(v->fields == nil)
			sysfatal("pushdata: %V: no fields", v);
		for(i = 0; i < t->nitems; i++)
			pushdata(v->fields[i]);
		break;
	case 'X':
	case 'F':
		sysfatal("pushdata: X and F aggregates not supported");
	case 's':
		n = utflen(v->val);
		s = v->val;
		for(i = 0; i < n; i++){
			s += chartorune(&ch, s);
			push32(ch);
		}
		break;
	default:
		sysfatal("pushdata: bad type fmt '%c'", t->fmt);
	}
}

void
poison(void *p, long n)
{
	uchar *cp;

	if(n == 0)
		return;
	cp = p;
	for(; n > 0; n--)
		*cp++ = (uchar)(Poison ^ (u64int)cp);

	/*
	 * All addresses are aligned to word boundaries in
	 * the stack. This makes poisoned memory to be odd
	 * as an address, and xdispose() and xptr() check for it
	 * to detect references to not initialized pointers.
	 */
	cp = p;
	cp[0] |= 1;
}

static void
datainit(void)
{
	ulong sz;
	Vent *v;
	int i;

	sz = Stack;
	if(nvents > 0){
		v = &vents[nvents-1];
		sz += v->addr + tents[v->tid].sz;
	}
	stack = emalloc(sz);
	stackend = stack + sz;
	sp = stack;
	for(i = 0; i < nvents; i++){
		v = &vents[i];
		if(v->addr != sp - stack)
			sysfatal("bad data '%s' addr %#ulx", v->name, v->addr);
		if(debug['D'])
			fprint(2, "data '%s'\tdaddr %#ulx va"
				" %#p sz %#ulx val '%s'\n",
				v->name, v->addr, sp, tents[v->tid].sz, v->val);
		if(v->val != nil)
			pushdata(v);
		else{
			sz = tents[v->tid].sz;
			poison(sp, sz);
			sp += sz;
		}
	}
	globend = sp;
	/* fake activation record for main */
	ap = sp;
	vp = sp;
	poison(sp, pents[entry].varsz);
	sp += pents[entry].varsz;
	pushdaddr(nil);
	pushdaddr(nil);
	pushdaddr(nil);
	push32(-1);
	push32(-1);
	fp = sp;
	procid = entry;
	if(debug['D'])
		fprint(2, "stack %#p globend %#p end %#p size %#ulx\n",
			sp, globend, stackend, stackend-sp);
	maxsp = sp;
}

static void
dumpxstck(int n)
{
	uchar *e;

	fprint(2, "stack:\t\tsp %#p fp %#p vp %#p ap %#p\n", sp, fp, vp, ap);
	for(e = sp - 4; e >= globend; e -= 4){
		fprint(2, "%#p\t%#ux\n", e, *(u32int*)e);
		if(--n <= 0)
			break;
	}
	fprint(2, "\n");
}

ulong
fetch(void)
{
	if(pc >= ntext)
		panic("bad program counter %#ux", pc);
	return text[pc++];
}

#define ADDR(x) ((uchar*)(x))
/* should convert to an actual pointer referring to addr x */

Tent*
tfetch(int tid)
{
	if(tid < 0 || tid >= ntents)
		panic("bad tid");
	return &tents[tid];
}

static void
idx(int tid)
{
	int v;
	uchar *addr;
	Tent *t;

	addr = popdaddr();
	t = tfetch(tid);
	v = pop32();
	if(v < t->first || v > t->last)
		panic("index value out of range");
	addr += (v - t->first) * tents[t->etid].sz;
	pushdaddr(addr); 
}

/*
 * type-check value of type tid, about to be copied,
 * and return its size.
 */
static long
tchk(Tent *t, void *s)
{
	Tent *nt;
	int *ep, i;
	char *cp;

	switch(t->fmt){
	case 'b':
	case 'c':
	case 'i':
	case 'e':
		ep = s;
		if(*ep < t->first || *ep > t->last)
			panic("assigned value out of range");
		break;
	case 'p':
		/* check pointer value */
		break;
	case 'a':
		/*
		 * Arrays may be used as buffer space,
		 * and it is not reasonable to require the
		 * entire array to be ok regarding values.
		 * So, disable this check.
		 */
		if(0){
			nt = &tents[t->etid];
			cp = s;
			for(i = 0; i < t->nitems; i++){
				tchk(nt, cp);
				cp += nt->sz;
			}
		}
		break;
	case 'R':
		cp = s;
		for(i = 0; i < t->nitems; i++){
			nt = &tents[t->fields[i].tid];
			tchk(nt, cp);
			cp += nt->sz;
		}
		break;
	}
	return t->sz;
}

static void
cast(Tent *t, int v)
{
	if(t->fmt == 'r')
		pushr((float)v);
	else{
		tchk(t, &v);
		push32(v);
	}
}

static void
castr(Tent *t, float r)
{
	int e;

	if(t->fmt == 'r')
		pushr(r);
	else{
		e = (int)r;
		tchk(t, &e);
		push32(e);
	}
}

/*
 * Caller pushes args,
 * call saves space for lvars and 4 saved regs.
 * The callee may call push/pop.
 * ret is called with sp == fp (if there is no ret value)
 * it restores pc, fp, vp, and ap, and moves the
 * return value found at the top of the stack to ap.
 *
 * Right before call:
 *
 *		<- sp
 *	arg
 *	arg
 *	arg
 *	XXX
 *
 * Right before ret:
 *
 *		<- sp
 *	ret
 *	ret
 *	savedpc	<- fp
 *
 * After ret:
 *
 *		<- sp
 *	ret
 *	ret	(<- old ap)
 *	XXX
 *
 * That is, a call+ret replaces args in the stack with result.
 */

static void
call(int pid)
{
	u32int spc;
	uchar *sfp, *svp, *sap;

	if(pid < 0 || pid >= npents)
		panic("bad pid");
	spc = pc;
	sfp = fp;
	svp = vp;
	sap = ap;

	ap = sp - pents[pid].argsz;
	vp = sp;
	poison(sp, pents[pid].varsz);
	sp += pents[pid].varsz;
	pushdaddr(sap);
	pushdaddr(svp);
	pushdaddr(sfp);
	push32(procid);
	push32(spc);
	fp = sp;
	pc = pents[pid].addr;
	procid = pid;
}

static void
ret(int pid)
{
	u32int spc;
	uchar *sfp, *svp, *sap, *retval;
	Pent *p;

	if(pid == entry){
		if(debug['X'])
			fprint(2, "program complete.\n");
		done(nil);
	}
	p = &pents[pid];
	sp -= p->retsz;
	retval = sp;
	spc = pop32();
	procid = pop32();
	sfp = popdaddr();
	svp = popdaddr();
	sap = popdaddr();
	sp = ap;
	if(p->retsz > 0)
		memmove(sp, retval, p->retsz);
	sp += p->retsz;
	fp = sfp;
	ap = sap;
	vp = svp;
	pc = spc;
}

static void
pam(void)
{
	u32int ir, ic, it, taddr, n;
	int a1, a2;
	uchar *d1, *d2;
	float r1, r2;
	Tent *t;

	if(entry >= npents)
		sysfatal("bad entry number");
	pc = pents[entry].addr;
	if(debug['X'])
		fprint(2, "entry pc %#ux\n", pc);
	while(pc < ntext){
		ninstr++;
		if(debug['S'])
			dumpxstck(10 * debug['S']);
		if(debug['D'] > 1)
			dumpglobals();
		ir = fetch();
		ic = IC(ir);
		it = IT(ir);
		if(debug['X'])
			if(hasarg(ir))
				if(it == ITreal)
					fprint(2, "%#05x:\t%I\t%#ux (%#g)\n",
						pc-1, ir, text[pc],
						*(float*)(text+pc));
				else
					fprint(2, "%#05x:\t%I\t%#ux\n",
						pc-1, ir, text[pc]);
			else
				fprint(2, "%#05x:\t%I\n", pc-1, ir);
		switch(ic){
		case ICnop:	/* nop */
			break;
		case ICptr:
			pushdaddr(xptr(popdaddr()));
			break;
		case ICindir:		/* indir  n -sp +sp */
			n = fetch();
			d1 = popdaddr();
			if(sp + n > stackend)
				panic("stack overflow");
			memmove(sp, d1, n);
			sp += n;
			break;
		case ICpush:		/* push n +sp */
			/* both real and int */
			push32(fetch());
			break;
		case ICjmp:		/* jmp addr */
			pc = fetch();
			break;
		case ICjmpt:		/* jmpt addr */
			taddr = fetch();
			if(pop32() != 0)
				pc = taddr;
			break;
		case ICjmpf:		/* jmpf addr */
			taddr = fetch();
			if(pop32() == 0)
				pc = taddr;
			break;
		case ICidx:		/* idx tid  -sp -sp +sp */
			idx(fetch());
			break;
		case ICfld:		/* fld n -sp +sp */
			n = fetch();
			d1 = popdaddr();
			pushdaddr(d1 + n);
			break;
		case ICdata:		/* data n +sp */
			for(n = fetch(); n > 0; n -= 4)
				push32(fetch());
			break;
		case ICdaddr:		/* daddr n +sp */
			n = fetch();
			pushdaddr(stack + n);
			break;
		case ICadd:		/* add -sp -sp +sp */
			if(it == ITreal)
				pushr(popr() + popr());
			else
				push32(pop32() + pop32());
			break;
		case ICsub:		/* sub -sp -sp +sp */
			if(it == ITreal){
				r1 = popr();
				r2 = popr();
				pushr(r1 - r2);
			}else{
				a1 = pop32();
				a2 = pop32();
				push32(a1 - a2);
			}
			break;
		case ICminus:		/* minus -sp +sp */
			if(it == ITreal)
				pushr(- popr());
			else
				push32(- pop32());
			break;
		case ICnot:		/* not -sp +sp */
			push32(pop32() == 0);
			break;
		case ICor:		/* or -sp -sp +sp */
			a1 = pop32();
			a2 = pop32();
			push32(a1 || a2 ? 1 : 0);
			break;
		case ICand:		/* and -sp -sp +sp */
			a1 = pop32();
			a2 = pop32();
			push32(a1 && a2 ? 1 : 0);
			break;
		case ICeq:		/* eq -sp -sp +sp */
			switch(it){
			case ITreal:
				r1 = popr();
				r2 = popr();
				push32(r1 == r2 ? 1 : 0);
				break;
			case ITaddr:
				d1 = popdaddr();
				d2 = popdaddr();
				push32(d1 == d2 ? 1 : 0);
				break;
			default:
				a1 = pop32();
				a2 = pop32();
				push32(a1 == a2 ? 1 : 0);
			}
			break;
		case ICeqm:		/* eqm  n -sp +sp */
			n = fetch();
			d1 = popdaddr();
			d2 = popdaddr();
			if(memcmp(d1, d2, n) == 0)
				push32(1);
			else
				push32(0);
			break;
		case ICne:		/* ne -sp -sp +sp */
			switch(it){
			case ITreal:
				r1 = popr();
				r2 = popr();
				push32(r1 != r2 ? 1 : 0);
				break;
			case ITaddr:
				d1 = popdaddr();
				d2 = popdaddr();
				push32(d1 != d2 ? 1 : 0);
				break;
			default:
				a1 = pop32();
				a2 = pop32();
				push32(a1 != a2 ? 1 : 0);
			}
			break;
		case ICnem:		/* eqm  n -sp +sp */
			n = fetch();
			d1 = popdaddr();
			d2 = popdaddr();
			if(memcmp(d1, d2, n) != 0)
				push32(1);
			else
				push32(0);
			break;
		case ICcast:		/* cast tid -sp +sp */
			t = tfetch(fetch());
			if(it == ITreal)
				castr(t, popr());
			else
				cast(t, pop32());
			break;
		case ICle:		/* le -sp -sp +sp */
			if(it == ITreal){
				r1 = popr();
				r2 = popr();
				push32(r1 <= r2 ? 1 : 0);
			}else{
				a1 = pop32();
				a2 = pop32();
				push32(a1 <= a2 ? 1 : 0);
			}
			break;
		case ICge:		/* ge -sp -sp +sp */
			if(it == ITreal){
				r1 = popr();
				r2 = popr();
				push32(r1 >= r2 ? 1 : 0);
			}else{
				a1 = pop32();
				a2 = pop32();
				push32(a1 >= a2 ? 1 : 0);
			}
			break;
		case ICpow:		/* pow -sp -sp +sp */
			if(it == ITreal){
				r1 = popr();
				r2 = popr();
				pushr(pow(r1, r2));
			}else{
				a1 = pop32();
				a2 = pop32();
				push32((u32int)pow((float)a1, (float)a2));
			}
			break;
		case IClt:		/* lt -sp -sp +sp */
			if(it == ITreal){
				r1 = popr();
				r2 = popr();
				push32(r1 < r2 ? 1 : 0);
			}else{
				a1 = pop32();
				a2 = pop32();
				push32(a1 < a2 ? 1 : 0);
			}
			break;
		case ICgt:		/* gt -sp -sp +sp */
			if(it == ITreal){
				r1 = popr();
				r2 = popr();
				push32(r1 > r2 ? 1 : 0);
			}else{
				a1 = pop32();
				a2 = pop32();
				push32(a1 > a2 ? 1 : 0);
			}
			break;
		case ICmul:		/* mul -sp -sp +sp */
			if(it == ITreal){
				r1 = popr();
				r2 = popr();
				pushr(r1 * r2);
			}else{
				a1 = pop32();
				a2 = pop32();
				push32(a1 * a2);
			}
			break;
		case ICdiv:		/* div -sp -sp +sp */
			if(it == ITreal){
				r1 = popr();
				r2 = popr();
				if(r2 == 0.0)
					panic("divide by 0.0");
				pushr(r1 / r2);
			}else{
				a1 = pop32();
				a2 = pop32();
				if(a2 == 0)
					panic("divide by 0");
				push32(a1 / a2);
			}
			break;
		case ICmod:		/* mod -sp -sp +sp */
			a1 = pop32();
			a2 = pop32();
			if(a2 == 0)
				panic("divide by zero");
			push32(a1 % a2);
			break;
		case ICcall:		/* call pid */
			n = fetch();
			if(n&PAMbuiltin){
				n &= ~PAMbuiltin;
				if(n >= Nbuiltins)
					panic("bad builtin call #%ud", n);
				builtin[n]();
			}else
				call(n);
			break;
		case ICret:		/* ret pid */
			ret(fetch());
			break;
		case ICarg:		/* arg n +sp */
			pushdaddr(ap + fetch());
			break;
		case IClvar:		/* lvar n +sp*/
			pushdaddr(vp + fetch());
			break;
		case ICstom:		/* stom  n -sp -sp */
			t = tfetch(fetch());
			d1 = popdaddr();
			d2 = popdaddr();
			if(d1 != d2){
				n = tchk(t, d2);
				memmove(d1, d2, n);
			}
			break;
		case ICsto:		/* sto  n -sp -sp */
			t = tfetch(fetch());
			d1 = popdaddr();
			n = t->sz;
			if(sp - n < stack)
				panic("stack underflow");
			sp -= n;
			tchk(t, sp);
			memmove(d1, sp, n);
			break;
		default:
			panic("unknown instruction");
		}
	}
	panic("bad program counter");
		
}

static int
Tfmt(Fmt *f)
{
	Tent *t;
	int i;
	Vent *v;

	t = va_arg(f->args, Tent*);
	if(t == nil)
		return fmtprint(f, "<nil>");
	fmtprint(f, "type %s %c %ld..%ld n=%d sz=%uld etid=%ud\n",
		t->name, t->fmt, t->first, t->last, t->nitems, t->sz, t->etid);
	switch(t->fmt){
	case 'e':
		for(i = 0; i < t->nitems; i++)
			fmtprint(f, "\t%s\n", t->lits[i]);
		break;
	case 'R':
		for(i = 0; i < t->nitems; i++){
			v = &t->fields[i];
			fmtprint(f, "\t%s %ud %uld\n", v->name, v->tid, v->addr);
		}
		break;
	}
	return 0;
}


static int
Vfmt(Fmt *f)
{
	Vent *v;
	Tent *t;
	int i, j;
	static int vtabs;

	v = va_arg(f->args, Vent*);
	if(v == nil)
		return fmtprint(f, "<nil>");
	fmtprint(f, "var %s tid %d addr %uld val %s %s:%d\n",
		v->name, v->tid, v->addr, v->val, v->fname, v->lineno);
	if(v->fields == nil)
		return 0;
	t = tfetch(v->tid);
	vtabs++;
	for(i = 0; i < t->nitems; i++){
		for(j = 0; j < vtabs; j++)
			fmtprint(f, "\t");
		fmtprint(f, "%V", v->fields[i]);
	}
	vtabs--;
	return 0;
}

static int
Pfmt(Fmt *f)
{
	Pent *p;
	int i;

	p = va_arg(f->args, Pent*);
	if(p == nil)
		return fmtprint(f, "<nil>");
	fmtprint(f, "prog %s addr %uld #args %ud #vars %ud ret %ud %s:%d\n",
		p->name, p->addr, p->nargs, p->nvars, p->retsz,
		p->fname, p->lineno);
	for(i = 0; i < p->nargs; i++)
		fmtprint(f, "\tp%V", &p->args[i]);
	for(i = 0; i < p->nvars; i++)
		fmtprint(f, "\tl%V", &p->vars[i]);
	return 0;
}

static void
badbin(char *s)
{
	sysfatal("%s:%uld: %s", fname, lineno, s);
}

static char*
getln(void)
{
	do{
		free(binln);
		++lineno;
		binln = Brdstr(bin, '\n', 1);
		if(binln == nil)
			badbin("truncated file");
	}while(binln[0] == '#');
	return binln;
}

static int
rtoks(void)
{
	do{
		ntoks = tokenize(getln(), toks, nelem(toks));
	}while(ntoks == 0);
	return ntoks;
}

static void
rhdr(void)
{
	if(rtoks() != 2)
		badbin("bad entry header");
	if(strcmp(toks[0], "entry") != 0)
		badbin("no entry");
	entry = strtoul(toks[1], nil, 0);
	if(debug['F'])
		fprint(2, "entry %uld\n", entry);
}

static void rvent(Vent*);

static Vent*
rventfield(void)
{
	int i;
	uint addr;
	Vent *v;

	switch(rtoks()){
	case 3:
		addr = strtol(toks[2], nil, 0);
		for(i = 0; i < nvents && vents[i].name != nil; i++)
			if(vents[i].addr == addr)
				return &vents[i];
		badbin("aggregate field address not found");
		break;
	case 6:
		v = emalloc(sizeof *v);
		rvent(v);
		return v;
		break;
	default:
		badbin("bad aggregate field");
	}
	return nil;
}

static void
rvent(Vent *v)
{
	int n, i;

	v->name = estrdup(toks[0]);
	v->tid = strtoul(toks[1], nil, 0);
	v->addr = strtoul(toks[2], nil, 0);
	if(v->tid >= ntents)
		badbin("bad type id");
	v->fname =  estrdup(toks[4]);
	v->lineno =  strtoul(toks[5], nil, 0);
	v->val = nil;
	switch(tents[v->tid].fmt){
	case 'f':
		if(strcmp(v->name, "stdin") == 0)
			v->val = "0";
		else if(strcmp(v->name, "stdout") == 0)
			v->val = "1";
		else if(strcmp(toks[3], "-") != 0)
			v->val =  estrdup(toks[3]);
		break;
	case 'a':
	case 'R':
		if(strcmp(toks[3], "-") != 0){
			n = strtol(toks[3], nil, 0);
			if(n <= 0)
				badbin("aggregate arity <= 0");
			v->fields = emalloc(sizeof(Vent*)*n);
			v->val = "$aggr";
			for(i = 0; i < n; i++)
				v->fields[i] = rventfield();
		}
		break;
	default:
		v->val =  estrdup(toks[3]);
	}
}

static void
rfield(Vent *v)
{
	if(rtoks() != 3)
		badbin("bad field entry");
	v->name = estrdup(toks[0]);
	v->tid = strtoul(toks[1], nil, 0);
	v->addr = strtoul(toks[2], nil, 0);
	if(v->tid >= ntents)
		badbin("bad type id");
}

static void
rtype(ulong i)
{
	int n;

	if(tents == nil)
		tents = emalloc(sizeof(Tent)*ntents);
	if(rtoks() != 8)
		badbin("bad type entry");
	tents[i].name = estrdup(toks[1]);
	tents[i].fmt = *toks[2];
	tents[i].first =  strtol(toks[3], nil, 0);
	tents[i].last =  strtol(toks[4], nil, 0);
	tents[i].nitems =  strtol(toks[5], nil, 0);
	tents[i].sz = strtol(toks[6], nil, 0);
	tents[i].etid = strtol(toks[7], nil, 0);
	switch(tents[i].fmt){
	case 'e':
		tents[i].lits = emalloc(sizeof(char*)* tents[i].nitems);
		for(n = 0; n < tents[i].nitems; n++)
			tents[i].lits[n] = estrdup(getln());
		break;
	case 'R':
		tents[i].fields = emalloc(sizeof(Vent) * tents[i].nitems);
		for(n = 0; n < tents[i].nitems; n++)
			rfield(&tents[i].fields[n]);
		break;
	}
	if(debug['F'])
		fprint(2, "tid %uld %T", i, &tents[i]);
}

static void
rpc(ulong i)
{
	static char *lfname;

	if(pcs == nil)
		pcs = emalloc(sizeof(Pc)*npcs);
	if(rtoks() != 3)
		badbin("bad pc entry");
	pcs[i].pc = strtoul(toks[0], nil, 16);
	if(lfname != nil && strcmp(toks[1], lfname) == 0)
		pcs[i].fname = lfname;
	else
		pcs[i].fname = estrdup(toks[1]);
	lfname = pcs[i].fname;
	pcs[i].lineno = strtoul(toks[2], nil, 0);
	if(debug['F'])
		fprint(2, "%05ulx\t%s:%uld\n",
			pcs[i].pc, pcs[i].fname, pcs[i].lineno);
}

static void
rvar(ulong i)
{
	if(vents == nil)
		vents = emalloc(sizeof(Vent)*nvents);
	if(rtoks() != 6)
		badbin("bad var entry");
	rvent(&vents[i]);
	if(debug['F'])
		fprint(2, "%V", &vents[i]);
}

static void
rproc(ulong i)
{
	int n;

	if(pents == nil)
		pents = emalloc(sizeof(Pent)*npents);
	if(rtoks() != 10)
		badbin("bad proc entry");
	if(strtoul(toks[0], nil, 0) != i)
		badbin("bad proc entry id");
	pents[i].name = estrdup(toks[1]);
	pents[i].addr = strtoul(toks[2], nil, 0);
	pents[i].nargs = strtoul(toks[3], nil, 0);
	pents[i].nvars = strtoul(toks[4], nil, 0);
	pents[i].retsz = strtoul(toks[5], nil, 0);
	pents[i].argsz = strtoul(toks[6], nil, 0);
	pents[i].varsz = strtoul(toks[7], nil, 0);
	pents[i].fname =  estrdup(toks[8]);
	pents[i].lineno =  strtoul(toks[9], nil, 0);
	if(pents[i].nargs > 0){
		pents[i].args = emalloc(sizeof(Vent)*pents[i].nargs);
		for(n = 0; n < pents[i].nargs; n++){
			if(rtoks() != 6)
				badbin("bad proc arg entry");
			rvent(&pents[i].args[n]);
		}
	}
	if(pents[i].nvars > 0){
		pents[i].vars = emalloc(sizeof(Vent)*pents[i].nvars);
		for(n = 0; n < pents[i].nvars; n++){
			if(rtoks() != 6)
				badbin("bad proc lvar entry");
			rvent(&pents[i].vars[n]);
		}
	}
	if(debug['F'])
		fprint(2, "%P", &pents[i]);
}

static void
rtext(void)
{
	int i, ndata;
	uint ir;
	float r;

	if(rtoks() != 2)
		badbin("bad tab header");
	if(strcmp(toks[0], "text") != 0)
		badbin("unexpected tab");
	ntext = strtoul(toks[1], nil, 0);
	text = emalloc(sizeof(u32int)*ntext);
	if(debug['F'])
		fprint(2, "tab text[%uld]\n", ntext);
	ndata = 0;
	for(i = 0; i < ntext; i++){
		rtoks();
		/* feature: ignore extra toks in line */
		if(ntoks < 2)
			badbin("bad text entry");
		if(ndata > 0){
			text[i] = strtoul(toks[1], nil, 0);
			if(debug['F'])
				fprint(2, "%05x\t%#ux\n", i, text[i]);
			ndata -= 4;
			continue;
		}
		if(toks[1][0] < 'a' || toks[1][0] > 'z')
			text[i] = strtoul(toks[1], nil, 0);
		else
			text[i] = icode(toks[1]);
		if(!hasarg(text[i])){
			if(debug['F'])
				fprint(2, "%05x\t%I\n", i, text[i]);
			continue;
		}
		if(ntoks < 3)
			panic("missing argument for %#ux %I", text[i], text[i]);
		if(i == ntext - 1)
			panic("truncated instruction");
		ir = text[i];
		if(IT(ir) == ITreal && IC(ir) != ICcast){
			r = (float)strtod(toks[2], nil);
			text[++i] = *(u32int*)&r;
			if(debug['F'])
				fprint(2, "%05x\t%I\t%e\n", i-1, ir, r);
		}else{
			text[++i] = strtoul(toks[2], nil, 0);
			if(IC(ir) == ICdata){
				ndata = text[i];
				if((ndata%4) != 0)
					badbin("bad data argument in text");
			}
			if(debug['F'])
				fprint(2, "%05x\t%I\t%ux\n", i-1, ir, text[i]);
		}
	}
	if(ndata > 0)
		panic("truncated instruction");
}

static void
rtab(char *name, ulong *np, void (*rf)(ulong))
{
	int i;

	if(rtoks() != 2)
		badbin("bad tab header");
	if(strcmp(toks[0], name) != 0)
		badbin("unexpected tab");
	*np = strtoul(toks[1], nil, 0);
	if(debug['F'])
		fprint(2, "tab %s[%uld]\n", name, *np);
	for(i = 0; i < *np; i++)
		rf(i);
}

static void
usage(void)
{
	fprint(2, "usage: %s [-wDFIMSXNv] file\n", argv0);
	exits("usage");
}

void
main(int argc, char *argv[])
{
	ARGBEGIN{
	case 'D':	/* data */
	case 'F':	/* input file */
	case 'M':	/* dynamic memory */
	case 'S':	/* stack x dump */
	case 'X':	/* excution trace */
	case 'I':	/* file input builtins */
		debug[ARGC()]++;
		break;
	case 'N':
		sflag++;
		break;
	case 'w':	/* don't wait for fucking windows */
		waitforwindows = 0;
		break;
	case 'v':
		print("%s: version %s\n", argv0, VERS);
		exits(nil);
	default:
		usage();
	}ARGEND;
	if(argc != 1)
		usage();

	assert(sizeof(u32int) == sizeof(float) && sizeof(u32int) == 4);

	fmtinstall('T', Tfmt);
	fmtinstall('v', vfmt);
	fmtinstall('V', Vfmt);
	fmtinstall('P', Pfmt);
	fmtinstall('I', Ifmt);

	bin = Bopen(argv[0], OREAD);
	if(bin == nil)
		sysfatal("%s: %r", argv[0]);
	fname = argv[0];
	rhdr();
	rtab("types", &ntents, rtype);
	rtab("vars", &nvents, rvar);
	rtab("procs", &npents, rproc);
	rtext();
	rtab("pcs", &npcs, rpc);
	free(binln);
	Bterm(bin);

	datainit();
	fnsinit();
	nofaults();
	pam();
	done(nil);
}
