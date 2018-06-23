#include "syshdr.h"
#include "pam.h"
#include "pilib.h"

typedef struct Xfile Xfile;

enum
{
	Incr = 8
};


struct Xfile
{
	Biobuf *b;
	int eof;
	int eol;
	char *err;
	int mode;
	char *fname;
};

/*
 * File I/O is a clear departure from the Pascal heritage.
 *
 * read(char) may be used to do (buffered) i/o one char at a time.
 * Special constants Eol and Eof may be obtained at end of line/file.
 * Once Eol is found, it must be read using readeol().
 * Eol may not  be written. writeeol() must be used instead.
 *
 * further calls to read(char) at Eof (after it has been returned once)
 * are a fault.
 *
 * Functions eof() and eol() report if the file is into eol/eof conditions.
 * That is, before reading, eof() would report False even if there's
 * nothing else to read.
 *
 * There are two advantages for this scheme:
 *	- it works fine for both streams and actual files.
 *	- text files are portable
 *	(in windows, '\r' may trigger the Eol condition and readeol() may
 *	 read the '\n' after it.; in unix, readeol() does nothing.)
 *	EOL contains the end of line string ("\n" or "\r\n"), code assumes
 *	it has one or two characters.
 */

static Xfile **files;
static int nfiles;
static Ptr *ptrs;
ulong nheap;
static char FD0[] = "/fd/0";
static char FD1[] = "/fd/1";


void
flushall(void)
{
	int i;

	for(i = 0; i < nfiles; i++)
		if(files[i] != nil && files[i]->b != nil && files[i]->mode == OWRITE)
			Bflush(files[i]->b);
}

static int
xbopen(char *name, char *m)
{
	int i, mode;

	for(i = 0; i < nfiles; i++)
		if(files[i] == nil)
			break;
	if(i == nfiles){
		if((nfiles%Incr) == 0)
			files = erealloc(files, (nfiles+Incr)*sizeof(Biobuf*));
		i = nfiles++;
	}
	mode = OREAD;
	if(strcmp(m, "rw") == 0)
		panic("mode 'rw' not supported in this implementation.");
	else if(strcmp(m, "w") == 0)
		mode = OWRITE;
	else if(strcmp(m, "r") != 0)
		panic("bad open mode '%s'", m);

	files[i] = emalloc(sizeof(Xfile));
	if(strcmp(name, FD0) == 0)
		files[i]->b = Bfdopen(0, mode);
	else if(strcmp(name, FD1) == 0)
		files[i]->b = Bfdopen(1, mode);
	else
		files[i]->b = Bopen(name, mode);
	files[i]->fname = estrdup(name);
	files[i]->mode = mode;
	if(files[i]->b == nil)
		panic("open: '%s': %r", name);
	return i;
}

void
fnsinit(void)
{
	xbopen(FD0, "r");
	xbopen(FD1, "w");
}

static void
xbclose(int i)
{
	if(i >= nfiles || files[i] == nil)
		panic("file not open");
	if(files[i]->b != nil){
		Bflush(files[i]->b);	/* not needed */
		Bterm(files[i]->b);
	}
	free(files[i]->err);
	free(files[i]);
	files[i] = nil;
}

static Xfile*
xbfile(int i)
{
	if(i >= nfiles || files[i] == nil)
		panic("file not open");
	return files[i];
}


static void
xacos(void)
{
	pushr(acos(popr()));
}

static void
xasin(void)
{
	pushr(asin(popr()));
}

static void
xatan(void)
{
	pushr(atan(popr()));
}

static void
xcos(void)
{
	pushr(cos(popr()));
}

static void
xnew(void)
{
	u64int *p;
	Ptr *pt;
	int sz;
	Tent *t;

	t = tfetch(pop32());
	sz = tents[t->etid].sz;
	p = popdaddr();
	pt = emalloc(sizeof *pt);
	pt->pc = pc;
	pt->tid = t->etid;
	pt->p = malloc(sz);
	if(pt->p == nil)
		panic("no more memory");
	poison(pt->p, sz);
	nheap += sz;
	pt->next = ptrs;
	ptrs = pt;
	*p = (u64int)pt;
	if(debug['M'])
		fprint(2, "newptr: Ptr %#p p %#p\n", pt, pt->p);
}

static void
xdispose(void)
{
	Ptr **p;
	Ptr *pt;

	p = popdaddr();
	if((u64int)p & 1)
		panic("dispose of a not initialized pointer");
	if(p == nil)
		panic("dispose of a dangling pointer");
	pt = *p;
	if(pt->p == nil)
		panic("memory already disposed");
	if(debug['M'])
		fprint(2, "freeptr: Ptr %#p p %#p\n", pt, pt->p);
	free(pt->p);
	pt->p = nil;
	nheap -= tents[pt->tid].sz;
}

void*
xptr(Ptr *pt)
{
	if((u64int)pt & 1)
		panic("dereferencing a dangling pointer");
	if(pt == nil)
		panic("dereferencing a nil pointer");
	if(debug['M'])
		fprint(2, "xptr: Ptr %#p p %#p\n", pt, pt->p);
	if(pt->p == nil)
		panic("attempt to use disposed memory");
	return pt->p;
}

void
undisposed(void)
{
	int once;
	Pc *pe, *leaks;

	once = 0;
	leaks = nil;
	for(; ptrs != nil; ptrs = ptrs->next)
		if(ptrs->p != nil){
			if(once++ == 0)
				fprint(2, "memory leaks:\n");
			pe = findpc(ptrs->pc);
			if(pe == nil)
				fprint(2, "pc %#ux\n", ptrs->pc);
			else{
				if(pe->n == 0){
					pe->next = leaks;
					leaks = pe;
				}
				pe->n++;
			}
		}
	for(pe = leaks; pe != nil; pe = pe->next)
		fprint(2, "%s:%uld\t(%d times)\n", pe->fname, pe->lineno, pe->n);
}

static void
xexp(void)
{
	pushr(exp(popr()));
}

static void
xfatal(void)
{
	char buf[512];
	Tent *t;
	int i;
	int *cp;

	flushall();
	t = tfetch(pop32());
	cp = popn(t->sz);
	for(i = 0; i < t->nitems && i < nelem(buf); i++)
		buf[i] = (char)cp[i];
	buf[i] = 0;
	if(i == 0)
		done("fatal");
	fprint(2, "fatal: %s\n", buf);
	done(buf);
}

static void
xclose(void)
{
	xbclose(pop32());
}

static void
xfeof(void)
{
	Xfile *f;
	int fid;

	fid = pop32();
	f = xbfile(fid);
	if(f->eof != 0)
		push32(1);
	else
		push32(0);
	if(debug['I'])
		fprint(2, "feof: file %d: eol %d eof %d\n", fid, f->eol, f->eof);
}

static void
xfeol(void)
{
	Xfile *f;
	int fid;

	fid = pop32();
	f = xbfile(fid);
	if(f->eol != 0)
		push32(1);
	else
		push32(0);
	if(debug['I'])
		fprint(2, "feol: file %d: eol %d eof %d\n", fid, f->eol, f->eof);
}

static void
xfpeek(void)
{
	Xfile *f;
	u32int *cp;
	int c;
	int fid;

	fid = pop32();
	f = xbfile(fid);
	cp = popdaddr();
	flushall();
	if(f->eof != 0)
		panic("peek: eof met");
	if(f->eol != 0)
		c = EOL[0];
	else{
		c = Bgetc(f->b);
		if(c < 0){
			c = 0xFF;
			f->eof = 1;
		}else{
			if(c == EOL[0])
				f->eol = 1;
			else
				Bungetc(f->b);
		}
	}
	*cp = c;
	if(debug['I'])
		fprint(2, "fpeek: c '%c' file %d: eol %d eof %d\n",
			c, fid, f->eol, f->eof);
}


static char*
readword(Xfile *f, int isnum)
{
	static char buf[512];
	char *sp, *ep;
	int c;

	sp = buf;
	ep = buf + sizeof buf;
	*--ep = 0;
	do{
		c = Bgetc(f->b);
	}while(c >= 0 && isspace(c));
	do{
		if(c < 0 && sp == buf)
			panic("read: eof met");
		*sp++ = c;
		c = Bgetc(f->b);
		if(isnum && strchr("0123456789+-eE.", c) == nil)
			break;
	}while(sp < ep && c >= 0 && !isspace(c));
	if(c >= 0){
		f->eol = c == EOL[0];
		if(f->eol == 0)
			Bungetc(f->b);
	}else
		f->eof = 1;
	*sp = 0;
	if(buf[0] == 0)
		panic("read: eof met");
	return buf;
}

static void
_xfread(int tid, Xfile *f)
{
	int n, i;
	char *s, *e;
	void *d;

	flushall();
	if(f->eof != 0)
		panic("read: eof met");
	if(f->mode == OWRITE)
		panic("read: file open for writing");
	d = popdaddr();
	switch(tents[tid].fmt){
	case 'i':
		s = readword(f, 1);
		n = strtol(s, &e, 0);
		if(s == e)
			panic("read: no int value found");
		if(e != nil && strlen(e) > 0)
			panic("bad formed input number");
		if(n < tents[tid].first || n > tents[tid].last)
			panic("read: value is out of range");
		*(int*)d = n;
		break;
	case 'e':
		s = readword(f, 0);
		for(i = 0; i < tents[tid].nitems; i++)
			if(cistrcmp(s, tents[tid].lits[i]) == 0){
				*(int*)d = tents[tid].first + i;
				return;
			}
		panic("read: no enumerated value found");
		break;
	case 'c':
		if(f->eol != 0)
			panic("read: at end of line");
		n = Bgetc(f->b);
		if(n < 0){
			n = 0xFF;
			f->eof = 1;
			f->eol = 0;
		}else{
			f->eol = n == EOL[0];
			if(n < tents[tid].first || n > tents[tid].last)
				panic("read: value is out of range");
		}
		*(int*)d = n;
		break;
	case 'b':
		s = readword(f, 0);
		n = 0;
		if(cistrcmp(s, "True") == 0)
			n = 1;
		else if(cistrcmp(s, "False") != 0)
			panic("read: no bool value found");
		*(int*)d = n;
		break;
	case 'r':
		s = readword(f, 1);
		*(float*)d = strtod(s, &e);
		if(s == e)
			panic("read: no float value found");
		if(e != nil && strlen(e) > 0)
			panic("bad formed input number");
		break;
	case 'a':
		for(i = 0; i < tents[tid].nitems; i++){
			n = Bgetc(f->b);
			((int*)d)[i] = n;
			if(n == EOL[0])
				panic("read: eol");
			if(n < 0)
				panic("read: eof met");
		}
		break;
	default:
		panic("read: can't read variables of this type");
	}
}

static void
xfread(void)
{
	Xfile *f;
	int tid, fid;

	tid = pop32();
	if(tid < 0 || tid >= ntents)
		panic("bad tid");
	fid = pop32();
	f = xbfile(fid);
	_xfread(tid, f);
	if(debug['I'])
		fprint(2, "fread: file %d: eol %d eof %d\n", fid, f->eol, f->eof);
}

static void
xfreadeol(void)
{
	Xfile *f;
	int fid, c;

	fid = pop32();
	f = xbfile(fid);
	if(f->eof != 0)
		panic("read: eof met");
	if(f->eol == 0)
		panic("read: not at end of line");
	flushall();
	if(EOL[1] != 0){
		c = Bgetc(f->b);
		if(c != EOL[1])
			panic("read: broken end of line");
	}
	f->eol = 0;
	if(debug['I'])
		fprint(2, "freadeol: file %d: eol %d eof %d\n", fid, f->eol, f->eof);
}

static void
xfreadln(void)
{
	Xfile *f;
	int tid, fid, c;

	tid = pop32();
	if(tid < 0 || tid >= ntents)
		panic("bad tid");
	fid = pop32();
	f = xbfile(fid);
	_xfread(tid, f);
	if(f->eol)		/* perhaps an empty line */
		goto Eol;
	do{
		c = Bgetc(f->b);
	}while(c >= 0 && c != EOL[0]);

	if(c == EOL[0]){
	Eol:	if(EOL[1] != 0){
			c = Bgetc(f->b);
			if(c != EOL[1])
				panic("freadln: broken end of line");
		}
		f->eol = 0;
	}else
		f->eof = 1;
	if(debug['I'])
		fprint(2, "freadln: file %d: eol %d eof %d\n", fid, f->eol, f->eof);	
}

static void
xfrewind(void)
{
	Xfile *f;
	int fid;

	fid = pop32();
	f = xbfile(fid);
	Bseek(f->b, 0, 0);
	f->eof = f->eol = 0;
	if(debug['I'])
		fprint(2, "frewind: file %d: eol %d eof %d\n", fid, f->eol, f->eof);
}

static void
_xfwrite(int nl)
{
	Xfile *f;
	int i;
	void *d;
	Tent *t;
	float r;

	t = tfetch(pop32());
	f = xbfile(pop32());
	if(f->mode == OREAD)
		panic("write: file open for reading");
	switch(t->fmt){
	case 'i':
		Bprint(f->b, "%d", pop32());
		break;
	case 'c':
		i = pop32();
		if(i == EOL[0])
			panic("write: end of line");
		/*
		 * NB: This check prevents using the file data type
		 * to handle binary files.
		 */
		if(i == 0xFF || i < 0)
			panic("write: can't write eof");
		if(i > 0xFF)
			panic("char value out of range");
		Bprint(f->b, "%c", (char)i);
		break;
	case 'b':
		if(pop32() != 0)
			Bprint(f->b, "True");
		else
			Bprint(f->b, "False");
		break;
	case 'r':
		r = popr();
		Bprint(f->b, "%#.5g", r);
		break;
	case 's':
	Str:
		d = popn(t->sz);
		for(i = 0; i < t->nitems; i++)
			Bprint(f->b, "%C", (Rune)((u32int*)d)[i]);
		break;
	case 'e':
		i = pop32();
		if(i < t->first || i > t->last)
			panic("can't print a value out of range");
		Bprint(f->b, "%s", t->lits[i-t->first]);
		break;
	case 'a':
		if(tents[t->etid].fmt == 'c')
			goto Str;
		/* fall */
	default:
		panic("can't write variables of type '%c'", t->fmt);
	}
	if(nl){
		Bwrite(f->b, EOL, strlen(EOL));
		Bflush(f->b);
	}
}

static void
xfwriteeol(void)
{
	Xfile *f;

	f = xbfile(pop32());
	if(f->mode == OREAD)
		panic("write: file open for reading");
	Bwrite(f->b, EOL, strlen(EOL));
	Bflush(f->b);
}

static void
xfflush(void)
{
	Xfile *f;

	f = xbfile(pop32());
	Bflush(f->b);
}

static void
xfwrite(void)
{
	_xfwrite(0);
}

static void
xfwriteln(void)
{
	_xfwrite(1);
}

static void
xlog(void)
{
	pushr(log(popr()));
}

static void
xlog10(void)
{
	pushr(log10(popr()));
}

static void
xopen(void)
{
	u32int *fp, i;
	char buf[512];
	char mode[3];
	int *fnp, *fmp;
	Tent *t1, *t2;

	t1 = tfetch(pop32());
	t2 = tfetch(pop32());
	fp = popdaddr();
	fnp = popn(t1->sz);
	fmp = popn(t2->sz);
	for(i = 0; i < t1->nitems && i < nelem(buf); i++)
		buf[i] = (char)fnp[i];
	buf[i] = 0;
	for(i = 0; i < t2->nitems && i < nelem(mode); i++)
		mode[i] = (char)fmp[i];
	mode[i] = 0;
	*fp = xbopen(buf, mode);
}

static void
xpow(void)
{
	float r1, r2;

	r1 = popr();
	r2 = popr();
	pushr(pow(r1, r2));
}

/*
 * BUG: pred/1 and succ/1 should become pred/2 and succ/2,
 * and receive the type id for the argument. This is needed
 * to perform range checks in them.
 * Otherwise, if the result is not assigned to a variable,
 * an out of range condition might be hidden, like in:
 *	writeln(succ(Maxint));
 */
static void
xpred(void)
{
	push32(pop32() - 1);
}

static void
xsucc(void)
{
	push32(pop32() + 1);
}

static void
xsin(void)
{
	pushr(sin(popr()));
}

static void
xsqrt(void)
{
	pushr(sqrt(popr()));
}

static void
xtan(void)
{
	pushr(tan(popr()));
}

static void
tabs(Fmt *f, int lvl)
{
	while(lvl-- > 0)
		fmtprint(f, "\t");
}

static void
dumpc(Fmt *f, int c)
{
	switch(c){
	case '\b':
		fmtprint(f, "'\\b'");
		break;
	case '\r':
		fmtprint(f, "'\\r'");
		break;
	case '\n':
		fmtprint(f, "'\\n'");
		break;
	case '\t':
		fmtprint(f, "'\\t'");
		break;
	default:
		if(c >= 0 && c < 0x20)
			fmtprint(f, "'\\%03o'", c);
		else if(c >= 0x20 && c < 0xFFFF)
			fmtprint(f, "'%C'", (Rune)c);
		else
			fmtprint(f, "'\\???'");
	}
}

static int vlvl;

int
vfmt(Fmt *f)
{
	Vmem v, nv;
	int i;
	Tent *t;
	Vent *fld;

	v = va_arg(f->args, Vmem);
	t = &tents[v.tid];
	switch(t->fmt){
	case 'b':
		if(*v.ep)
			fmtprint(f, "True");
		else
			fmtprint(f, "False");
		break;
	case 'c':
		dumpc(f, *v.ep);
		break;
	case 'i':
		if(*v.ep < t->first || *v.ep > t->last)
			fmtprint(f, "out of range");
		else
			fmtprint(f, "%d", *v.ep);
		break;
	case 'e':
		if(*v.ep < t->first || *v.ep > t->last)
			fmtprint(f, "out of range");
		else
			fmtprint(f, "%s", t->lits[*v.ep - t->first]);
		break;
	case 'r':
		fmtprint(f, "%f", *v.rp);
		break;
	case 'p':
		if(*v.pp == 0ULL)
			fmtprint(f, "nil");
		else
			fmtprint(f, "%#ullx", *v.pp);
		break;
	case 'a':
		if(tents[t->etid].fmt == 'c')
			goto Str;
		fmtprint(f, "{\n");
		vlvl++;
		nv.tid = t->etid;
		nv.cp = v.cp;
		for(i = 0; i < t->nitems; i++){
			tabs(f, vlvl);
			fmtprint(f, "[%d] %v,\n", i, nv);
			nv.cp += tents[t->etid].sz;
		}
		vlvl--;
		tabs(f, vlvl);
		fmtprint(f, "}");
		break;
	case 'R':
		fmtprint(f, "{\n");
		vlvl++;
		nv.cp = v.cp;
		for(i = 0; i < t->nitems; i++){
			fld = &t->fields[i];
			nv.tid = fld->tid;
			tabs(f, vlvl);
			fmtprint(f, ".%s = %v,\n", fld->name, nv);
			nv.cp += tents[fld->tid].sz;
		}
		vlvl--;
		tabs(f, vlvl);
		fmtprint(f, "}");
		break;
	case 'X':
		fmtprint(f, "<procedure>");
		break;
	case 'f':
		if(*v.ep < 0 || *v.ep >= nfiles)
			return fmtprint(f, "invalid file");
		if(files[*v.ep] == nil)
			return fmtprint(f, "closed file");
		switch(*v.ep){
		case 0:
			fmtprint(f, "<stdin>");
			break;
		case 1:
			fmtprint(f, "<stdout>");
			break;
		default:
			fmtprint(f, "<file #%d>", *v.ep);
		}
		break;
	case 'F':
		fmtprint(f, "<function>");
		break;
	case 's':
	Str:
		fmtprint(f, "\"");
		for(i = 0; i < t->nitems; i++){
			fmtprint(f, "%C", (Rune)*v.cp);
			v.cp += 4;
		}
		fmtprint(f, "\"");
		break;
	default:
		sysfatal("vfmt: fmt %#ux", t->fmt);
	}
	return 0;
}

static void
dumplocals(char *tag, int n, Vent *vents, uchar *lp)
{
	int i, j;
	Vmem v;

	for(i = 0; i < vlvl; i++)
		fprint(2, "\t");
	fprint(2, "%s:\n", tag);
	for(i = 0; i < n; i++){
		v.tid = vents[i].tid;
		v.p = lp + vents[i].addr;
		for(j = 0; j < vlvl; j++)
			fprint(2, "\t");
		if(debug['S'] || debug['D'] || debug ['M'])
			fprint(2, "%#p  ", v.p);
		fprint(2, "%s = %v\n", vents[i].name, v);
	}
}

void
dumpglobals(void)
{
	if(nvents > 0)
		dumplocals("global variables", nvents, vents, stack);
}

static void
dumpheap(void)
{
	Ptr *pt;
	int i;
	Vmem v;

	if(ptrs == nil)
		return;
	for(i = 0; i < vlvl; i++)
		fprint(2, "\t");
	fprint(2, "heap:\n");
	for(pt = ptrs; pt != nil; pt = pt->next)
		if(pt->p != nil){
			v.tid = pt->tid;
			v.p = pt->p;
			for(i = 0; i < vlvl; i++)
				fprint(2, "\t");
			fprint(2, "%#p = %v\n", pt, v);
		}
}

typedef struct SFrame SFrame;
struct SFrame
{
	u32int	pc;
	u32int	pid;
	u32int*	fp;
	uchar*	vp;
	uchar*	ap;
};

static void
proctrace(SFrame fr)
{
	Pc *pe;
	Pent *pent;

	pent = &pents[fr.pid];
	fprint(2, "%s() pid %d\tpc %#08ux", pent->name, fr.pid, fr.pc);
	if(0)
		fprint(2, " args %d vars %d", pent->nargs, pent->nvars);
	pe = findpc(fr.pc);
	if(pe != nil)
		fprint(2, " %s:%uld\n", pe->fname, pe->lineno);
	else
		fprint(2, "\n");
	vlvl++;
	if(pent->nargs > 0)
		dumplocals("arguments", pent->nargs, pent->args, fr.ap);
	if(pent->nvars > 0)
		dumplocals("local variables", pent->nvars, pent->vars, fr.vp);
	vlvl--;
	fprint(2, "\n");
}

static void*
getdaddr(u32int *p)
{
	u64int a;

	a = p[1];
	a <<= 32;
	a |= p[0];
	return (void*)a;
}

static int
nextproc(SFrame *fr)
{
	u32int *fp;

	fp = fr->fp;
	fr->pc = fp[-1];		/* saved pc */
	if(fr->pc == ~0)
		return -1;
	fr->pid = fp[-2];
	fr->fp = getdaddr(fp-4);
	fr->vp = getdaddr(fp-6);
	fr->ap = getdaddr(fp-8);
	return 0;
}

void
xstack(void)
{
	SFrame fr;

	fr.pid = procid;
	fr.pc = pc;
	fr.vp = vp;
	fr.ap = ap;
	fr.fp = (u32int*)fp;
	fprint(2, "stack trace at:\n");
	for(;;){
		proctrace(fr);
		if(nextproc(&fr) < 0)
			break;
		fprint(2, "called from:\n");
	}
}

static void
xdata(void)
{
	dumpglobals();
	dumpheap();
}

Builtin builtin[Nbuiltins] = {
	[PBacos]	xacos,		/* real */
	[PBasin]		xasin,		/* real */
	[PBatan]	xatan,		/* real */
	[PBcos]		xcos,		/* real */
	[PBdispose]	xdispose,	/* ptr */
	[PBexp]		xexp,		/* real */
	[PBfatal]	xfatal,		/* tid array of char */
	[PBclose]	xclose,		/* file */
	[PBfeof]	xfeof,		/* file */
	[PBfeol]	xfeol,		/* file */
	[PBfpeek]	xfpeek,		/* file char */
	[PBfread]	xfread,		/* tid file &arg_of_type_tid */
	[PBfreadln]	xfreadln,	/* tid file &arg_of_type_tid */
	[PBfreadeol]	xfreadeol,	/* file */
	[PBfrewind]	xfrewind,	/* file */
	[PBfwrite]	xfwrite,		/* tid file &arg_of_type_tid */
	[PBfwriteln]	xfwriteln,	/* tid file &arg_of_type_tid */
	[PBfwriteeol]	xfwriteeol,	/* file */
	[PBfflush]	xfflush,	/* file */
	[PBlog]		xlog,		/* real */
	[PBlog10]	xlog10,		/* real */
	[PBnew]		xnew,		/* tid ptr */
	[PBopen]	xopen,		/* tid x2 file array of char x2 */
	[PBpow]		xpow,		/* real real  */
	[PBpred]	xpred,		/* int */
	[PBsin]		xsin,		/* real */
	[PBsqrt]	xsqrt,		/* real */
	[PBsucc]	xsucc,		/* int */
	[PBtan]		xtan,		/* real */
	[PBstack]	xstack,		/* void */
	[PBdata]	xdata,		/* void */
};
