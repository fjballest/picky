typedef struct List List;
typedef struct Sym Sym;
typedef struct Stmt Stmt;
typedef struct Type Type;
typedef struct Env Env;
typedef struct Builtin Builtin;
typedef struct Code Code;
typedef struct Prog Prog;
typedef struct Pcent Pcent;
typedef struct Stats Stats;

enum
{

	Incr = 32,
	Aincr = 128,

	/* limits */
	Maxsval = 1024,
	Nbighash = 997,
	Nhash = 101,

	/* limit on arry index sizes */
	Maxidx = 0x40000000,

	Eof = -1,

	/* symbol types and subtypes */
	Snone = 0,
	Skey,		/* keyword */
	Sstr,		/* a string buffer */
	Sconst,		/* constant or literal */
	Stype,		/* type def */
	Svar,		/* obj def */
	Sunary,		/* unary expression */
	Sbinary,		/* binary expression */
	Sproc,		/* procedure */
	Sfunc,		/* function */
	Sfcall,		/* procedure or function call */

	/* Type kinds */
	Tundef = 0,
	Tint,
	Tbool,
	Tchar,
	Treal,
	Tenum,	/* 5 */
	Trange,
	Tarry,
	Trec,
	Tptr,
	Tfile,	/* 10 */
	Tproc,
	Tfunc,
	Tprog,
	Tfwd,
	Tstr,	/* 15; fake: array[int] of char; but universal */

	/* List kinds */
	Lstmt = 0,
	Lsym,

	/* Operations, besides any of < > = , + - * / % [ . and ^ */
	Onone = 0,
	Ole,
	Oge,
	Odotdot,
	Oand,
	Oor,		/* 5 */
	Oeq,
	One,
	Opow,
	Oint,
	Onil,		/* 10 */
	Ochar,
	Oreal,
	Ostr,
	Otrue,
	Ofalse,		/* 15 */
	Onot,
	Olit,
	Ocast,
	Oparm,
	Orefparm,	/* 20 */
	Olvar,
	Ouminus,
	Oaggr,
};

/*
 * One per program, procedure, and function.
 * Used to keep symbols found in it and also to collect
 * definitions for arguments, constants, types, variables, and statements.
 */
struct Env
{
	ulong id;
	Sym* tab[Nhash];	/* symbol table */
	Env* prev;		/* in stack */
	Sym* prog;		/* ongoing program, procedure, or function */
	Type* rec;		/* ongoing record definition */
};

struct List
{
	int	nitems;
	int	kind;
	union{
		Stmt**	stmt;
		Sym**	sym;
		void**	items;
	};
};

struct Builtin
{
	char *name;
	u32int id;
	int kind;
	char *args;
	char r;
	Sym* (*fn)(Builtin *b, List *args);
};

/*
 * Statements
 */
struct Stmt
{
	int	op;
	char*	sfname;
	int	lineno;
	union{
		List*	list;		/* '{' */
		struct{			/* = */
			Sym*	lval;
			Sym*	rval;
		};
		struct{			/* IF */
			Sym*	cond;
			Stmt*	thenarm;
			Stmt*	elsearm;
		};
		Sym*	fcall;		/* FCALL */
		struct{
			Sym*	expr;	/* RETURN, DO, WHILE, FOR, CASE */
			Stmt*	stmt;
			Stmt*	incr;	/* last statement in fors (i++|i--) */
		};
	};
};

/*
 * Types
 */
struct Type
{
	int	op;
	Sym*	sym;
	int	first;
	int	last;
	union{
		List*	lits;			/* Tenum */
		Type*	ref;			/* Tptr */
		Type* super;			/* Trange */
		struct{				/* Tarry, Tstr */
			Type* idx;
			Type* elem;
		};
		List*	fields;			/* Trec */
		struct{
			List* parms;		/* Tproc, Tfunc */
			Type* rtype;
		};
	};

	/* backend */
	ulong	id;
	ulong	sz;
};

/* pc/src table */
struct Pcent
{
	Pcent*	next;
	Stmt*	st;
	Sym*	nd;
	ulong	pc;
};

/* generated code */
struct Code
{
	u32int	addr;
	Pcent*	pcs;
	Pcent*	pcstl;
	u32int*	p;
	ulong	np;
	ulong	ap;
};

struct Prog
{
	Sym* psym;
	List* parms;
	Type* rtype;	/* ret type or nil if none */
	List* consts;
	List* types;
	List* vars;
	List* procs;
	Stmt* stmt;
	Builtin *b;
	int nrets;

	/* backend */
	Code	code;
	ulong	parmsz;
	ulong	varsz;
};

/*
 * Symbol table entry.
 */
struct Sym
{
	ulong	id;
	char*	name;
	Sym*	hnext;
	int	stype;
	int	op;

	char*	fname;
	int	lineno;

	Type*	type;

	union{
		int	tok;
		long	ival;
		double	rval;
		char*	sval;
		List*	vals;
		struct{
			int	used;
			int	set;
		};
		struct{			/* binary, unary */
			Sym*	left;
			Sym*	right;
		};
		struct{			/* Sfcall */
			Sym*	fsym;
			List*	fargs;
		};
		struct{			/* "." */
			Sym*	rec;
			Sym*	field;
		};
		struct{
			Sym*	swfield;	/* switch field */
			Sym*	swval;	/* variant */
		};
		Prog*	prog;
	};

	/* backend */

	union{
		ulong	addr;
		ulong	off;	/* fields */
	};

};

struct Stats
{
	ulong	nenvs;	/* # of envs used */
	ulong	menvs;	/* # of envs allocated */
	ulong	nsyms;	/* # of syms allocated */
	ulong	nexpr;	/* # of syms for expressions */
	ulong	nlists;	/* # of lists allocated */
	ulong	mlist;	/* # of items in longest list */
	ulong	nstmts;	/* # of stmts allocated */
	ulong	nprogs;	/* # of progs allocated */
	ulong	ntypes;	/* # of types allocated */
	ulong	nstrs;	/* # of strings allocated */
	ulong	nhash;	/* # of lookups in hash */
	ulong	nlink;	/* # of loopups using overflow links */
	ulong	mlink;	/* # for longest overflow list used in hash */
};

#pragma varargck type "N" Sym*
#pragma varargck type "T" Type*
#pragma varargck type "t" Type*
#pragma varargck type "X" Stmt*
#pragma varargck type "R" Stmt*
#pragma varargck type "E" Env*


/*	|c/f2p lex.c	*/
extern void	main(int argc, char *argv[]);
extern long	yylex(void);

/*	|c/f2p sym.c	*/
extern int	Nfmt(Fmt *f);
extern int	Xfmt(Fmt *f);
extern void	addstmt(List *l, Stmt *n);
extern void	addsym(List *l, Sym *n);
extern void	addsym0(List *l, Sym *n);
extern List*	alloclist(void);
extern Prog*	allocprog(void);
extern Sym*	allocsym(void);
extern void	appsyms(List *l1, List *l2);
extern Sym*	caseexpr(Sym *vnd, Sym *e);
extern int	checkdup(Sym *s);
extern void	cpsrc(Stmt *to, Stmt *from);
extern Sym*	declconst(Sym *n, Sym *expr);
extern Sym*	declfield(Sym *n, Type *t);
extern Sym*	declfunc(Sym *n);
extern Sym*	declproc(Sym *n);
extern void	declprogdone(Sym *n);
extern Sym*	decltype(Sym *n, Type *t);
extern Sym*	declvar(Sym *n, Type *t);
extern Sym*	defssym(Sym *s, int kind);
extern Sym*	defsym(char *n, int kind);
extern void	dumpenv(int fd, Env *e, int recur);
extern void	dumpprog(int fd, Sym *s);
extern void	evalexpr(Sym *n);
extern int	evaluated(Sym *s);
extern Sym*	fieldaccess(Sym *lval, char *fld);
extern int	islval(Sym *n);
extern Sym*	keylookup(char *n);
extern Sym*	lookup(char *n, int kind);
extern Stmt*	newassign(Sym *lval, Sym *rval);
extern Stmt*	newbody(List *l);
extern Sym*	newexpr(int k, int op, Sym *s1, Sym *s2);
extern Sym*	newfcall(Sym *f, List *args, int op);
extern Stmt*	newfor(Sym *lval, Sym *from, Sym *to, Stmt *body);
extern Sym*	newint(long ival, int op, Type *t);
extern List*	newlist(int lkind);
extern Sym*	newparm(Sym *n, Type *t, int byref);
extern Sym*	newprog(Sym *n);
extern Sym*	newreal(double rval, Type *t);
extern Stmt*	newstmt(int op);
extern Sym*	newstr(char *sval);
extern Type*	newstrtype(int len);
extern Stmt*	newswitch(Sym *expr, List *cases);
extern Sym*	newvarnode(Sym *s);
extern char*	opname(int op);
extern void	popenv(void);
extern Env*	pushenv(void);
extern void	setswfield(List *l, Sym *sw);
extern void	setswval(List *l, Sym *sw);
extern Sym*	strlookup(char *n);
extern void	syminit(void);
extern void	checkcond(Stmt*, Sym*);

/*	|c/f2p type.c	*/
extern int	Tfmt(Fmt *f);
extern void	checkundefs(void);
extern void	declstdtypes(List *tl);
extern char*	enumname(Type *t, long v);
extern void	initrectype(Type *t);
extern Sym*	newaggr(Type *t, List *nl);
extern Type*	newarrytype(Type *idx, Type *elem);
extern Type*	newordtype(List *nl);
extern Type*	newrangetype(Type *super, Sym *v0, Sym *v1);
extern Type*	newtype(int op);
extern int	returnsok(Stmt *x, Type *rt);
extern int	setslval(Stmt *x, Sym *v);
extern int	setused(Stmt *x, Sym *v);
extern int	tchkbinary(Sym *n);
extern int	tchkcall(Sym *fs, List *args);
extern int	tchkunary(Sym *n);
extern int	tcompat(Type *t0, Type *t1, Type **tp);
extern Type*	tderef(Type *t);
extern long	tfirst(Type *t);
extern int	tis(Type *t, int op);
extern int	tisatom(Type *t);
extern int	tiscmp(Type *t);
extern int	tisord(Type *t);
extern long	tlast(Type *t);
extern ulong	tlen(Type *t);
extern ulong	tsz(Type *t);
extern void	typeinit(void);

/*	|c/f2p builtin.c		*/
extern int	bargcheck(Builtin *b, List *al, char *as);
extern void	builtininit(void);
extern Type*	brtype(Builtin *b, List *al);

/*	|c/f2p gen.c		*/
extern int	Rfmt(Fmt *f);
extern void	gen(Biobuf *bout, char *nm);

#pragma	varargck	argpos	yyerror	1
#pragma	varargck	argpos	symerror	2
#pragma	varargck	argpos	stmterror	2
#pragma	varargck	argpos	diag	1
#pragma	varargck	argpos	oprint	1

/*	|c/f2p p.y		*/
extern void	diag(char* fmt, ...);
extern void	stmterror(Stmt *s, char* fmt, ...);
extern void	symerror(Sym *s, char* fmt, ...);
extern void	yyerror(char* fmt, ...);

extern Env *env;
extern int lineno;
extern char *fname;
extern char *topname[];
extern int debug[];
extern char sval[];
extern int nerrors;
extern ulong tgen;
extern Type *tundef, *tint, *treal, *tchar, *tbool, *tfile;
extern Type *tcchar, *tcbool, *tcreal, *tcint, *tcnil;
extern Sym *badnode, *bpred, *bsucc, *pstdin, *pstdout;
extern Stats stats;
extern int globalsok;
extern double EPS;

