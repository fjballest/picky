typedef struct Tent Tent;
typedef struct Vent Vent;
typedef struct Pent Pent;
typedef struct Vmem Vmem;
typedef struct Ptr Ptr;
typedef struct Pc Pc;

enum
{
	Ntoks = 15,
	Stack = 64 * 1024 * 1024,
};

struct Tent
{
	char *name;	/* of the type */
	char fmt;	/* value format character */
	long first;	/* legal value or index */
	long last;	/* idem */
	int nitems;	/* # of values or elements */
	ulong sz;	/* in memory for values */
	uint etid;	/* element type id */
	char **lits;	/* names for literals */
	Vent *fields;	/* only name, tid, and addr defined */
};

struct Vent
{
	char *name;	/* of variable or constant */
	uint tid;	/* type id */
	ulong addr;	/* in memory (offset for args, l.vars.) */
	char *fname;
	int lineno;
	char *val;	/* initial value as a string, or nil. */
	Vent **fields;	/* aggregate members */
};

struct Pent
{
	char *name;	/* for procedure/function */
	ulong addr;	/* for its code in text */
	int nargs;	/* # of arguments */
	int nvars;	/* # of variables */
	int retsz;	/* size for return type or 0 */
	int argsz;	/* size for arguments in stack */
	int varsz;	/* size for local vars in stack */
	char *fname;
	int lineno;
	Vent *args;	/* Var descriptors for args */
	Vent *vars;	/* Var descriptors for local vars. */
};

struct Pc
{
	ulong pc;
	char *fname;
	ulong lineno;
	Pc*	next;	/* Pc with leaks; for leaks */
	uint	n;	/* # of leaks in this Pc; for leaks */
};

struct Vmem
{
	int tid;
	union{
		void *p;
		int *ep;
		float *rp;
		u64int *pp;
		char *cp;
	};
};

struct Ptr
{
	uint	tid;
	u32int	pc;
	void*	p;
	Ptr*	next;
};

#pragma varargck type "T" Tent*
#pragma varargck type "V" Vent*
#pragma varargck type "P" Pent*
#pragma varargck type "v" Vmem
#pragma varargck type "I" u32int

#pragma	varargck	argpos	panic	1

typedef void (*Builtin)(void);

/*	|c/f2p pilib.c	*/
extern void	dumpglobals(void);
extern void	flushall(void);
extern void	fnsinit(void);
extern void	undisposed(void);
extern int	vfmt(Fmt *f);
extern void*	xptr(Ptr *pt);

/*	|c/f2p pi.c| grep -v main	*/
extern void	done(char *sts);
extern ulong	fetch(void);
extern void	poison(void *p, long n);
extern Pc*	findpc(u32int pc);
extern void	panic(char *fmt, ...);
extern u32int	pop32(void);
extern u64int	pop64(void);
extern void*	popdaddr(void);
extern float	popr(void);
extern void	push32(u32int c);
extern void	push64(u64int c);
extern void	push8(u32int c);
extern void	pushdaddr(void *p);
extern void	pushr(float r);
extern Tent*	tfetch(int tid);
extern u32int	top32(void);
extern void*	popn(int);

extern Builtin builtin[Nbuiltins];
extern Tent *tents;
extern Pent *pents;
extern Vent *vents;
extern ulong ntents, nvents, nheap;
extern u32int pc, procid;
extern uchar *sp, *fp, *vp, *ap, *stack;
extern int debug[];
