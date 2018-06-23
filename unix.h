/*
 * Convenience definitions.
 */
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#include <unistd.h>
#include <time.h>
#include <fcntl.h>
#include <errno.h>
#include <limits.h>

/* we have no assert in Cygwin... */
#define assert(x)
#define NDEBUG

#include <stdint.h>
#include <stdarg.h>
#include <sys/time.h>
#include <ctype.h>
#include <signal.h>
#include <math.h>
#define PI 3.14159265359
 
#undef isspace
#undef isalpha
#undef isalnum
#undef isdigit

/* provided by russ unix repackage */
#include <lib9.h>
#include <fmt.h>
#include <bio.h>
#include "fmtdef.h"


#define getcallerpc(x) 0xbabebabe
#define setmalloctag(x, y)

#define	nelem(x)	(sizeof(x)/sizeof((x)[0]))
#define	USED(x)	if(x){}else{}
#define	SET(x)	((x)=0)
#define	ROUND(s, sz)	(((s)+((sz)-1))&~((sz)-1))

#define	ORDWR	O_RDWR

/* in unix sometimes there is ulong, sometimes not */
#define ulong	uint32_t

typedef uint16_t	u16int;
typedef uint32_t	u32int;
typedef uint64_t	u64int;
typedef unsigned char	uchar;
typedef struct	timeval Timeval;
typedef struct Sstats	Sstats;
typedef struct Ctext	Ctext;

enum
{
	Maxerr = 256,
	Kb = 1024,
};


char* strecpy(char *to, char *e, char *from);
void* mallocz(long sz, int zero);
long readn(int, void*, long);

#include <strings.h>
#define cistrcmp strcasecmp

void sysfatal(char *fmt, ...);

extern char* argv0;
/* EOL defined defined by Makefile after copying to syshdr.h */

/* command line */
extern char	*argv0;
extern void __fixargv0(void);
#define	ARGBEGIN	for((argv0?0:(argv0=(__fixargv0(),*argv))),argv++,argc--;\
			    argv[0] && argv[0][0]=='-' && argv[0][1];\
			    argc--, argv++) {\
				char *_args, *_argt;\
				Rune _argc;\
				_args = &argv[0][1];\
				if(_args[0]=='-' && _args[1]==0){\
					argc--; argv++; break;\
				}\
				_argc = 0;\
				while(*_args && (_args += chartorune(&_argc, _args)))\
				switch(_argc)
#define	ARGEND		SET(_argt);USED(_argt);USED(_argc);USED(_args);}USED(argv);USED(argc);
#define	ARGF()		(_argt=_args, _args="",\
				(*_argt? _argt: argv[1]? (argc--, *++argv): 0))
#define	EARGF(x)	(_argt=_args, _args="",\
				(*_argt? _argt: argv[1]? (argc--, *++argv): ((x), abort(), (char*)0)))

#define	ARGC()		_argc

#pragma varargck		argpos	esmprint 1

char*	estrdup(char*);
void*	emalloc(int);
void*	emallocz(int, int);
void*	erealloc(void*,int);
char*	esmprint(char *fmt, ...);
extern void	finishwait(void);

