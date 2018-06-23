#include <u.h>
#include <libc.h>
#include <bio.h>
#include <ctype.h>

#pragma varargck		argpos	esmprint 1
#define EOL	"\n"

/*
 *	|c/f2p emalloc.c plan9.c
 */
extern Biobuf*	Bfdopen(int f, int mode);
extern void	chmodx(char *n);
extern void*	emalloc(int sz);
extern void*	emallocz(int sz, int zero);
extern void*	erealloc(void* p, int sz);
extern char*	esmprint(char *fmt, ...);
extern char*	estrdup(char* s);
extern void	finishwait(void);
extern void	nofaults(void);
extern void	nofperr(void);
