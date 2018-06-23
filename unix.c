#include "syshdr.h"

char *argv0;

int _CRT_fmode = _OBINARY;
int _fmode = _OBINARY;


char*
strecpy(char *to, char *e, char *from)
{
	if(to >= e)
		return to;
	to = memccpy(to, from, '\0', e - to);
	if(to == nil){
		to = e - 1;
		*to = '\0';
	}else{
		to--;
	}
	return to;
}

void*
mallocz(long sz, int zero)
{
	void *p;

	p = malloc(sz);
	if(p == nil){
		fprint(2, "%s: no memory\n", argv0);
		return nil;
	}
	if(zero)
		memset(p, 0, sz);
	return p;
}

long
readn(int f, void *av, long n)
{
	char *a;
	long m, t;

	a = av;
	t = 0;
	while(t < n){
		m = read(f, a+t, n-t);
		if(m <= 0){
			if(t == 0)
				return m;
			break;
		}
		t += m;
	}
	return t;
}

void
sysfatal(char *fmt, ...)
{
	va_list arg;
	char buf[1024];
	int n;

	n = 0;
	if(argv0 != nil)
		n = snprint(buf, sizeof(buf), "%s: ", argv0);
	va_start(arg, fmt);
	n += vsnprint(buf+n, sizeof(buf)-n, fmt, arg);
	va_end(arg);
	n += snprint(buf+n, sizeof(buf)-n, "\n");
	buf[sizeof(buf)-1] = 0;
	
	write(2, buf, n);
	exit(1);
}


void
exits(char *s)
{
	if(s == nil || strcmp(s, "") == 0)
		exit(0);
	else
		exit(1);
}

void
chmodx(char *n)
{
	chmod(n, 0755);
}

void
nofperr(void)
{
	sigset_t mask, omask;
	/*
	 * disable FP exceptions due to builtin calls with
	 * bad user arguments.
	 */

	sigemptyset(&mask);
	sigaddset(&mask, SIGFPE);
	//sigprocmask(SIG_BLOCK, &mask, &omask);
	return;	
}

void __fixargv0(void) { }


static char qsep[] = " \t\r\n";

static char*
qtoken(char *s, char *sep)
{
	int quoting;
	char *t;

	quoting = 0;
	t = s;	/* s is output string, t is input string */
	while(*t!='\0' && (quoting || utfrune(sep, *t)==nil)){
		if(*t != '\''){
			*s++ = *t++;
			continue;
		}
		/* *t is a quote */
		if(!quoting){
			quoting = 1;
			t++;
			continue;
		}
		/* quoting and we're on a quote */
		if(t[1] != '\''){
			/* end of quoted section; absorb closing quote */
			t++;
			quoting = 0;
			continue;
		}
		/* doubled quote; fold one quote into two */
		t++;
		*s++ = *t++;
	}
	if(*s != '\0'){
		*s = '\0';
		if(t == s)
			t++;
	}
	return t;
}

static char*
etoken(char *t, char *sep)
{
	int quoting;

	/* move to end of next token */
	quoting = 0;
	while(*t!='\0' && (quoting || utfrune(sep, *t)==nil)){
		if(*t != '\''){
			t++;
			continue;
		}
		/* *t is a quote */
		if(!quoting){
			quoting = 1;
			t++;
			continue;
		}
		/* quoting and we're on a quote */
		if(t[1] != '\''){
			/* end of quoted section; absorb closing quote */
			t++;
			quoting = 0;
			continue;
		}
		/* doubled quote; fold one quote into two */
		t += 2;
	}
	return t;
}

int
gettokens(char *s, char **args, int maxargs, char *sep)
{
	int nargs;

	for(nargs=0; nargs<maxargs; nargs++){
		while(*s!='\0' && utfrune(sep, *s)!=nil)
			*s++ = '\0';
		if(*s == '\0')
			break;
		args[nargs] = s;
		s = etoken(s, sep);
	}

	return nargs;
}


int
tokenize(char *s, char **args, int maxargs)
{
	int nargs;

	for(nargs=0; nargs<maxargs; nargs++){
		while(*s!='\0' && utfrune(qsep, *s)!=nil)
			s++;
		if(*s == '\0')
			break;
		args[nargs] = s;
		s = qtoken(s, qsep);
	}

	return nargs;
}


static void
faulthndlr(int i)
{
	char nb[80];
	extern void faulted(char*);

	snprint(nb, sizeof nb, "fault number %d", i);
	faulted(nb);
}

void
nofaults(void)
{
	signal(SIGSEGV, faulthndlr);
}

int iswin = ISCYGWIN;
/* only waits in windows */
void
finishwait(void)
{
	char c;

	if(iswin)
		read(0, &c, 1);
	return;
}
