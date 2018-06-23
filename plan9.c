#include "syshdr.h"
#include "pam.h"
#include "pilib.h"

void
nofperr(void)
{
	/*
	 * disable FP exceptions due to builtin calls with
	 * bad user arguments.
	 */
	setfcr(getfcr() &~(FPINVAL|FPOVFL));
}

void
chmodx(char *n)
{
	Dir wd;
	
	nulldir(&wd);
	wd.mode = 0775;
	if(dirwstat(n, &wd) < 0)
		sysfatal("%s: wstat: %r", n);
}

static int
faulthndlr(void *, char *s)
{
	extern void faulted(char*);

	faulted(s);
	return 0;
}

void
nofaults(void)
{
	atnotify(faulthndlr, 1);
}

/*
 * Needed because windows does not have /fd/0 nor /dev/fd/0
 * (same for /fd/1)
 */
Biobuf*
Bfdopen(int f, int mode)
{
	Biobuf *bp;

	bp = malloc(sizeof(Biobuf));
	if(bp == 0)
		return 0;
	Binits(bp, f, mode, bp->b, sizeof(bp->b));
	bp->flag = Bmagic;
	return bp;
}

/* only waits in windows */
void
finishwait(void)
{
	return;
}
