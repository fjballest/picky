#include	"lib9.h"
#include	<bio.h>


Biobuf*
Bfdopen(int f, int mode)
{
	Biobuf *bp;

	_setmode(f, _O_BINARY);
	bp = malloc(sizeof(Biobuf));
	if(bp == 0)
		return 0;
	Binits(bp, f, mode, bp->b, sizeof(bp->b));
	bp->flag = Bmagic;
	return bp;
}
