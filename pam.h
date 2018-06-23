

enum
{
	/* picky abstract machine: PAM:
	 *
	 * Instructions are: ic|it
	 * They correspond to a stack machine that takes
	 * operands from the stack (-sp) and leaves
	 * results on the stack (+sp).
	 * Some instructions carry an additional parameter
	 * that takes the following instruction slot.
	 * data addresses given to addr, arg, and lvar take one slot
	 * in the text but are stored as u64ints in both data and stack.
	 *
	 * bools, chars, and ints are u32ints.
	 * reals are float (same size of u32ints).
	 *
	 * Those flagged |r have a real variant with type ITreal.
	 * Those flagged |a have a variant with type ITaddr
	 *
	 * Strings are always handed through references.
	 *
	 * The activation frame is:
	 *		<- sp
	 *	temp
	 *	temp	<- fp
	 *	savedpc		fp[-1]
	 *	savedpid		fp[-2]		(procid for caller)
	 *	savedfp		fp[-3,-4]
	 *	savedvp		fp[-5,-6]
	 *	savedap		fp[-7,-8]
	 *	lvar
	 *	lvar	<- vp
	 *	arg
	 *	arg
	 *	arg	<- ap
	 *	XXX
	 *	...
	 *		<- savedvp
	 *	...
	 *		<- savedfp
	 *
	 */

	/* instruction code (ic) */
	ICnop = 0,	/* nop */
	ICle,		/* le|r -sp -sp +sp */
	ICge,		/* ge|r -sp -sp +sp */
	ICpow,		/* pow|r -sp -sp +sp */
	IClt,		/* lt|r -sp -sp +sp */
	ICgt,		/* gt|r -sp -sp +sp */
	ICmul,		/* mul|r -sp -sp +sp */
	ICdiv,		/* div|r -sp -sp +sp */
	ICmod,		/* mod|r -sp -sp +sp */
	ICadd,		/* add|r -sp -sp +sp */
	ICsub,		/* sub|r -sp -sp +sp */
	ICminus,		/* minus|r -sp +sp */
	ICnot,		/* not -sp +sp */
	ICor,		/* or -sp -sp +sp */
	ICand,		/* and -sp -sp +sp */
	ICeq,		/* eq|r|a -sp -sp +sp */
	ICne,		/* ne|r|a -sp -sp +sp */
	ICptr,		/* ptr -sp +sp */
			/* obtain address for ptr in stack */

	ICargs,		/* those after have an argument */

	ICpush=ICargs,	/* push|r n +sp */
			/* push n in the stack */
	ICindir,		/* indir|a  n -sp +sp */
			/* replace address with referenced bytes */
	ICjmp,		/* jmp addr */
	ICjmpt,		/* jmpt addr */
	ICjmpf,		/* jmpf addr */
	ICidx,		/* idx tid  -sp -sp +sp */
			/* replace address[index] with elem. addr. */
	ICfld,		/* fld n -sp +sp */
			/* replace obj addr with field (at n) addr. */
	ICdaddr,	/* daddr n +sp */
			/* push address for data at n */
	ICdata,		/* data n +sp */
			/* push n bytes of data following instruction */
	ICeqm,		/* eqm n -sp -sp +sp */
			/* compare data pointed to by addresses */
	ICnem,		/* nem n -sp -sp +sp */
			/* compare data pointed to by addresses */
	ICcall,		/* call pid */
	ICret,		/* ret pid */
	ICarg,		/* arg n +sp */
			/* push address for arg object at n */
	IClvar,		/* lvar n +sp*/
			/* push address for lvar object at n */
	ICstom,		/* stom tid -sp -sp */
			/* cp tid's sz bytes from address to address */
	ICsto,		/* sto tid -sp -sp */
			/* cp tid's sz bytes to address from stack */
	ICcast,		/* cast|r tid -sp +sp */
			/* convert int (or real |r) to type tid */

	/* instr. type (it) */
	ITint  = 0,
	ITaddr = 0x40,
	ITreal = 0x80,
	ITmask = ITreal|ITaddr,

	/* Builtin addresses */
	PAMbuiltin = 0x80000000,

	/* builtin numbers (must be |PAMbuiltin) */
	PBacos = 0,
	PBasin,
	PBatan,
	PBclose,
	PBcos,
	PBdispose,	/* 0x5 */
	PBexp,
	PBfatal,
	PBfeof,
	PBfeol,
	PBfpeek,	/* 0xa */
	PBfread,
	PBfreadeol,
	PBfreadln,
	PBfrewind,
	PBfwrite,	/* 0xf */
	PBfwriteln,
	PBfwriteeol,
	PBlog,
	PBlog10,
	PBnew,		/* 0x14 */
	PBopen,	
	PBpow,
	PBpred,
	PBsin,
	PBsqrt,		/* 0x19 */
	PBsucc,
	PBtan,
	PBstack,
	PBdata,
	PBfflush,
	Nbuiltins,
};

#pragma varargck type "I" u32int	/* instruction codes */

#define IC(c)	((c)&0x3F)
#define IT(c)	((c)&ITmask)

#define MKI(c,t)	((c)|(t))

/* |c/f2p pam.c		*/
extern int	Ifmt(Fmt *f);
extern int	hasarg(u32int ir);
extern ulong	icode(char *s);
