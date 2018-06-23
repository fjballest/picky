#include "syshdr.h"
#include "p.h"
#include "pam.h"
#include "y.tab.h"

static char *itnames[256] = {
	[ITint]	"",
	[ITreal] "r",
	[ITaddr] "a",
};

static char *icnames[256] = {
	[ICadd]		"add",
	[ICadd|ITreal]	"addr",
	[ICand]		"and",
	[ICarg]		"arg",
	[ICcall]	"call",
	[ICcast]	"cast",
	[ICcast|ITreal]	"castr",
	[ICdaddr]	"daddr",
	[ICdata]	"data",
	[ICdata|ITreal]	"datar",
	[ICdiv]		"div",
	[ICdiv|ITreal]	"divr",
	[ICeq]		"eq",
	[ICeqm]		"eqm",
	[ICeq|ITaddr]	"eqa",
	[ICeq|ITreal]	"eqr",
	[ICfld]		"fld",
	[ICge]		"ge",
	[ICge|ITreal]	"ger",
	[ICgt]		"gt",
	[ICgt|ITreal]	"gtr",
	[ICidx]		"idx",
	[ICindir]	"ind",
	[ICjmp]		"jmp",
	[ICjmpf]	"jmpf",
	[ICjmpt]	"jmpt",
	[ICle]		"le",
	[ICle|ITreal]	"ler",
	[IClt]		"lt",
	[IClt|ITreal]	"ltr",
	[IClvar]	"lvar",
	[ICminus]	"minus",
	[ICminus|ITreal]	"minusr",
	[ICmod]		"mod",
	[ICmod|ITreal]	"modr",
	[ICmul]		"mul",
	[ICmul|ITreal]	"mulr",
	[ICne]		"ne",
	[ICnem]		"nem",
	[ICne|ITaddr]	"nea",
	[ICne|ITreal]	"ner",
	[ICnop]		"nop",
	[ICnot]		"not",
	[ICor]		"or",
	[ICpow]		"pow",
	[ICpow|ITreal]	"powr",
	[ICptr]		"ptr",
	[ICpush]	"push",
	[ICpush|ITreal]	"pushr",
	[ICret]		"ret",
	[ICsto]		"sto",
	[ICstom]	"stom",
	[ICsub]		"sub",
	[ICsub|ITreal]	"subr",
};

static struct
{
	int ic;
	char *name;
} nems[] = {
	{ICadd,		"add"},
	{ICadd|ITreal,	"addr"},
	{ICand,		"and"},
	{ICarg,		"arg"},
	{ICcall,	"call"},
	{ICcast,	"cast"},
	{ICcast|ITreal,	"castr"},
	{ICdaddr,	"daddr"},
	{ICdata,	"data"},
	{ICdata|ITreal,	"datar"},
	{ICdiv,		"div"},
	{ICdiv|ITreal,	"divr"},
	{ICeq,		"eq"},
	{ICeqm,		"eqm"},
	{ICeq|ITaddr,	"eqa"},
	{ICeq|ITreal,	"eqr"},
	{ICfld,		"fld"},
	{ICge,		"ge"},
	{ICge|ITreal,	"ger"},
	{ICgt,		"gt"},
	{ICgt|ITreal,	"gtr"},
	{ICidx,		"idx"},
	{ICindir,	"ind"},
	{ICjmp,		"jmp"},
	{ICjmpf,	"jmpf"},
	{ICjmpt,	"jmpt"},
	{ICle,		"le"},
	{ICle|ITreal,	"ler"},
	{IClt,		"lt"},
	{IClt|ITreal,	"ltr"},
	{IClvar,	"lvar"},
	{ICminus,	"minus"},
	{ICminus|ITreal,	"minusr"},
	{ICmod,		"mod"},
	{ICmod|ITreal,	"modr"},
	{ICmul,		"mul"},
	{ICmul|ITreal,	"mulr"},
	{ICne,		"ne"},
	{ICnem,		"nem"},
	{ICne|ITaddr,	"nea"},
	{ICne|ITreal,	"ner"},
	{ICnop,		"nop"},
	{ICnot,		"not"},
	{ICor,		"or"},
	{ICpow,		"pow"},
	{ICpow|ITreal,	"powr"},
	{ICptr,		"ptr"},
	{ICpush,	"push"},
	{ICpush|ITreal,	"pushr"},
	{ICret,		"ret"},
	{ICsto,		"sto"},
	{ICstom,	"stom"},
	{ICsub,		"sub"},
	{ICsub|ITreal,	"subr"},
};

int
hasarg(u32int ir)
{
	return IC(ir) >= ICargs;
}

int
Ifmt(Fmt *f)
{
	u32int ir, ic, it;

	ir = va_arg(f->args, u32int);
	ic = IC(ir);
	it = IT(ir);
	if(ic >= nelem(icnames) || icnames[ic] == nil)
		sysfatal("bad IC %d", ic);
	if(it >= nelem(itnames) || itnames[it] == nil)
		sysfatal("bad IT %#x", it);
	return fmtprint(f, "%s%s", icnames[IC(ir)], itnames[IT(ir)]);
}

ulong
icode(char *s)
{
	int i;

	for(i = 0; i < nelem(nems); i++)
		if(strcmp(nems[i].name, s) == 0)
			return nems[i].ic;
	fprint(2, "no instruction '%s'\n", s);
	return ~0;
}
