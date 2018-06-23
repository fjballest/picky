program clases;



consts:
	numalumnos=3;
	numclases= 3 ;

types:
	TipoAlumno = record
	{
		dni: int;
		nota1: float;
		nota2: float;
		nota3:float;
	};
	
	TipoAlumnClase= int 0..numalumnos-1;
	TipoClase = array[TipoAlumnClase] of TipoAlumno;
	
consts:

	A=TipoAlumno(234, 3.4, 5.6, 4.3);
	B=TipoAlumno(4372, 5.4, 9.8, 5.7);
	C=TipoAlumno(762, 5.7, 9.7, 8.8);
	D=TipoAlumno(5672,10.0 , 2.8, 4.5);
	E=TipoAlumno(43872,8.5, 4.8, 5.0);
	F=TipoAlumno(67432,5.4,9.5, 6.7);
	G=TipoAlumno(4382,5.4,9.7, 4.5);
	H=TipoAlumno(345,8.4,2.8, 3.4);
	I=TipoAlumno(43562,7.4,9.5,6.5);
	clase1=TipoClase(A,B,C);
	clase2=TipoClase(D,E,F);
	clase3=TipoClase(G,H,I);

procedure lectura();

function notamedia(clase:TipoClase):float
	n:float;
	i:int;
	suma:float;
	media:float;
{

	n=0.0;
	i=0;suma=0.0;
	for (i=0, i<numalumnos-1){
		
		/*uma=clase[i].nota1+clase[i].nota2+clase[i].nota3;*/
		
		writeln(i);
		
		n=n+suma;
	}
	media= n/(3.0*3.0);
	return media;

}

procedure main()
{
	write(notamedia(clase1));
	/*mismaclase();
	todaclase();
	mayornota();*/

}