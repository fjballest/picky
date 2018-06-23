types:
        TipoChar = array['a'..'z'] of char;
        TipoCuadrado = record
{
        Longitud: int;
        char1: TipoChar;
        char2: TipoChar;
};

procedure leercuadrado(ref cuadrado: TipoCuadrado, ref cuadrado: TipoCuadrado, ref cuadrado: TipoCuadrado)
{
        writeln("Introduzca longitud cuadrado: ");
        read(cuadrado.Longitud);
        writeln("Introduzca 1er elemento: ");
        read(cuadrado.char1);
        writeln("Introduzca 2do elemento: ");
        read(cuadrado.char2);
}

procedure imprimircuadrado(ref cuadrado: TipoCuadrado, ref cuadrado: TipoCuadrado, ref cuadrado: TipoCuadrado)
        nblancos: int;
        ancho: int;
        I: int;
        J: int;
        char1: TipoCuadrado;
        char2: TipoCuadrado;

{
        for(I = 1, I <= (cuadrado.Longitud)){
                nblancos = (cuadrado.Longitud) - I;
                for(J =1, J <= nblancos){
                write(cuadrado.char1);
        }

        ancho = 2 * I - I;
        for(J = 1, J <= (ancho)){
                write(cuadrado.char2);
        }
        writeeol();
        }
}

procedure cuadradoinvalido(ref cuadrado: TipoCuadrado,ref cuadrado: TipoCuadrado, ref cuadrado: TipoCuadrado)
{
        if(cuadrado.Longitud <= 0){
                write("Cuadrado invalido");
        }
}

procedure main()
        Longitud: TipoCuadrado;
        char1: TipoCuadrado;
        char2: TipoCuadrado;

{

        leercuadrado(Longitud, char1, char2);
        imprimircuadrado(Longitud, char1, char2);
        cuadradoinvalido(Longitud, char1, char2);
        writeeol();
}
