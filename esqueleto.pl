  :- dynamic(diccionario/1).

% Dado un nombre de archivo que contiene todas las palabras que se quieren
% agregar al diccionario (una por linea), vacia diccionario/1 y agrega
% las definiciones nuevas

cargar(NombreDeArchivo) :-
  retractall(diccionario(_)),
  atom_codes(NombreDeArchivo, Arch),
  open(Arch, read, Str),
  read_file(Str,_),
  close(Str).

read_file(Stream,[]) :- at_end_of_stream(Stream).
read_file(Stream,[X|L]) :- not(at_end_of_stream(Stream)),
    read_line_to_codes(Stream,Codes),
    string_codes(X, Codes),
    assertz(diccionario(X)),
    read_file(Stream,L), !.

%==============================================================
% listar mensajes secretos de ejemplo.
ej(1, [rombo, cuadrado, espacio, perro, cuadrado, sol, cuadrado]).
% solo deberia ser "la cosa" porque cuadrado != triangulo
ej(2, [rombo, cuadrado, espacio, perro, triangulo, sol, cuadrado]).
%
ej(3, [rombo, cuadrado, perro, cuadrado, sol, luna, triangulo, estrella, arbol, gato]).

%==============================================================
%
% EJERCICIO 1
%
% diccionario_lista/1 
% diccionario_lista(?L)
%
% Equivalente a diccionario/1, pero
% que trabaje con listas de codigos de 
% caracteres en lugar de strings.
%
diccionario_lista(Codes) :- diccionario(X), 
                            string_codes(X,Codes). 

%==============================================================
%
% EJERCICIO 2
% 
% juntar_con/3
% juntar_con(?L, +J, ?R)
%
% Si se instancia L y J, instancia en R lo pedido.
% Si se instancia J y R, instancia en L las posibles
% listas de listas que forman R si se usa J como limitador
% Por ahora no se tiene en cuenta el caso de la palabra
% 'espacio' que tiene un comportamiento especial en el tp
% ya que no debería ser un elemento de una de las sublistas de L.
% En este punto no nos interesa.
%
juntar_con([L|LS], J, R):- append(L, [J|L2], R), 
                           juntar_con(LS,J,L2).
juntar_con([LS], J, LS) :- not(append(_, [J|_], LS)).

%==============================================================
%
% EJERCICIO 3
% 
% palabras/2
% palabras(+S, -P)
%
% Dada S instanciada una lista de simbolos, instancia en P
% una lista de listas, donde cada lista es una palabra.  
% Donde cada palabra de S se identifica si es separada
% por el simbolo 'espacio'.
% En este punto usamos juntar_con, pero aqui tenemos
% que agregar la restricción para que el simbolo 'espacio'
% no quede dentro de una palabra.
%
palabras(S,P):- juntar_con(P, espacio, S),
                append(P,R),
                not(member(espacio, R)).
  
%==============================================================
%
% EJERCICIO 4
% 
% asignar_var/3
% asignar_var(+A, +MI, -MF)
%
% A debe ser un atomo y MI una lista de tuplas (atomo, var_fresca), ambas 
% deben estar instanciadas para que se instancie en MF un mapeo que contenga
% los mismos elementos que MI más una correspondencia para el símbolo A (si no 
% estaba presente en MI). 
%
% Esto funciona ya que asignar_var pone una var fresca para un atomo, y como 
% sabemos una ver fresca puede unificar con otra var fresca (o cualquier otra 
% cosa). Por lo tanto cuando se trata asignar un atomo que ya está en la 
% lista, la unificación se produce sin inconvenientes.
%
asignar_var(A, MI, [(A,_)|MI]):- not(member((A,_),MI)).
asignar_var(A, MI, MI):- member((A,_),MI).

%==============================================================
%
% EJERCICIO 5
% 
% palabras_con_variables/2
% palabras_con_variables(+P, -V)
%
% Dada P instanciada en una lista de listas de atomos, se instancia en V   
% una lista de listas de variables, donde a cada atomo le corresponde 
% una unica variable.
%
%
palabras_con_variables([],[]).
palabras_con_variables([[A]|P],[[T]|V]):- palabras_con_variables(P,V),
                                          asignar_misma_var(P,V,A,T).
palabras_con_variables([[A|TailA]|P], [[T|TailT]|V]):- TailA \= [],
                                          palabras_con_variables([TailA|P],[TailT|V]),
                                          asignar_misma_var([TailA|P],[TailT|V],A,T).
%
% asignar_misma_var/4
% asignar_misma_var(+P,+V,+A,-T)
%
% Sea P una lista instanciada de listas de atomos
% Sea V la lista instanciada de listas de var frescar
% relacionadas con los atomos de P, donde a cada atomo
% le corresponde una una variable.
% Sea A un atomo instanciado. Se instancia en T
% una var fresca para A, nueva si A no está en alguna
% sublista de P y en lo contrario la varfresca de V
% relacionada con ese atomo.
%
asignar_misma_var(P,V,A,T):-
  append(P,ConcatedP),
  append(V,ConcatedV),
  zip(ConcatedP,ConcatedV, ZippedPV),
  list_to_set(ZippedPV,WithoutRepeatPV),
  asignar_var(A,WithoutRepeatPV,MF),
  member((A,T),MF).

%
% zip/3
% zip(+L1,+L2,-L3)
%
% Dadas L1, y L2 dos listas instanciadas del mismo tamaño.
% instancia en L3 una lista de tuplas donde
% el primer elemento de la tupla pertenece a L1 
% y el segundo a L2, y ambos elementos están en 
% la misma posición de su lista.
% 
zip([],[],[]).
zip([X|XS], [Y|YS], [(X,Y)|Z]):- zip(XS,YS,Z).


%==============================================================
%
% EJERCICIO 6
% 
% quitar/3
% quitar(+E, +L, -R).
%
% Dado E un atomo instanciado y L una lista instanciada de atomos, 
% instancia en R el resultado de quitar todas las apariciones de E en L. 
% L puede contener elemento instanciados y 
% no instanciados. E puede no estar instanciado.
%
quitar(_,[],[]).
quitar(E,[A|L],R):- E==A, quitar(E,L,R).
quitar(E,[A|L],[A|R]):- E\==A, quitar(E,L,R).

%==============================================================
%
% EJERCICIO 7
% 
% cant_distintos/2
% cant_distintos(+L, -S)
%
% Dada L una lista de atomos y variables, instancie en 
% S la cantidad de elementos distintos que contiene L.
%
cant_distintos([],0).
cant_distintos([A|L],S):- quitar(A,L,R), 
                          cant_distintos(R,T), 
                          S is T+1.

%==============================================================
%
% EJERCICIO 8
% 
% descifrar/2
% descifrar(+S, -M)
%
% Dada S una lista de simbolos con un mensaje secreto,
% instancia en M los posibles mensajes descifrados 
% (uno por vez) utilizando las palabras del
% diccionario/1
%
descifrar(S,M):-
  palabras(S,P),
  palabras_con_variables(P,N),
  validar_palabra_en_dict(N),
  agregar_espacio_a_palabras(N,V),
  validar_solo_letras_por_simbolos(S,V),
  string_codes(M,V).

%
% validar_solo_letras_por_simbolo/2
%
% validar_solo_letras_por_simbolo(+N,+M) 
%
% Dada N instanciada, una lista de listas de atomos
% y M istanciada una lista de listas de var frescas
% es exito si cada letra se corresponde con una var
% fresca.
%
validar_solo_letras_por_simbolos([],[]).
validar_solo_letras_por_simbolos([X|XS],[Y|YS]):- validar_solo_letras_por_simbolo_aux(X,Y,XS,YS),
                                                  validar_solo_letras_por_simbolos(XS,YS).
%
% validar_solo_letras_por_simbolo_aux/4
% validar_solo_letras_por_simbolo_aux(+X,+Y,+XS,+YS)
% 
% Auxiliar para validar_solo_letras_por_simbolos
% Funcionamiento trivial.
%
validar_solo_letras_por_simbolo_aux(_,_,[],[]).
validar_solo_letras_por_simbolo_aux(X,Y,[X|ZS],[Y|WS]):- validar_solo_letras_por_simbolo_aux(X,Y,ZS,WS).
validar_solo_letras_por_simbolo_aux(X,Y,[Z|ZS],[W|WS]):- X\==Z,
                                                         Y\==W,
                                                         validar_solo_letras_por_simbolo_aux(X,Y,ZS,WS).

%
% agregar_espacio_a_palabras/2
% agregar_espacio_a_palabras(+N, -V)
%
% Dada N una lista de listas instanciada,
% instancia en V 'juntar_con' de N con el
% número 32 como caracter separador.
% El numero 32 es el codigo del atomo espacio. 
% 
agregar_espacio_a_palabras(N,V) :- juntar_con(N, 32, V).

%
% validar_palabra_en_dict/1
% validar_palabra_en_dict(+P).
%
% Dada P una lista de listas de codigos instanciada 
% Es exito si los strings asociados a esas listas 
% de cógidos pertenecen al diccionario cargado.
%
% 
validar_palabra_en_dict([]).
validar_palabra_en_dict([P|PS]):- diccionario_lista(P),
                                  validar_palabra_en_dict(PS).

%==============================================================
%
% EJERCICIO 9
% 
% descifrar_sin_espacios/2
% descifrar_sin_espacios(S, M)
%
% descifrar_sin_espacios(+S,-M)
%
% Dada en S una lista instanciada sin atomos
% 'espacio' instancie en M los posibles mensajes descifrados 
% utilizando las palabras del diccionario/1,
% intercalando los espacios necesarios para que el mensaje 
% use todos los símbolos.
%
descifrar_sin_espacios(S,M):- separar_con_espacios(S,E),
                              descifrar(E,M).

%
% separar_con_espacios/2
% separar_con_espacios(+S, -M)
%
% Dada en S una lista de atomos
% instancia en E esa lista de atomos
% con atomos 'espacio' entre sus elementos.
% Cada solución E es una de las posibles 
% combinaciones, SIN el atomo 'espacio'
% en la ultima posición.
%
separar_con_espacios([],[]).
separar_con_espacios(S,E):- agregar_espacios(S,E),
                            last(E,M),
                            M \== espacio.

%
% agregar_espacios/2
% agregar_espacios(+S,-E)
%
% Dada en S una lista de atomos
% instancia en E esa lista de atomos
% con atomos 'espacio' entre sus elementos.
% Cada solución E es una de las posibles 
% combinaciones.  
%
agregar_espacios([],[]).
agregar_espacios([S|SS],[S,espacio|EE]):- agregar_espacios(SS,EE).
agregar_espacios([S|SS],[S|EE]):- agregar_espacios(SS,EE).

%==============================================================
%
% EJERCICIO 10
% 
% mensajes_mas_parejos/2
% mensajes_mas_parejos(S, M)
%
% Falta implementar.
%
%
