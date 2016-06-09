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
% juntar_con(+L, +J, -R)
%
% La hicimos reversible para simplificar 
% la resolución del ejercicio 3.
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
% Utilizando la reversibilidad del anterior punto la 
% la implementacion de este punto es trivial.
% Es reversible.
%
palabras(S,P):- juntar_con(P, espacio, S).

%==============================================================
%
% EJERCICIO 4
% 
% asignar_var/3
% asignar_var(+A, +MI, -MF)
%
% ¿Por qué funciona? - Falta responder. 
%
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
% En esta función tuvimos que implementar 
% zip como auxilir para poder resolver 
% el problema.
%
%
palabras_con_variables([],[]).
palabras_con_variables([[A]|P],[[T]|V]):-
  palabras_con_variables(P,V),
  append(P,ConcatedP),
  append(V,ConcatedV),
  zip(ConcatedP,ConcatedV, ZippedPV),
  list_to_set(ZippedPV,WithoutRepeatPV),
  asignar_var(A,WithoutRepeatPV,MF),
  member((A,T),MF).

palabras_con_variables([[A|TailA]|P], [[T|TailT]|V]):-
  TailA \= [],
  palabras_con_variables([TailA|P],[TailT|V]),
  append([TailA|P],ConcatedP),
  append([TailT|V],ConcatedV),
  zip(ConcatedP,ConcatedV, ZippedPV),
  list_to_set(ZippedPV,WithoutRepeatPV),
  asignar_var(A,WithoutRepeatPV,MF),
  member((A,T),MF).

%
% zip
% zip/3
% zip(+X,+Y,-Z)
%
% Implentacion de la funciona clasica zip
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
% Dado E un atomo y L una lista de atomos, instancia en R 
% el resultado de quitar todas las apariciones de E en L. 
% L puede contener elemento instanciados y 
% no instanciados. E puede no estar instanciado.
%
quitar(_,[],[]).

quitar(E,[A|L],R):- var(E), var(A), E==A, quitar(E,L,R).
quitar(E,[A|L],[A|R]):- var(E), var(A), E\==A, quitar(E,L,R).

quitar(E,[A|L],[A|R]):- var(E), nonvar(A), quitar(E,L,R).
quitar(E,[A|L],[A|R]):- nonvar(E), var(A), quitar(E,L,R).

quitar(E,[A|L],[A|R]):- nonvar(E), nonvar(A), E\==A, quitar(E,L,R).
quitar(E,[A|L],R):- nonvar(E), nonvar(A), E==A, quitar(E,L,R).

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
  generar_soluciones(N,MCodes),
  ground(MCodes),
  setof(_,maplist(string_codes,MList,MCodes),_),
  implode(MList," ",M).

%
% generar_soluciones/2
%
generar_soluciones([],[]).
generar_soluciones([N|NS],[M|MCodes]):-
  diccionario_lista(L),
  match(L,N,M),
  generar_soluciones(NS,MCodes).

%
% match/3
%
match(L,N,M):-
  length(L, LengthL),
  length(N, LengthN),
  LengthL=:=LengthN,
  cambiar_var_por_valor(L,N),
  M=N.

%
% cambiar_var_por_valor/2
%
cambiar_var_por_valor([],[]).
cambiar_var_por_valor([L|LS], [A|AS]):-
  A=L,
  cambiar_var_por_valor(LS,AS).

%
% implode/3
%
implode([],_,"").
implode([A],_,A).
implode([A|XS],C,S):-
  XS \= [],
  implode(XS,C,R),
  string_concat(A,C,T),
  string_concat(T,R,S).

%==============================================================
%
% EJERCICIO 9
% 
% descifrar_sin_espacios/2
% descifrar_sin_espacios(S, M)
%
% Falta implementar.
%

%==============================================================
%
% EJERCICIO 10
% 
% mensajes_mas_parejos/2
% mensajes_mas_parejos(S, M)
%
% Falta implementar.
%

