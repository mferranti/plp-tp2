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
read_file(Stream,[X|L]) :-
    not(at_end_of_stream(Stream)),
    read_line_to_codes(Stream,Codes),
    string_codes(X, Codes),
    assertz(diccionario(X)),
    read_file(Stream,L), !.

% listar mensajes secretos de ejemplo.
ej(1, [rombo, cuadrado, espacio, perro, cuadrado, sol, cuadrado]).
% solo debería ser "la cosa" porque cuadrado != triangulo
ej(2, [rombo, cuadrado, espacio, perro, triangulo, sol, cuadrado]).

ej(3, [rombo, cuadrado, perro, cuadrado, sol, luna, triangulo, estrella, arbol, gato]).

ej(4, [rombo, espacio, perro]).

% Ejercicio 1

diccionario_lista(Codes) :- diccionario(X), string_codes(X,Codes). 

% Ejercicio 2
%juntar_con(+L, +J, -R).

juntar_con([L|LS], J, R):- append(L, [J|L2], R), juntar_con(LS,J,L2).
juntar_con([LS], J, LS) :- not(append(_, [J|_], LS)).

% Ejercicio 3

%palabras(S, P).
palabras(S,P):- juntar_con(P, espacio, S).

% Ejercicio 4
% asignar_var(A, MI, MF)

asignar_var(A, MI, [(A,_)|MI]):- not(member((A,_),MI)).
asignar_var(A, MI, MI):- member((A,_),MI).


% Ejercicio 5

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

zip([],[],[]).
zip([X|XS], [Y|YS], [(X,Y)|Z]):- zip(XS,YS,Z).

