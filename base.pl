
% predicado para abrir un archivo -------------------------------------------------------------------------
abrir(KB):-
	open('/Users/juan/Desktop/Proyecto Representacion del conocimiento/KnowledgeBase/KB.txt',read,Stream),
	readclauses(Stream,X),
	close(Stream),
	atom_to_term_conversion(X,KB).

% --------------------------------------------------------------------------------------------------------


% predicado para guardar un archivo -----------------------------------------------------------------------
guardar(KB):-
	open('/Users/juan/Desktop/Proyecto Representacion del conocimiento/KnowledgeBase/KB.txt',write,Stream),
	writeq(Stream,KB),
	close(Stream).

% --------------------------------------------------------------------------------------------------------



% predicados auxiliares para el manejo de archivos -------------------------------------------------------

readclauses(InStream,W) :-
        get0(InStream,Char),
        checkCharAndReadRest(Char,Chars,InStream),
	atom_chars(W,Chars). 

checkCharAndReadRest(-1,[],_) :- !.  % End of Stream	
checkCharAndReadRest(end_of_file,[],_) :- !.

checkCharAndReadRest(Char,[Char|Chars],InStream) :-
        get0(InStream,NextChar),
        checkCharAndReadRest(NextChar,Chars,InStream).

atom_to_term_conversion(ATOM, TERM) :-
	 atom(ATOM),
	 atom_to_chars(ATOM,STR),
	 atom_to_chars('.',PTO),
	 append(STR,PTO,STR_PTO),
	 read_from_chars(STR_PTO,TERM).

% --------------------------------------------------------------------------------------------------------


% predicado para agregar una nueva clase a la base de conocimiento ----------------------------------------

clase_nueva(NombreClase, Padre, KB, KBnueva):-
append(KB,[class(NombreClase,Padre,[],[],[])],KBnueva).

% ---------------------------------------------------------------------------------------------------------



% predicado para cambiar el nombre de una clase --------------------------------------------------------------

cambiar_nombre_clase(Clase,NombreNuevo,KB,KBNueva):-
	cambiarElemento(class(Clase,Mother,Props,Rels,Objects),class(NombreNuevo,Mother,Props,Rels,Objects),KB,TemporalKB),
	cambiarPadre(Clase,NombreNuevo,TemporalKB,TemporalKB2),
	cambiar_relaciones_con_objetos(Clase,NombreNuevo,TemporalKB2,NewKB).

% --------------------------------------------------------------------------------------------------------

% predicados para encontrar los elementos hijos de una clase ---------------------------------------------

hijos_clase(Clase, KB, Resultado):-
es_clase(Clase,KB,si), %aqui se valida que sea una clase
	hijos_de_clase(Clase,KB,Resultado).
hijos_clase(_,_,unknown).

hijos_de_clase(_,[],[]).
hijos_de_clase(Class,[class(Son,Class,_,_,_)|T],Sons):-
	hijos_de_clase(Class,T,Brothers),	
	append([Son],Brothers,Sons).
hijos_de_clase(Class,[_|T],Sons):-
	hijos_de_clase(Class,T,Sons).	

% --------------------------------------------------------------------------------------------------------


% predicados auxiliares en las validaciones -------------------------------------------------------------

es_clase(_,[],desconocido).
es_clase(Clase,[class(not(Clase),_,_,_,_)|_],no).
es_clase(Clase,[class(Clase,_,_,_,_)|_],si).
es_clase(Clase,[_|T],Resultado):-
	es_clase(Clase,T,Resultado).

% --------------------------------------------------------------------------------------------------------


% predicados auxiliares en el manejo de listas -----------------------------------------------------------

cambiarElemento(_,_,[],[]).
cambiarElemento(X,Y,[X|T],[Y|N]):-
	cambiarElemento(X,Y,T,N).
cambiarElemento(X,Y,[H|T],[H|N]):-
	cambiarElemento(X,Y,T,N).


cambiarPadre(_,_,[],[]).
cambiarPadre(AntiguoPadre,PadreNuevo,[class(C,AntiguoPadre,P,R,O)|T],[class(C,PadreNuevo,P,R,O)|N]):-
	cambiarPadre(AntiguoPadre,PadreNuevo,T,N).
cambiarPadre(AntiguoPadre,PadreNuevo,[H|T],[H|N]):-
	cambiarPadre(AntiguoPadre,PadreNuevo,T,N).

% --------------------------------------------------------------------------------------------------------

cambiar_relaciones_con_objetos(_,_,[],[]).

cambiar_relaciones_con_objetos(Objeto,NombreNuevo,[class(C,M,P,R,O)|T],[class(C,M,P,NewR,NewO)|NewT]):-
	cambiar_relaciones(Objeto,NombreNuevo,O,NewO),
	cambiar_relacion(Objeto,NombreNuevo,R,NewR),
	cambiar_relaciones_con_objetos(Objeto,NombreNuevo,T,NewT).

cambiar_relaciones(_,_,[],[]).

cambiar_relaciones(Object,NewName,[[id=>N,P,R]|T],[[id=>N,P,NewR]|NewT]):-
	cambiar_relacion(Object,NewName,R,NewR),
	cambiar_relaciones(Object,NewName,T,NewT).

cambiar_relacion(_,_,[],[]).

cambiar_relacion(OldName,NewName,[R=>OldName|T],[R=>NewName|NewT]):-
	cambiar_relacion(OldName,NewName,T,NewT).

cambiar_relacion(OldName,NewName,[not(R=>OldName)|T],[not(R=>NewName)|NewT]):-
	cambiar_relacion(OldName,NewName,T,NewT).

cambiar_relacion(OldName,NewName,[H|T],[H|NewT]):-
	cambiar_relacion(OldName,NewName,T,NewT).
