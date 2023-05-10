:- module(_,_,[classic,assertions,regtypes]).
author_data('Hernando', 'Padilla', 'Miguel', 'c200113').

board1([cell(pos(1,1) ,op(*,-3)),
         cell(pos(1,2) ,op(- ,1)),
         cell(pos(1,3) ,op(- ,4)),
         cell(pos(1,4) ,op(- ,555)),
         cell(pos(2,1) ,op(- ,3)),
         cell(pos(2,2) ,op(+ ,2000)),
         cell(pos(2,3) ,op(* ,133)),
         cell(pos(2,4) ,op(- ,444)),
         cell(pos(3,1) ,op(* ,0)),
         cell(pos(3,2) ,op(* ,155)),
         cell(pos(3,3) ,op(// ,2)),
         cell(pos(3,4) ,op(+ ,20)),
         cell(pos(4,1) ,op(- ,2)),
         cell(pos(4,2) ,op(- ,1000)),
         cell(pos(4,3) ,op(- ,9)),
         cell(pos(4,4) ,op(* ,4))]).

DireccionesPermitidas =[dir(n,3), dir(s,4), dir(o,2), dir(se,10)].
:- dynamic direccionesPermitidas/2.


%:- pred init
%   #"Inicializa las variables dinámicas.".

%init :-
%    dir(X,Y),
%    assert(direccionesPermitidas(X,Y)),
%    fail.

%init :-
%    fail.

:-pred efectuar_movimiento(Pos,Dir,Pos2)
  #"".

efectuar_movimiento(pos(X1,Y1), n, pos(X2,Y1)):-
    X2 is X1-1.

efectuar_movimiento(pos(X1,Y1), s, pos(X2,Y1)):-
    X2 is X1+1.

efectuar_movimiento(pos(X1,Y1), e, pos(X1,Y2)):-
    Y2 is Y1-1.

efectuar_movimiento(pos(X1,Y1), o,pos(X1,Y2)):-
    Y2 is Y1+1.


efectuar_movimiento(pos(X1,Y1), no, pos(X2,Y2)):-
    efectuar_movimiento(pos(X1,Y1), n, pos(X2,Y2)),
    efectuar_movimiento(pos(X1,Y1), o,pos(X2,Y2)).

efectuar_movimiento(pos(X1,Y1), ne, pos(X2,Y2)):-
    efectuar_movimiento(pos(X1,Y1), n, pos(X2,Y2)),
    efectuar_movimiento(pos(X1,Y1), e,pos(X2,Y2)).
    
efectuar_movimiento(pos(X1,Y1), so, pos(X2,Y2)):-
    efectuar_movimiento(pos(X1,Y1), n, pos(X2,Y2)),
    efectuar_movimiento(pos(X1,Y1), o,pos(X2,Y2)).

efectuar_movimiento(pos(X1,Y1), se, pos(X2,Y2)):-
    efectuar_movimiento(pos(X1,Y1), s, pos(X2,Y2)),
    efectuar_movimiento(pos(X1,Y1), e,pos(X2,Y2)).




    