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


board2([cell(pos(1,1) ,op(*,-3)),
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
  #"Recibe en @var{Dir} una dirección, que puede ser norte(n), sur, 
    esto(e), oeste(o) y las combinaciones de estas cuatro direcciones.
    El predicado se encarga de comprobar que si @var{Pos} se mueve en 
    la dirección marcada en @var{Dir}, se mueve a la casilla @var{Pos2}.".

efectuar_movimiento(pos(X1,Y1), n, pos(X2,Y1)):-
    X2 is X1-1.

efectuar_movimiento(pos(X1,Y1), s, pos(X2,Y1)):-
    X2 is X1+1.

efectuar_movimiento(pos(X1,Y1), o, pos(X1,Y2)):-
    Y2 is Y1-1.

efectuar_movimiento(pos(X1,Y1), e,pos(X1,Y2)):-
    Y2 is Y1+1.


efectuar_movimiento(pos(X1,Y1), no, pos(X2,Y2)):-
    efectuar_movimiento(pos(X1,Y1), n, pos(X2,_)),
    efectuar_movimiento(pos(X1,Y1), o,pos(_,Y2)).

efectuar_movimiento(pos(X1,Y1), ne, pos(X2,Y2)):-
    efectuar_movimiento(pos(X1,Y1), n, pos(X2,_)),
    efectuar_movimiento(pos(X1,Y1), e,pos(_,Y2)).
    
efectuar_movimiento(pos(X1,Y1), so, pos(X2,Y2)):-
    efectuar_movimiento(pos(X1,Y1), s, pos(X2,_)),
    efectuar_movimiento(pos(X1,Y1), o,pos(_,Y2)).

efectuar_movimiento(pos(X1,Y1), se, pos(X2,Y2)):-
    efectuar_movimiento(pos(X1,Y1), s, pos(X2,_)),
    efectuar_movimiento(pos(X1,Y1), e,pos(_,Y2)).

%para comprobar movimientos, hacer una lista que recorra de derecha a izquierda la lista de posiciones y comprueba 

:- pred movimiento_valido(N,Pos,Dir)
   #"Comprueba que en un tablero de tamaño @var{N}x@var{N}, desde la posicion indicada
     en la variable @var{Pos}, se pueda mover en la dirección indicada en @var{Dir}.".

movimiento_valido(N,pos(X,Y),n):-
    X > 1,
    Y =< N.

movimiento_valido(N,pos(X,Y),s):-
    X < N,
    Y =< N.

movimiento_valido(N,pos(X,Y),o):-
    Y > 1,
    X =< N.

movimiento_valido(N,pos(X,Y),e):-
    Y < N,
    X =< N.

%:- test movimiento_valido(N,P,D) : (N = 6, P = pos(2,6), D = n) + not_fails #"Caso 1 efectuar movimientos". %?,


:- pred aplicar_op(Op, Valor, Valor2)
   #"En @var{Op} se recibe un valor y un operador. En @var{Valor2} esta el resultado de la operación del primer valor de @var{Op} y @var{Valor}, usando el operador recibido en el segundo argumento de @var{Op}.".%Preguntar por el foro sobre si se puede poner op(A,B) en el predicado

aplicar_op(op(+, Op1),Op2, Resultado) :-
    Resultado is Op1 + Op2.

aplicar_op(op(-, Op1),Op2, Resultado) :-
    Resultado is Op1 - Op2.

aplicar_op(op(*,Op1),Op2, Resultado) :-
    Resultado is Op1 * Op2.

aplicar_op(op(//, Op1),Op2, Resultado) :-
    Resultado is Op1 //  Op2.

% select cell no esta aún

:- pred select_cell(IPos,Op,Board,NewBoard)
  #"".

%select_cell(IPos, Op, [cell(IPos,Op)|Board], NewBoard):-
%    select_cell(IPos, Op, Board, NewBoard).
select_cell(IPos, Op, [cell(IPos,Op)|Board], Board).

select_cell(IPos, Op, [cell(Pos,Ope)|Board], [cell(Pos,Ope)|NewBoard]) :-
    IPos \= Pos,
    select_cell(IPos, Op, Board, NewBoard).
    
%select_cell(_,_,[],[]).

%select_cell(pos(1,2),op(- ,1), [cell(pos(1,1) ,op(*,-3)),cell(pos(1,2),op(*,-3)), cell(pos(1,3) ,op(- ,4))],[cell(pos(1,1) ,op(- ,1)), cell(pos(1,3) ,op(- ,4))]).
%select_cell(pos(1,2),op(- ,1), [cell(pos(1,1) ,op(*,-3)),cell(pos(1,2),op(-,1)), cell(pos(1,3) ,op(- ,4))],[cell(pos(1,1) ,op(* ,-3)), cell(pos(1,3) ,op(- ,4))]).

%[cell(pos(1,1) ,op(*,-3)),cell(pos(1,2) ,op(- ,1)), cell(pos(1,3) ,op(- ,4))]

:- pred select_dir(Dir,Dirs,NewDirs)
   #"En @var{Dir} hay una dirección, puede ser n, s, o, e, además de las comninaciones entre estas. En 
     @var{Dirs} es donde hay una lista de direcciones permitidas, que es una lista de estructuras dir(A,B),
     con A siendo una dirección y B el numero de veces que se puede ir en esa dirección.
     En @var{NewDirs} esta la misma lista, pero para @var{Dir} un valor menos en el número de movimientos 
     que permiten realizar, o no aparecer si solo podía realizar un movimiento.".

%select_dir(_,[],[]).

%select_dir(Dir, [dir(Dir,X)|Dirs],[dir(Dir,Y)|NewDirs]):-
select_dir(Dir, [dir(Dir,X)|Dirs],[dir(Dir,Y)|Dirs]):-
    X > 1,
    Y is X - 1.

select_dir(Dir, [dir(Dir,1)|Dirs],Dirs).

select_dir(Dir, [N|Dirs],[N|NewDirs]):-
    N =.. [dir,A,B],
    Dir \= A,
    select_dir(Dir,Dirs,NewDirs).
    
%select_dir(n,[dir(n,3), dir(s,4), dir(o,2), dir(se,10)],[dir(n,2), dir(s,4), dir(o,2), dir(se,10)]).

%select_dir(n,[dir(n,3), dir(s,4), dir(o,2), dir(se,10)],A).&

%:- pred generar_recorrido(Ipos,N,Board,[A|DireccionesPermitidas,Recorrido,Valor)


%primer paso valor empiece a 0
%generar_recorrido(Ipos,N,Board,DireccionesPermitidas,Recorrido,Valor):-
%    generar_recorrido_aux(Ipos,N,Board,DireccionesPermitidas,Recorrido,0).

%generar_recorrido_aux(Ipos,N,Board,DireccionesPermitidas,[(Ipos,Valor)|Recorrido],Valor):-
%    movimiento_valido(N,Ipos,Dir), %encuentras una dirección posible a la que ir
%    select_dir(Dir, DireccionesPermitidas, NewDireccionesPermitidas), %se elimina la dirección escogida de la lista de direcciones permitidas
%    select_cell(Ipos,Op,Board,NewBoard), %Obtienes la operación asociada a la celda y el tablero sin esa celda
%    aplicar_op(Op,Valor,Nuevo_Valor), %Realizas la operación de la celda
%    efectuar_movimiento(Ipos,Dir, NewPos), %Te mueves a la posición que corresponda
%    select_cell(NewPos,Op,NewBoard,A), %compruebas la posicion de la posicion a la que te mueves
    %me falta el recorrido
%    generar_recorrido_aux(NewPos,N,NewBoard,NewDireccionesPermitidas,Recorrido,Nuevo_Valor).%me falta el recorrido

%generar_recorrido_aux(_,_,[],_,_,_).
%generar_recorrido_aux(_,_,_,[],_,_).
%generar_recorrido_aux(_,_,_,_,[],_).



%generar_recorrido(pos(1,1),2,[cell(pos(1,1) ,op(*,-3)),cell(pos(1,2) ,op(- ,1)),cell(pos(2,1) ,op(- ,3)),cell(pos(2,2) ,op(+ ,2000))],[dir(n,3), dir(s,4), dir(o,2), dir(se,10)],A,V).
    
%  generar_recorrido(pos(1,1),3,[cell(pos(1,1) ,op(*,-3)),cell(pos(1,2) ,op(- ,1)),cell(pos(1,3) ,op(- ,4)),cell(pos(2,1) ,op(- ,3)),cell(pos(2,2) ,op(+ ,2000)),cell(pos(2,3) ,op(* ,133)),cell(pos(3,1) ,op(* ,0)),cell(pos(3,2) ,op(* ,155)),cell(pos(3,3) ,op(// ,2))],[dir(n,3), dir(s,4), dir(o,2), dir(se,10)],A,V).  