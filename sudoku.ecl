:- use_module(library(ic)).
:- use_module(library(ic_global)).


sudoku(G) :- 
   dados(G),
   flatten(G,Gf),
   Gf #:: 1..9,
   restrs_linhas(G),
   transposta(G,Gt),
   restrs_linhas(Gt),
   blocos(G,Bs),
   restrs_linhas(Bs),
   labeling(Gf).


dados([[5,3,_,_,7,_,_,_,_],
       [6,_,_,1,9,5,_,_,_],
       [_,9,8,_,_,_,_,6,_],
       [8,_,_,_,6,_,_,_,3],
       [4,_,_,8,_,3,_,_,1],
       [7,_,_,_,2,_,_,_,6],
       [_,6,_,_,_,_,2,8,_],
       [_,_,_,4,1,9,_,_,5],
       [_,_,_,_,8,_,_,7,9]]).


restrs_linhas([]).
restrs_linhas([L|RLinhas]) :- ic_global:alldifferent(L),
    restrs_linhas(RLinhas).

blocos([],[]).
blocos([L1,L2,L3|RLinhas],Blocos) :- 
    blocos_(L1,L2,L3,Blocos,RBlocos),
    blocos(RLinhas,RBlocos).

blocos_([],[],[],RBlocosF,RBlocosF).
blocos_([A,B,C|RL1],[D,E,F|RL2],[G,H,I|RL3],[Bloco|RBlocos],RBlocosF) :- 
    Bloco = [A,B,C,D,E,F,G,H,I],
    blocos_(RL1,RL2,RL3,RBlocos,RBlocosF).

transposta([],[]).
transposta([X],Sxs) :- !, transposta_(X,Sxs).
transposta([X,Y|R],Tf) :- transposta([Y|R],T),
   transposta__(X,T,Tf).

transposta_([],[]).
transposta_([X|R],[[X]|Rs]) :- transposta_(R,Rs).

transposta__([],[],[]).
transposta__([X|R],[Tx|Rt],[[X|Tx]|RRt]) :- transposta__(R,Rt,RRt).

