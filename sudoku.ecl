:- use_module(library(ic)).
:- use_module(library(ic_global)).


sudoku(G, Bs, Sf) :-
	  samurai(G),
   	  flatten(G,Gf),
   	  Gf #:: 1..9,
   	  restrs_linhas(G),
   	  transposta(G,Gt),
   	  restrs_linhas(Gt),
  	  blocos(G,Bs),
   	  restrs_linhas(Bs).
  	  %labeling(Gf).  Foi preciso tirar para o samurai


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



samurai(S) :-
	   dados_samurai(S), restrs_sudoku(S, BS5, RestBlocos, [], LVarsF), selec([9,7,3,1], RestBlocos, LPartilhada), selec2([1,3,7,9], BS5, LPartilhada), labeling(LVarsF).

restrs_sudoku([S5],BS5,[], LVars, LVarsF) :-
			   !, sudoku(S5,BS5, S5F), append(S5F, LVars, LVarsF). % !, - cut e faz com que nao entre nos proximos predicados
restrs_sudoku([S|RestSs],BS5,[BS|RBlocos), LVars, LVarsF) :-
					   sudoku(S,BS, Sf), restrs_sudoku(RestSs,BS5,RBlocos), append(Sf, LVars, LVarsF).

selec2([],_,[]).
selec2([I|R], BS5, [Bi|Rf]) :- seleciona(I,BS5,Bi), selec2(R,BS5,Rf).

selec([],[],[]).
selec([I|Ris],[B|Rblocos],[Bi|ResBs]) :- selec2(I,B,Bi), selec(Ris, Rblocos, ResBS).


dados_samurai([[[_,3,_,7,_,9,_,2,_],[_,_,8,_,6,_,7,_,_],[6,7,_,_,_,_,_,5,1],[_,_,6,8,_,5,3,_,_],[3,_,_,_,9,_,_,_,5],[_,_,4,2,_,6,1,_,_],[8,6,_,_,_,_,_,7,_],[_,_,7,_,4,_,_,_,_],[_,5,_,9,_,3,_,6,_]],
               [[_,3,_,4,_,6,_,8,_],[_,_,6,_,8,_,3,_,_],[9,7,_,_,_,_,_,5,6],[_,_,3,1,_,2,9,_,_],[2,_,_,_,3,_,_,_,8],[_,_,5,8,_,4,2,_,_],[_,2,_,_,_,_,_,9,7],[_,_,_,_,1,_,5,_,_],[_,5,_,2,_,7,_,1,_]],
               [[_,6,_,7,_,9,_,3,_],[_,_,1,_,4,_,_,_,_],[3,4,_,_,_,_,_,9,_],[_,_,5,6,_,2,9,_,_],[7,_,_,_,5,_,_,_,4],[_,_,8,4,_,3,5,_,_],[6,5,_,_,_,_,_,8,9],[_,_,3,_,9,_,6,_,_],[_,8,_,5,_,4,_,7,_]],
               [[_,9,_,7,_,4,_,2,_],[_,_,_,_,2,_,6,_,_],[_,7,_,_,_,_,_,1,9],[_,_,2,8,_,7,9,_,_],[3,_,_,_,9,_,_,_,7],[_,_,9,6,_,1,2,_,_],[4,2,_,_,_,_,_,9,6],[_,_,6,_,4,_,7,_,_],[_,5,_,2,_,6,_,4,_]],
               [[_,7,_,6,_,3,_,2,_],[_,_,_,_,8,_,_,_,_],[_,6,_,_,_,_,_,5,_],[_,_,5,8,_,2,7,_,_],[3,_,_,_,1,_,_,_,2],[_,_,9,3,_,7,5,_,_],[_,3,_,_,_,_,_,9,_],[_,_,_,_,6,_,_,_,_],[_,9,_,1,_,4,_,7,_]]]).
