edge('S','A',300).
edge('S','D',20).
edge('A','B',1500).
edge('A','D',400).
edge('B','C',9).
edge('B','E',200).
edge('C','D',2000).
edge('C','G',12).
edge('G','F',800).
edge('F','E',400).
edge('E','D',3).

connected(X,Y,L) :- edge(X,Y,L) ; edge(Y,X,L).

path(A,B,L,Path) :-
wrt
