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
edge('E','D',300).


connected(X,Y,L) :- edge(X,Y,L) ; edge(Y,X,L).

travel(Node1,Node2,Len,Path,[Node2|Path]) :-
    connected(Node1,Node2,Len).
travel(StartNode,EndNode,Len,Visited,Path) :-
    connected(StartNode,AnotherNode,NodeDist),
    AnotherNode \==EndNode,
    \+member(AnotherNode,Visited),
    travel(AnotherNode,EndNode,SubPathLength,[AnotherNode|Visited],Path),
    Len is NodeDist+SubPathLength.


path(StartNode,EndNode,Length,Path) :-
    travel(StartNode,EndNode,Length,[StartNode],Q),
    reverse(Q,Path).
path(StartNode,WayPoint,EndNode,Length,Path) :-
    travel(StartNode,EndNode,Length,[StartNode],Q),
    member(WayPoint,Q),
    reverse(Q,Path).
path(StartNode,WayPoint1,WayPoint2,EndNode,Length,Path) :-
    travel(StartNode,EndNode,Length,[StartNode],Q),
    member(WayPoint1,Q),
    member(WayPoint2,Q),
    reverse(Q,Path).
path(StartNode,WayPoint1,WayPoint2,WayPoint3,EndNode,Length,Path) :-
    travel(StartNode,EndNode,Length,[StartNode],Q),
    member(WayPoint1,Q),
    member(WayPoint2,Q),
    member(WayPoint3,Q),
    reverse(Q,Path).
pathAll(StartNode,EndNode,Length,Path) :-
    travel(StartNode,EndNode,Length,[StartNode],Q),
    member('A',Q),
    member('B',Q),
    member('C',Q),
    member('D',Q),
    member('E',Q),
    member('F',Q),
    member('G',Q),
    reverse(Q,Path).


shortestPath(StartNode,EndNode,Length,Path):-
    setof([Path,Length], path(StartNode,EndNode,Length, Path), Set),
    Set = [_|_],
    shortest(Set,[Path,Length]).
shortestPath(StartNode, WayPoint, EndNode, Path, Length):-
    setof([Path,Length], path(StartNode,WayPoint,EndNode,Length, Path), Set),
    Set = [_|_],
    shortest(Set,[Path,Length]).
shortestPath(StartNode, WayPoint1,WayPoint2, EndNode, Path, Length):-
    setof([Path,Length], path(StartNode,WayPoint1,WayPoint2,EndNode,Length, Path), Set),
    Set = [_|_],
    shortest(Set,[Path,Length]).
shortestPath(StartNode, WayPoint1,WayPoint2,WayPoint3,EndNode, Path, Length):-
    setof([Path,Length], path(StartNode,WayPoint1,WayPoint2,WayPoint3,EndNode,Length, Path), Set),
    Set = [_|_],
    shortest(Set,[Path,Length]).

shortestPathAll(StartNode,Length,Path):-
    setof([Path,Length], pathAll(StartNode,StartNode,Length, Path), Set),
    Set = [_|_],
    shortest(Set,[Path,Length]).



shortestPathTwoWay(StartNode, WayPoint, Path, Length):-
    shortestPath(StartNode,WayPoint,StartNode,Path, Length).




%possibly redundant layer. May be possible to call min directly
shortest([First|Rest],Min):-
    min(First,Rest,Min).

min(Min,[],Min).
min([_,M],[[P,L]|R],Min) :-
    L < M,
    !,
    min([P,L],R,Min).

min(M,[_|R],Min) :-
    min(M,R,Min).
