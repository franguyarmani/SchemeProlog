vertex(us,ca).
vertex(us,nv).
vertex(us,or).
vertex(us,ut).
vertex(us,az).
vertex(us,pa).
vertex(us,id).
vertex(us,wa).
vertex(us,nm).


edge_broken_sym(us,ca,nv).
edge_broken_sym(us,ca,or).
edge_broken_sym(us,ca,az).
edge_broken_sym(us,nv,or).
edge_broken_sym(us,nv,ut).
edge_broken_sym(us,nv,id).
edge_broken_sym(us,nv,az).
edge_broken_sym(us,or,id).
edge_broken_sym(us,id,ut).
edge_broken_sym(us,ut,az).
edge_broken_sym(us,or,wa).
edge_broken_sym(us,id,wa).
edge_broken_sym(us,az,nm).


edge(us,X,Y) :- edge_broken_sym(us,X,Y).
edge(us,X,Y) :- edge_broken_sym(us,Y,X).



newNeighbor(Vertex,Neighbor,List):-
    vertex(us, Neighbor),
    edge(us,Vertex,Neighbor),
    \+member(Neighbor, List).


coloring([Vertex-Color],Visited):-
    color(Color),
    \+newNeighbor(Vertex,_,Visited).

coloring([Vertex-Color,Neighbor-NeighborColor|Rest],Visited):-
    color(Color),
    edge(us,Vertex,Neighbor),
    vertex(us,Neighbor),
    Vertex \== Neighbor,
    \+ member(Neighbor,Visited),
    findall(Neighbor,newNeighbor(Vertex,Neighbor,Visited),NewNeighbors),
    coloring([Neighbor-NeighborColor|Rest],[Vertex,Neighbor|Visited]),
    \+matchNeighbor(Vertex-Color,[Neighbor-NeighborColor|Rest]).


findNewNeighbors(Vertex,Visited,NewNeighbors):-
    findall(Neighbor,newNeighbor(Vertex,Neighbor,Visited),NewNeighbors).

