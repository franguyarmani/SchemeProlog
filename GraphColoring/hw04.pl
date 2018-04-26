color(0).
color(1).
color(2).
color(3).

matchNeighbor(Vertex-Color,List):-
    edge(us,Vertex,Neighbor),
    member(Neighbor-Color,List).

color_all([Vertex|Uncolored],Coloring,Colored):-
    color(Color),
    \+matchNeighbor(Vertex-Color,Coloring),
    color_all(Uncolored, [Vertex-Color|Coloring],Colored).
color_all([],Colored,Colored).

coloring(Graph, Colorings):-
    consult(Graph),
    findall(Vertex, vertex(us,Vertex), Vertices),
    color_all(Vertices, [], Colorings).
