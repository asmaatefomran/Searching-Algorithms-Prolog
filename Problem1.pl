grid_size(5, 5).

obstacle((1,5)).
obstacle((2,2)).
obstacle((3,3)).
obstacle((4,2)).
obstacle((5,4)).

pallon((1,3)).
pallon((2,5)).
pallon((3,4)).
pallon((4,1)).
pallon((5,3)).

starting_position((1,1)).

valid((X, Y)) :-
    grid_size(M,N),
    X >= 1, X =< M,
    Y >= 1, Y =< N,
    \+ obstacle((X, Y)).

% Possible moves
move((X, Y), (NX, Y)) :- NX is X + 1, valid((NX, Y)).  % Down
move((X, Y), (NX, Y)) :- NX is X - 1, valid((NX, Y)).  % Up
move((X, Y), (X, NY)) :- NY is Y + 1, valid((X, NY)).  % Right
move((X, Y), (X, NY)) :- NY is Y - 1, valid((X, NY)).  % Left

% Count Pallons
count_pallons([], 0).
count_pallons([Current | Rest], Count) :-
    pallon(Current),
    count_pallons(Rest, RemainingCount),
    ( member(Current, Rest) ->
        Count = RemainingCount      % repeated
    ;
        Count is RemainingCount + 1
    ).
count_pallons([Current | Rest], Count) :-
    \+ pallon(Current),
    count_pallons(Rest, Count).

% Display Grid
display_grid(CurrentPos, Visited) :-
    grid_size(M, N),
    nl, write('Grid State:'), nl,
    forall(between(1, M, X),
           (forall(between(1, N, Y),
                   (display_cell((X,Y), CurrentPos, Visited))),
            nl)),
    nl.

% Display individual cell
display_cell(Pos, CurrentPos, Visited) :-
    ( Pos = CurrentPos -> write('D') 
    ; obstacle(Pos) -> write('o')    
    ; pallon(Pos), \+ member(Pos, Visited) -> write('p') 
    ; member(Pos, Visited) -> write('*')  
    ; write('-')                     
    ),
    write(' ').

% Entry point
find_best_path(Path, MaxCount) :-
    starting_position(Start),
    bfs([[Start]], [], [], 0, Path, MaxCount).

% BFS 
bfs([], _, BestPath, BestCount, BestPath, BestCount).
bfs([[CurrentPos | RestPath] | Queue], Visited, CurrentBestPath, CurrentBestCount, FinalBestPath, FinalBestCount) :-
    reverse([CurrentPos | RestPath], FullPath),
    count_pallons(FullPath, PallonCount),
    (PallonCount > CurrentBestCount ->
        NewBestPath = FullPath, NewBestCount = PallonCount
    ;
        NewBestPath = CurrentBestPath, NewBestCount = CurrentBestCount
    ),
    findall([NextPos, CurrentPos | RestPath],
        (move(CurrentPos, NextPos), \+ member(NextPos, [CurrentPos | RestPath])),
        NewPaths),
    append(Queue, NewPaths, UpdatedQueue),
    bfs(UpdatedQueue, [CurrentPos | Visited], NewBestPath, NewBestCount, FinalBestPath, FinalBestCount).

% Start 
start :-
    find_best_path(RawPath, PallonCount),
    write('Best Path: '), write(RawPath), nl,
    write('Delivery Points Visited: '), write(PallonCount), nl,
    display_final_path(RawPath, []).

% Display in correct order
display_final_path([], _).
display_final_path([Pos | Rest], Visited) :-
    NewVisited = [Pos | Visited],  
    display_grid(Pos, NewVisited),
    display_final_path(Rest, NewVisited).