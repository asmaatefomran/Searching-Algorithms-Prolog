% Problem 2
:- dynamic obs/2, del/2, grid_s/2,recharge/2,get_energy/1.


% test Cases
% case 1 Energy = 10. Short and Long
%grid_s(4, 4).
%obs(2, 2).
%del(1, 4).
%recharge(3,1).

% case 2 Long and short but long reach the goal , Energy = 6.
%grid_s(5, 5).
%obs(2, 3).
%obs(2, 3).
%del(5, 5).
%recharge(3,1).

% Case 3 No Solution, Energy = 4.
%grid_s(4, 4).
%obs(2,2).
%obs(2,3).
%obs(3,2).
%obs(3,3).
%del(4, 4).

% case 4 have same solution , Energy = 6.
grid_s(3, 3).
obs(2, 2).

del(1, 3).
del(3, 1).
recharge(2,3).


move((X,Y), (NWX,Y), 1) :- NWX is X + 1, valid(NWX, Y). % right
move((X,Y), (X,NWY), 1) :- NWY is Y + 1, valid(X, NWY). % up
move((X,Y), (NWX,Y), 1) :- NWX is X - 1, valid(NWX, Y). % left
move((X,Y), (X,NWY), 1) :- NWY is Y - 1, valid(X, NWY). % down

% In the correct range or not
valid(X, Y) :-
    grid_s(MxX, MxY),
    X >= 1, X =< MxX,
    Y >= 1, Y =< MxY,
    \+ obs(X, Y).

% Heuristic calc
calc_h(Pos, Rem, H) :-
    findall(D, (member(P, Rem), manhattan_dis(Pos, P, D)), Dists),
    (Dists = [] -> H = 0; min_list(Dists, H)).

manhattan_dis((X1,Y1), (X2,Y2), D) :-
    D is abs(X1 - X2) + abs(Y1 - Y2).

% A* Search
astar_search(Start, Path,Energy) :-
    get_all_delivery_points(AllPoints),
    search([node(Start, AllPoints, [], 0, 0,Energy)], [], Path,Energy).

% No Path Found
search([],_,_,_):-
   !, write(' No Solutuion Found , False'), nl , fail.

% All Delivery Collected
search([node(Pos, [], Path, _, _, _)|_], _, FullPath,_) :-
    reverse([Pos|Path],FullPath), !.

% Recursive case
search([Node|Open], Closed, Sol,MxEnergy) :-
    Node = node(Pos, Rem, Path, G, _,En),
    findall(
        node(NwPos, NwRem, [Pos|Path], NwG, NwF,NwE),
        (
            move(Pos, NwPos, MoveCost),
            En >= MoveCost, % check if have enough energy
            (member(NwPos, Rem) ->
                select(NwPos, Rem, NwRem) ;
                NwRem = Rem
            ),
            % Recharge if find recharge station
            NwPos = (NX,NY),
           (recharge(NX,NY)-> NwE = MxEnergy;NwE is En - MoveCost),
            NwG is G + MoveCost,
            calc_h(NwPos, NwRem, H),
            NwF is NwG + H,
            \+ member(node(NwPos, NwRem, _, _, _, _), Closed)
        ),
        Children
    ),
    append(Open, Children, NwOpen),
    predsort(compare_f, NwOpen, SortedOpen),
    search(SortedOpen, [Node|Closed], Sol,MxEnergy).

% compare functions so we can sort
compare_f(<, node(_,_,_,_,F1,_), node(_,_,_,_,F2,_)) :- F1 < F2.
compare_f(>, node(_,_,_,_,F1,_), node(_,_,_,_,F2,_)) :- F1 > F2.
compare_f(=, _, _):- !.


get_all_delivery_points(Points) :-
    findall((X,Y), del(X,Y), Points).

% Ptint Final Path

print_path([]).
print_path([(X,Y)|Rest]) :-
    format('(~w, ~w) ', [X, Y]),
    print_path(Rest).

visualize_grid(Path) :-
    grid_s(M, N),
    format('~nGrid Visualization (D=Path, O=Obstacle, P=Delivery, R=Recharge):~n'),
    forall(between(1, N, Y),
       (
           nl,
           forall(between(1, M, X),
              (
                  (recharge(X,Y)-> write(' R ')
                  ;member((X,Y), Path) -> write(' D ')
                  ; obs(X,Y) -> write(' O ')
                  ; del(X,Y) -> write(' P ')
                  ; write(' . '))
              )
           )
       )
    ),
    nl.

% Problem 1

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

validp((X, Y)) :-
    grid_size(M,N),
    X >= 1, X =< M,
    Y >= 1, Y =< N,
    \+ obstacle((X, Y)).

% Possible moves
movep((X, Y), (NX, Y)) :- NX is X + 1, validp((NX, Y)).  % Down
movep((X, Y), (NX, Y)) :- NX is X - 1, validp((NX, Y)).  % Up
movep((X, Y), (X, NY)) :- NY is Y + 1, validp((X, NY)).  % Right
movep((X, Y), (X, NY)) :- NY is Y - 1, validp((X, NY)).  % Left

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
        (movep(CurrentPos, NextPos), \+ member(NextPos, [CurrentPos | RestPath])),
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


start2 :-
    write('Enter Starting Energy: '),
    read(Energy),
    (integer(Energy), Energy > 0 ->
        retractall(get_energy(_)),
        asserta(get_energy(Energy)),
        (astar_search((1,1), Path, Energy) ->
            format('Optimal path found:~n'),
            print_path(Path), nl,
            visualize_grid(Path)
        ;
            true
        )
    ;
    write('Invalid energy value. Please enter a positive integer.'), nl
    ).

