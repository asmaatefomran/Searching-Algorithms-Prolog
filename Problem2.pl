:- dynamic obs/2, del/2, grid_s/2.

grid_s(5, 5).


obs(2, 2).
obs(3, 4).
obs(5, 3).

del(1, 3).
del(4, 4).
del(5, 5).

move((X,Y), (NWX,Y), 1) :- NWX is X + 1, valid(NWX, Y). % right
move((X,Y), (X,NWY), 1) :- NWY is Y + 1, valid(X, NWY). % down
move((X,Y), (NWX,Y), 1) :- NWX is X - 1, valid(NWX, Y). % left
move((X,Y), (X,NWY), 1) :- NWY is Y - 1, valid(X, NWY). % up

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
astar_search(Start, Path) :-
    get_all_delivery_points(AllPoints),
    search([node(Start, AllPoints, [], 0, 0)], [], RevPath),
    reverse(RevPath, Path).

% Base case>>>>finished all delevry points
search([node(Pos, [], Path, _, _)|_], _, [Pos|Path]) :- !.

% Recursive case
search([Node|Open], Closed, Sol) :-
    Node = node(Pos, Rem, Path, G, _),
    findall(
        node(NwPos, NwRem, [Pos|Path], NwG, NwF),
        (
            move(Pos, NwPos, MoveCost),
            (member(NwPos, Rem) ->
                select(NwPos, Rem, NwRem) ;
                NwRem = Rem
            ),
            NwG is G + MoveCost,
            calc_h(NwPos, NwRem, H),
            NwF is NwG + H,
            \+ member(node(NwPos, NwRem, _, _, _), Closed)
        ),
        Children
    ),
    append(Open, Children, NwOpen),
    predsort(compare_f, NwOpen, SortedOpen),
    search(SortedOpen, [Node|Closed], Sol).

% compare functions so we can sort
compare_f(<, node(_,_,_,_,F1), node(_,_,_,_,F2)) :- F1 < F2.
compare_f(>, node(_,_,_,_,F1), node(_,_,_,_,F2)) :- F1 > F2.
compare_f(=, _, _).


get_all_delivery_points(Points) :-
    findall((X,Y), del(X,Y), Points).

% Main execution
:- initialization(main).


main:-
    (astar_search((1,1), Path) ->
        format('Optimal path found:~n'),
        print_path(Path),
        visualize_grid(Path)
    ;
        write('No path found! Check obstacles and delivery points.')
    ).

print_path([]).
print_path([(X,Y)|Rest]) :-
    format('(~w, ~w) ', [X, Y]),
    print_path(Rest).

visualize_grid(Path) :-
    grid_s(M, N),
    format('~nGrid Visualization (D=Path, O=Obstacle, P=Delivery):~n'),
    between(1, N, Y),
    nl,
    between(1, M, X),
    (member((X,Y), Path) -> write(' D ');
     obs(X,Y) -> write(' O ');
     del(X,Y) -> write(' P ');
     write(' . ')),
    fail.
visualize_grid(_) :- nl.
