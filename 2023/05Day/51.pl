rl(seed, soil, 50, 98, 2).
rl(seed, soil, 52, 50, 48).

rl(soil, fert, 0, 15, 37).
rl(soil, fert, 37, 52, 2).
rl(soil, fert, 39, 0, 15).

rl(fert, water, 49, 53, 8).
rl(fert, water, 0, 11, 42).
rl(fert, water, 42, 0, 7).
rl(fert, water, 57, 7, 4).

rl(water, light, 88, 18, 7).
rl(water, light, 18, 25, 70).

rl(light, temp, 45, 77, 23).
rl(light, temp, 81, 45, 19).
rl(light, temp, 68, 64, 13).

rl(temp, hum, 0, 69, 1).
rl(temp, hum, 1, 0, 69).

rl(hum, loc, 60, 56, 37).
rl(hum, loc, 56, 93, 4).

find(From, To, N, Map) :-
        rl(From, To, DestStart, RangeStart, RangeLen),
        RangeEnd is RangeStart + RangeLen,
        N >= RangeStart,
        N < RangeEnd,
        Map is DestStart + (N - RangeStart), !.
find(From, To, N, Map) :-
        rl(From, To, _, RangeStart, RangeLen),
        RangeEnd is RangeStart + RangeLen,
        (N < RangeStart ; N >= RangeEnd),
        Map is N, !.
find(From, To, N, Map) :-
        \+ rl(From, To, _, _, _),
        find(From, Int, N, Map1),
        find(Int, To, Map1, Map).

solve([X|Xs], Res) :-
        find(seed, loc, X, Min),
        solve_aux(Xs, Min, Res).

solve_aux([], Min, Min).
solve_aux([X|Xs], Min, Res) :-
        find(seed, loc, X, New),
        ((New < Min) -> NewMin = New ; NewMin = Min),
        solve_aux(Xs, NewMin, Res).
