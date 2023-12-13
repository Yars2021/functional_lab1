-module(lab1_28f).

-export([naturals/1, filter_odd/1, shifted_square/3, reduce_list/1, reduce_list_t/1, spiral_sum/1, spiral_sum_t/1, spiral_sum_f/1]).

% List generation
naturals(N) -> naturals(N, N, []).

naturals(_, 0, L) -> L;
naturals(N, C, L) -> naturals(N, C - 1, [C | L]).


% Leaves only odds
filter_odd([]) -> [];
filter_odd([H | T]) when H rem 2 == 0 -> filter_odd(T);
filter_odd([H | T]) -> [H | filter_odd(T)].


% Map function
shifted_square([], _, _) -> [];
shifted_square([H | T], D, DD) -> [H * H + D | shifted_square(T, D + DD, DD)].


% Reduce to sum (recursion)
reduce_list([]) -> 0;
reduce_list([H | T]) -> H + reduce_list(T).


% Reduce to sum (tail recursion)
reduce_list_t(L) -> reduce_list_t(L, 0).

reduce_list_t([], Sum) -> Sum;
reduce_list_t([H | T], Sum) -> reduce_list_t(T, Sum + H).


% (Map, Reduce) x 4
spiral_sum_list(Odd_List) -> 
    reduce_list(shifted_square(Odd_List, 0, 0)) + 
    reduce_list(shifted_square(Odd_List, 0, -2)) + 
    reduce_list(shifted_square(Odd_List, 0, -4)) + 
    reduce_list(shifted_square(Odd_List, 0, -6)) - 3.


% Generate, Filter, Map + Reduce
spiral_sum(N) -> spiral_sum_list(filter_odd(naturals(N))).


% (Map, Reduce) x 4
spiral_sum_list_t(Odd_List) -> 
    reduce_list_t(shifted_square(Odd_List, 0, 0)) + 
    reduce_list_t(shifted_square(Odd_List, 0, -2)) + 
    reduce_list_t(shifted_square(Odd_List, 0, -4)) + 
    reduce_list_t(shifted_square(Odd_List, 0, -6)) - 3.


% Generate, Filter, Map + Reduce
spiral_sum_t(N) -> spiral_sum_list_t(filter_odd(naturals(N))).


% Fold
spiral_sum_list_f(Odd_List) -> 
    lists:foldl(fun(X, Acc) -> X + Acc end, 0, shifted_square(Odd_List, 0, 0)) +
    lists:foldl(fun(X, Acc) -> X + Acc end, 0, shifted_square(Odd_List, 0, -2)) +
    lists:foldl(fun(X, Acc) -> X + Acc end, 0, shifted_square(Odd_List, 0, -4)) +
    lists:foldl(fun(X, Acc) -> X + Acc end, 0, shifted_square(Odd_List, 0, -6)) - 3.


% Generate, Filter, Fold
spiral_sum_f(N) -> spiral_sum_list_f(filter_odd(naturals(N))).
