-module(lab1_3f).

-export([is_prime/1, naturals/1, filter_primes/1, filter_factors/2, reduce_factors/1, reduce_factors_t/1, get_max_factor/1, get_max_factor_t/1, get_max_factor_f/1]).

% Tail recursion
is_prime(N) -> is_prime(N, 2, round(math:sqrt(N))).

is_prime(0, _, _) -> false;
is_prime(1, _, _) -> false;
is_prime(2, _, _) -> true;
is_prime(_, M, Lim) when M > Lim -> true;
is_prime(N, M, _) when N rem M == 0 -> false;
is_prime(N, M, Lim) -> is_prime(N, M + 1, Lim).


% List generation
naturals(N) -> naturals(N, N, []).

naturals(_, 0, L) -> L;
naturals(N, C, L) -> naturals(N, C - 1, [C | L]).


% List filtration (only primes)
filter_primes([]) -> [];
filter_primes([H | T]) -> 
    H_Is_Prime = is_prime(H),
    if
        H_Is_Prime == true -> [H | filter_primes(T)];
        true -> filter_primes(T)
    end.


% List filtration (only factors of N)
filter_factors(_, []) -> [];
filter_factors(N, [H | T]) when N rem H == 0 -> [H | filter_factors(N, T)];
filter_factors(N, [_ | T]) -> filter_factors(N, T).


% Reduce list of factors (recursion)
reduce_factors([]) -> 0;
reduce_factors([H | T]) ->
    Tail_Factor = reduce_factors(T),
    if
        Tail_Factor > H -> Tail_Factor;
        true -> H
    end.


% Reduce list of factors (tail recursion)
reduce_factors_t(L) -> reduce_factors_t(L, 0).

reduce_factors_t([], M) -> M;
reduce_factors_t([H | T], M) when H > M -> reduce_factors_t(T, H);
reduce_factors_t([_ | T], M) -> reduce_factors_t(T, M).


% Generate, Filter, Filter, Reduce
get_max_factor(0) -> error;
get_max_factor(1) -> error;
get_max_factor(N) -> reduce_factors(filter_factors(N, filter_primes(naturals(round(math:sqrt(N)))))).


% Generate, Filter, Filter, Reduce
get_max_factor_t(0) -> error;
get_max_factor_t(1) -> error;
get_max_factor_t(N) -> reduce_factors_t(filter_factors(N, filter_primes(naturals(round(math:sqrt(N)))))).


% Fold
get_max_factor_f(0) -> error;
get_max_factor_f(1) -> error;
get_max_factor_f(N) -> 
    lists:foldl(fun(X, Acc) -> 
            if
                X > Acc -> X;
                true -> Acc
            end
        end, 
        0, filter_factors(N, filter_primes(naturals(round(math:sqrt(N)))))).
