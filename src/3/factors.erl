-module(factors).

-export([is_prime/1, naturals/1]).
-export([naturals_server/1, naturals_next/1, naturals_lazy/1]).
-export([filter_primes/1, filter_factors/2]).
-export([reduce_factors/1, reduce_factors_t/1]).
-export([get_max_factor/1, get_max_factor_t/1, get_max_factor_f/1]).

get_max(A, B) when A > B -> A;
get_max(_, B) -> B.


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


% Lazy list generation
naturals_server(N) ->
    receive
        {From, next} ->
            From ! {self(), N},
            naturals_server(N - 1)
    end.

naturals_next(Pid) ->
    Pid ! {self(), next},
    receive
        {_From, N} -> N
    end.


naturals_lazy(N) ->
    naturals_lazy(spawn(factors, naturals_server, [N]), N, []).

naturals_lazy(_, 0, L) -> L;
naturals_lazy(Pid, C, L) ->
    naturals_lazy(Pid, C - 1, [naturals_next(Pid) | L]).


% List filtration (only primes)
filter_primes([]) -> [];
filter_primes([H | T]) ->
    HIsPrime = is_prime(H),
    case HIsPrime of
        true -> [H | filter_primes(T)];
        false -> filter_primes(T)
    end.


% List filtration (only factors of N)
filter_factors(_, []) -> [];
filter_factors(N, [H | T]) when N rem H == 0 -> [H | filter_factors(N, T)];
filter_factors(N, [_ | T]) -> filter_factors(N, T).


% Reduce list of factors (recursion)
reduce_factors([]) -> 0;
reduce_factors([H | T]) -> get_max(reduce_factors(T), H).


% Reduce list of factors (tail recursion)
reduce_factors_t(L) -> reduce_factors_t(L, 0).

reduce_factors_t([], M) -> M;
reduce_factors_t([H | T], M) when H > M -> reduce_factors_t(T, H);
reduce_factors_t([_ | T], M) -> reduce_factors_t(T, M).


% Generate (Lazy), Filter, Filter, Reduce
get_max_factor(0) -> error;
get_max_factor(1) -> error;
get_max_factor(N) ->
    reduce_factors(filter_factors(N, filter_primes(naturals_lazy(round(math:sqrt(N)))))).


% Generate, Filter, Filter, Reduce
get_max_factor_t(0) -> error;
get_max_factor_t(1) -> error;
get_max_factor_t(N) ->
    reduce_factors_t(filter_factors(N, filter_primes(naturals(round(math:sqrt(N)))))).


% Fold
get_max_factor_f(0) -> error;
get_max_factor_f(1) -> error;
get_max_factor_f(N) ->
    lists:foldl(fun(X, Acc) -> get_max(X, Acc) end,
        0, filter_factors(N, filter_primes(naturals(round(math:sqrt(N)))))).
