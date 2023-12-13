-module(spiral_tests).
-include_lib("eunit/include/eunit.hrl").

% Reduce with normal recursion
spiral_sum_test() ->
    ?assertEqual(spiral_alg:spiral_sum(5), 101),
    ?assertEqual(spiral_alg:spiral_sum(1001), 669171001).

% Reduce with tail recursion
spiral_sum_t_test() ->
    ?assertEqual(spiral_alg:spiral_sum_t(5), 101),
    ?assertEqual(spiral_alg:spiral_sum_t(1001), 669171001).

% Fold
spiral_sum_f_test() ->
    ?assertEqual(spiral_alg:spiral_sum_f(5), 101),
    ?assertEqual(spiral_alg:spiral_sum_f(1001), 669171001).
