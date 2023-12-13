-module(lab1_28_tests).
-include_lib("eunit/include/eunit.hrl").

% Reduce with normal recursion
spiral_sum_test() ->
    ?assertEqual(lab1_28f:spiral_sum(5), 101),
    ?assertEqual(lab1_28f:spiral_sum(1001), 669171001).

% Reduce with tail recursion
spiral_sum_t_test() ->
    ?assertEqual(lab1_28f:spiral_sum_t(5), 101),
    ?assertEqual(lab1_28f:spiral_sum_t(1001), 669171001).

% Fold
spiral_sum_f_test() ->
    ?assertEqual(lab1_28f:spiral_sum_f(5), 101),
    ?assertEqual(lab1_28f:spiral_sum_f(1001), 669171001).
