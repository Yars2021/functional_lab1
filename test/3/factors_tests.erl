-module(factors_tests).
-include_lib("eunit/include/eunit.hrl").

% Reduce with normal recursion
get_max_factor_test() ->
    ?assertEqual(factors:get_max_factor(13195), 29),
    ?assertEqual(factors:get_max_factor(600851475143), 6857).

% Reduce with tail recursion
get_max_factor_t_test() ->
    ?assertEqual(factors:get_max_factor_t(13195), 29),
    ?assertEqual(factors:get_max_factor_t(600851475143), 6857).

% Fold
get_max_factor_f_test() ->
    ?assertEqual(factors:get_max_factor_f(13195), 29),
    ?assertEqual(factors:get_max_factor_f(600851475143), 6857).
