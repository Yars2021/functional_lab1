-module(a).

-export([item_s/4, sum_s/4, spiral_sum/1]).

item_s(S1, D1, DD, N) -> S1 + (2 * D1 + DD * (N - 2)) * (N - 1) / 2.

sum_s(S1, D1, DD, N) -> (S1 + (D1 + DD * (N - 2) / 3) * (N - 1) / 2) * N.

spiral_sum(N) ->
	sum_s(1, 2, 8, N div 2 + 1) +
	sum_s(1, 4, 8, N div 2 + 1) +
	sum_s(1, 6, 8, N div 2 + 1) +
	sum_s(1, 8, 8, N div 2 + 1) - 3.
