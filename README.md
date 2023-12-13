# Функциональное программирование. Лабораторная работа №1

Вариант: 3, 28

Цель: освоить базовые приёмы и абстракции функционального программирования: функции, поток управления и поток данных,
сопоставление с образцом, рекурсия, свёртка, отображение, работа с функциями как с данными, списки.

В рамках лабораторной работы вам предлагается решить несколько задач [проекта Эйлер](https://projecteuler.net/archives).
Список задач -- ваш вариант.

Для каждой проблемы должно быть представлено несколько решений:

1. монолитные реализации с использованием:
    - хвостовой рекурсии;
    - рекурсии (вариант с хвостовой рекурсией не является примером рекурсии);
2. модульной реализации, где явно разделена генерация последовательности, фильтрация и свёртка (должны использоваться
   функции reduce/fold, filter и аналогичные);
3. генерация последовательности при помощи отображения (map);
4. работа со спец. синтаксисом для циклов (где применимо);
5. работа с бесконечными списками для языков, поддерживающих ленивые коллекции или итераторы как часть языка (к примеру
   Haskell, Clojure);
6. реализация на любом удобном для вас традиционном языке программирования для сравнения.

Требуется использовать идиоматичный для технологии стиль программирования.

Содержание отчёта:

- титульный лист;
- описание проблемы;
- ключевые элементы реализации с минимальными комментариями;
- выводы (отзыв об использованных приёмах программирования).

Примечания:

- необходимо понимание разницы между ленивыми коллекциями и итераторами;
- нужно знать особенности используемой технологии и того, как работают использованные вами приёмы.

## Задача 3. Largest Prime Factor

<p>The prime factors of 13195 are 5, 7, 13 and 29.</p>
<p>What is the largest prime factor of the number 600851475143?</p>

https://projecteuler.net/problem=3

### Реализация на C

```C
#include <stdio.h>
#include <math.h>

int is_prime(int arg) {
	if (arg < 2) return 0;

	for (int i = 2; i <= sqrt(arg) + 1; i++)
		if (arg % i == 0)
			return 0;

	return 1;
}

int next_prime(int arg) {
	for (; is_prime(arg + 1) == 0; arg++);
	return arg + 1;
}

int get_largest_factor(int arg) {
	int factor = 2;

	while (arg > 1 && factor <= arg) {
		if (arg % factor == 0) arg /= factor;
		else factor = next_prime(factor);
	}

	return factor;
}

int main(int argc, char **argv) {
	printf("%d\n", get_largest_factor(600851475143));

	return 0;
}
```

Результат:

```6857```

### Общая часть кода

```erlang
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
```

### Реализация c рекурсией

```erlang
% Reduce list of factors (recursion)
reduce_factors([]) -> 0;
reduce_factors([H | T]) -> get_max(reduce_factors(T), H).

% Generate, Filter, Filter, Reduce
get_max_factor(0) -> error;
get_max_factor(1) -> error;
get_max_factor(N) ->
    reduce_factors(filter_factors(N, filter_primes(naturals(round(math:sqrt(N)))))).
```

### Реализация с хвостовой рекурсией

```erlang
% Reduce list of factors (tail recursion)
reduce_factors_t(L) -> reduce_factors_t(L, 0).

reduce_factors_t([], M) -> M;
reduce_factors_t([H | T], M) when H > M -> reduce_factors_t(T, H);
reduce_factors_t([_ | T], M) -> reduce_factors_t(T, M).

% Generate, Filter, Filter, Reduce
get_max_factor_t(0) -> error;
get_max_factor_t(1) -> error;
get_max_factor_t(N) -> reduce_factors_t(filter_factors(N, filter_primes(naturals(round(math:sqrt(N)))))).
```

### Реализация со свёрткой

```erlang
% Fold
get_max_factor_f(0) -> error;
get_max_factor_f(1) -> error;
get_max_factor_f(N) -> 
    lists:foldl(fun(X, Acc) -> get_max(X, Acc) end, 
        0, filter_factors(N, filter_primes(naturals(round(math:sqrt(N)))))).
```

## Задача 28. Reciprocal Cycles

<p>Starting with the number 1 and moving to the right in a clockwise direction a 5 by 5 spiral is formed as follows:</p>
<p class="monospace center"><span class="red"><b>21</b></span> 22 23 24 <span class="red"><b>25</b></span><br>
20  <span class="red"><b>7</b></span>  8  <span class="red"><b>9</b></span> 10<br>
19  6  <span class="red"><b>1</b></span>  2 11<br>
18  <span class="red"><b>5</b></span>  4  <span class="red"><b>3</b></span> 12<br><span class="red"><b>17</b></span> 16 15 14 <span class="red"><b>13</b></span></p>
<p>It can be verified that the sum of the numbers on the diagonals is 101.</p>
<p>What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?</p>

https://projecteuler.net/problem=28

### Реализация на C

```C
#include <stdio.h>

int get_spiral_sum_a(int n) {
    int odds[n / 2 + 1];
    int s2 = 0, s3 = 0, s4 = 0;
    int dd2 = -2, dd3 = -4, dd4 = -6;
    int ans = 0;

    for (int i = 0; i < n / 2 + 1; i++) odds[i] = i * 2 + 1;
    for (int i = 0; i < n / 2 + 1; i++) odds[i] *= odds[i];

    for (int i = 0; i < n / 2 + 1; i++) {
        ans += (odds[i] * 4 + s2 + s3 + s4);
        s2 += dd2;
        s3 += dd3;
        s4 += dd4;
    }

    return ans - 3;
}

int main(int argc, char **argv) {
	printf("%d\n", get_spiral_sum_a(1001));

	return 0;
}
```

Результат:

```669171001```

### Отдельное математически доказанное решение
```erlang
sum_s(S1, D1, DD, N) -> (S1 + (D1 + DD * (N - 2) / 3) * (N - 1) / 2) * N.

spiral_sum(N) ->
  sum_s(1, 2, 8, N div 2 + 1) +
  sum_s(1, 4, 8, N div 2 + 1) +
  sum_s(1, 6, 8, N div 2 + 1) +
  sum_s(1, 8, 8, N div 2 + 1) - 3.
```

### Общая часть кода для алгоритмического решения

```erlang
% List generation
naturals(N) -> naturals(N, N, []).

naturals(_, 0, L) -> L;
naturals(N, C, L) -> naturals(N, C - 1, [C | L]).


% Leaves only odds
filter_odd([]) -> [];
filter_odd([H | T]) when H rem 2 == 0 -> filter_odd(T);
filter_odd([H | T]) -> [H | filter_odd(T)].

% Apply x^2 + d, d += dd
shifted_square([], _, _) -> [];
shifted_square([H | T], D, DD) -> [H * H + D | shifted_square(T, D + DD, DD)].
```

### Реализация с рекурсией

```erlang
% Reduce to sum (recursion)
reduce_list([]) -> 0;
reduce_list([H | T]) -> H + reduce_list(T).


% Reduce x 4
spiral_sum_list(OddList) ->
    reduce_list(shifted_square(OddList, 0, 0)) +
    reduce_list(shifted_square(OddList, 0, -2)) +
    reduce_list(shifted_square(OddList, 0, -4)) +
    reduce_list(shifted_square(OddList, 0, -6)) - 3.


% Generate, Filter, Reduce
spiral_sum(N) -> spiral_sum_list(filter_odd(naturals(N))).
```

### Реализация с хвостовой рекурсией и filter

```erlang
% Reduce to sum (tail recursion)
reduce_list_t(L) -> reduce_list_t(L, 0).

reduce_list_t([], Sum) -> Sum;
reduce_list_t([H | T], Sum) -> reduce_list_t(T, Sum + H).


% Reduce x 4
spiral_sum_list_t(OddList) ->
    reduce_list_t(shifted_square(OddList, 0, 0)) +
    reduce_list_t(shifted_square(OddList, 0, -2)) +
    reduce_list_t(shifted_square(OddList, 0, -4)) +
    reduce_list_t(shifted_square(OddList, 0, -6)) - 3.


% Generate, Filter, Reduce
spiral_sum_t(N) -> spiral_sum_list_t(lists:filter(fun(X) -> X rem 2 == 1 end, naturals(N))).
```

### Реализация со свёрткой

```erlang
% Fold
spiral_sum_list_f(OddList) ->
    lists:foldl(fun(X, Acc) -> X + Acc end, 0, shifted_square(OddList, 0, 0)) +
    lists:foldl(fun(X, Acc) -> X + Acc end, 0, shifted_square(OddList, 0, -2)) +
    lists:foldl(fun(X, Acc) -> X + Acc end, 0, shifted_square(OddList, 0, -4)) +
    lists:foldl(fun(X, Acc) -> X + Acc end, 0, shifted_square(OddList, 0, -6)) - 3.


% Generate, Filter, Fold
spiral_sum_f(N) -> spiral_sum_list_f(filter_odd(naturals(N))).
```

### Реализация с map

```erlang
spiral_sum_list(OddList) ->
    reduce_list(shifted_square(OddList, 0, 0)) +
    reduce_list(shifted_square(OddList, 0, -2)) +
    reduce_list(shifted_square(OddList, 0, -4)) +
    reduce_list(shifted_square(OddList, 0, -6)) - 3.


% Map
spiral_sum_m(N) -> spiral_sum_list(lists:map(fun(X) -> 2 * (X - 1) + 1 end, naturals(N div 2 + 1))).
```

## Выводы

В ходе выполнения работы я ознакомился с языком Erlang и базовыми принципами функционального программирования.
Я реализовал две задачи из проекта Эйлер в нескольких вариантах: с обычной рекурсией, с хвостовой, со свёрткой (fold) с отображением (map).
Во второй задаче были использованы библиотченые filter, foldl и map.
Ещё для второй задачи я нашел математическое решение, работающее быстрее алогитмических. К реализациям были написаны тесты.
Также для сравнения обе задачи были реализованы также на языке C.
