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
	printf("%d\n", get_largest_factor(1234567890));

	return 0;
}
