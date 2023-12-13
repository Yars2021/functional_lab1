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
