#include <cstdint>
#include <iostream>

uint_least64_t write(uint_least64_t val);
uint_least64_t read();
uint_least64_t test(uint_least64_t x);
uint_least64_t f();
uint_least64_t ff();
uint_least64_t fff();
int main();

uint_least64_t write(uint_least64_t val) {
	std::cout << val << '\n';
	return 0;
}

uint_least64_t read() {
	uint_least64_t ret;
	std::cin >> ret;
	return ret;
}

uint_least64_t test(uint_least64_t x) {
	return write(x);
}

uint_least64_t f() {
	return 10;

}

uint_least64_t ff() {
	return 1;

}

uint_least64_t fff() {
	if (read() > 5) {
		return f();
	}
	else {
		return ff();
	}
}

int main() {
	return test(fff());
}