.PHONY: run
run: a.out
	./a.out

a.out: based.hpp based_test.hpp based.cpp
	g++ -std=c++23 -Wall -Wextra -Wpedantic -Werror -fno-exceptions -fno-rtti -O0 -g based.cpp

.PHONY: 
notest: based.hpp based_test.hpp based.cpp
	g++ -std=c++23 -Wall -Wextra -Wpedantic -Werror -fno-exceptions -fno-rtti -O0 -g -DSKIP_STATIC_ASSERT_TESTS based.cpp

.PHONY: clean
clean:
	rm -f a.out
