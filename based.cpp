/// Small example demonstrating runtime use of the based library.


#include <cassert>
#include <cstddef>
#include <expected>
#include <iostream>
#include <string>
#include <vector>

#ifndef SKIP_STATIC_ASSERT_TESTS
#include "based_test.hpp"
#else
#include "based.hpp"
#endif


int main() {
    std::vector<std::byte> original_data{std::byte{0x12}, std::byte{0x34}, std::byte{0x56}};
    std::string text = based::encode<based::base64, std::basic_string>(original_data);
    std::expected<std::vector<std::byte>, based::decode_error> data =
        based::decode<based::base64, std::vector>(text);
    assert(data.has_value());
    assert(data.value() == original_data);
}
