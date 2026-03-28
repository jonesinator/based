/// Compile-time tests for the based library using static_assert. Aims to achieve 100% code coverage
/// at compile-time, but it's difficult to prove this with current tooling.
///
/// This file includes based.hpp and adds extensive static_assert checks to validate correctness.


#ifndef BASED_TEST_HPP
#define BASED_TEST_HPP


#include "based.hpp"
#include <vector>


namespace based {


// Validate round_up.
static_assert(round_up(0, 4) == 0);
static_assert(round_up(1, 4) == 4);
static_assert(round_up(4, 4) == 4);
static_assert(round_up(5, 4) == 8);

// Validate array concatenation.
static_assert(concat(std::array{1, 2, 3}, std::array{4, 5, 6}) == std::array{1, 2, 3, 4, 5, 6});
static_assert(concat(std::array<int, 0>{}, std::array{1}) == std::array{1});
static_assert(concat(std::array{1}, std::array<int, 0>{}) == std::array{1});

static_assert(concat(std::array{1, 2}, std::array{3, 4}, std::array{5, 6}) == std::array{1, 2, 3, 4, 5, 6});
static_assert(concat(std::array{1}, std::array{2}, std::array{3}, std::array{4}) == std::array{1, 2, 3, 4});

// Validate chars.
static_assert(chars<'X'> == std::array{'X'});
static_assert(chars<'A', 'D'> == std::array{'A', 'B', 'C', 'D'});

// Validate valid_encoding_size.
static_assert(!valid_encoding_size<128>);
static_assert( valid_encoding_size< 64>);
static_assert( valid_encoding_size< 32>);
static_assert( valid_encoding_size< 16>);
static_assert( valid_encoding_size<  8>);
static_assert( valid_encoding_size<  4>);
static_assert(!valid_encoding_size<  3>);
static_assert( valid_encoding_size<  2>);
static_assert(!valid_encoding_size<  1>);

// Validate bits_per_char.
static_assert(bits_per_char<64> == 6);
static_assert(bits_per_char<32> == 5);
static_assert(bits_per_char<16> == 4);
static_assert(bits_per_char< 8> == 3);
static_assert(bits_per_char< 4> == 2);
static_assert(bits_per_char< 2> == 1);

// Validate block_bytes.
static_assert(block_bytes<64> == 3);
static_assert(block_bytes<32> == 5);
static_assert(block_bytes<16> == 1);
static_assert(block_bytes< 8> == 3);
static_assert(block_bytes< 4> == 1);
static_assert(block_bytes< 2> == 1);

// Validate block_chars.
static_assert(block_chars<64> == 4);
static_assert(block_chars<32> == 8);
static_assert(block_chars<16> == 2);
static_assert(block_chars< 8> == 8);
static_assert(block_chars< 4> == 4);
static_assert(block_chars< 2> == 8);

// Validate encoded_size.
static_assert(encoded_size<64>(0) == 0);
static_assert(encoded_size<64>(1) == 4);
static_assert(encoded_size<64>(2) == 4);
static_assert(encoded_size<64>(3) == 4);
static_assert(encoded_size<64>(4) == 8);
static_assert(encoded_size<32>(0) == 0);
static_assert(encoded_size<32>(1) == 8);
static_assert(encoded_size<32>(5) == 8);
static_assert(encoded_size<32>(6) == 16);
static_assert(encoded_size<16>(0) == 0);
static_assert(encoded_size<16>(1) == 2);
static_assert(encoded_size<16>(2) == 4);
static_assert(encoded_size<16>(3) == 6);
static_assert(encoded_size< 8>(0) == 0);
static_assert(encoded_size< 8>(1) == 8);
static_assert(encoded_size< 8>(2) == 8);
static_assert(encoded_size< 8>(3) == 8);
static_assert(encoded_size< 8>(4) == 16);
static_assert(encoded_size< 4>(0) == 0);
static_assert(encoded_size< 4>(1) == 4);
static_assert(encoded_size< 4>(2) == 8);
static_assert(encoded_size< 2>(0) == 0);
static_assert(encoded_size< 2>(1) == 8);
static_assert(encoded_size< 2>(2) == 16);

// Validate decoded_size.
static_assert(decoded_size<64>( 0) ==  0);
static_assert(decoded_size<64>( 4) ==  3);
static_assert(decoded_size<64>( 8) ==  6);
static_assert(decoded_size<32>( 0) ==  0);
static_assert(decoded_size<32>( 8) ==  5);
static_assert(decoded_size<32>(16) == 10);
static_assert(decoded_size<16>( 0) ==  0);
static_assert(decoded_size<16>( 2) ==  1);
static_assert(decoded_size<16>( 4) ==  2);
static_assert(decoded_size< 8>( 0) ==  0);
static_assert(decoded_size< 8>( 8) ==  3);
static_assert(decoded_size< 8>(16) ==  6);
static_assert(decoded_size< 4>( 0) ==  0);
static_assert(decoded_size< 4>( 4) ==  1);
static_assert(decoded_size< 4>( 8) ==  2);
static_assert(decoded_size< 2>( 0) ==  0);
static_assert(decoded_size< 2>( 8) ==  1);
static_assert(decoded_size< 2>(16) ==  2);

// Validate needs_pad.
static_assert( needs_pad<64>);
static_assert( needs_pad<32>);
static_assert(!needs_pad<16>);
static_assert( needs_pad< 8>);
static_assert(!needs_pad< 4>);
static_assert(!needs_pad< 2>);


/// Some additional utilities are required to perform full testing of the library at compile-time.
/// These functions are placed in a separate, nested namespace. They are not considered part of the
/// library, and can change at any time.
namespace test {


/// Turns a variadic list of arguments into a byte array.
template <typename... T>
[[nodiscard]] consteval std::array<std::byte, sizeof...(T)> to_bytes(T... values) noexcept {
    return std::array<std::byte, sizeof...(values)>{static_cast<std::byte>(values)...};
}


/// Turns a string constant into a character array.
template <std::size_t N>
consteval std::array<char, N - 1> to_text(const char (&text)[N]) noexcept {
    std::array<char, N - 1> result{};
    for (std::size_t i = 0; i < N - 1; ++i) {
        result[i] = text[i];
    }
    return result;
}


/// Gets a sized span from a byte array.
template <std::size_t N>
[[nodiscard]] consteval std::span<const std::byte, N> byte_span(
    const std::array<std::byte, N>& bytes
) noexcept {
    return std::span<const std::byte, N>{bytes};
}


/// Positive tests for all encoding and decoding interfaces.
template <encoding E, std::size_t NBytes, std::size_t NChars>
consteval bool test_encoding(
    const std::array<std::byte, NBytes> data,
    const std::array<char, NChars> text
) noexcept {
    static constexpr std::size_t data_buffer_size = round_up(NBytes, E.block_bytes);
    using data_buffer_block = std::array<std::byte, data_buffer_size>;

    data_buffer_block data_buffer{};
    std::array<std::byte, NBytes> data_buffer_sized{};
    std::vector<std::byte> data_vector(data.begin(), data.end());
    std::vector<std::byte> decoded;
    std::array<char, NChars> text_buffer{};
    std::vector<char> text_vector(text.begin(), text.end());
    std::vector<char> encoded;

    // Sizes known fully at compile-time. Encode into a std::span backed by a std::array.
    text_buffer.fill('\0');
    encode<E>(std::span<char, NChars>{text_buffer}, byte_span(data));
    if (text_buffer != text) {
        std::abort();
    }

    // Sizes known fully at compile-time. Encode into a new std::array.
    if (encode<E, std::array>(byte_span(data)) != text) {
        std::abort();
    }

    // Sizes known only at run-time. Encode into a std::span backed by a std::array.
    text_buffer.fill('\0');
    if (!encode<E>(std::span<char>{text_buffer}, data).has_value()) {
        std::abort();
    }
    if (text_buffer != text) {
        std::abort();
    }

    // Sizes known only at run-time. Encode into a new std::vector.
    if (encode<E, std::vector>(data) != text_vector) {
        std::abort();
    }

    // Single block encoder.
    encoded.clear();
    encoder<E, 1> encoder1;
    for (const std::byte b : data) {
        if (auto encoded_block = encoder1.push(b)) {
            std::ranges::copy(*encoded_block, std::back_inserter(encoded));
        }
    }
    if (auto text_and_size = encoder1.flush()) {
        const auto& [encoded_block, size] = *text_and_size;
        std::ranges::copy(
            encoded_block.begin(), encoded_block.begin() + size, std::back_inserter(encoded));
    }
    if (encoded != text_vector) {
        std::abort();
    }

    // Double block encoder.
    encoded.clear();
    encoder<E, 2> encoder2;
    for (const std::byte b : data) {
        if (auto text = encoder2.push(b)) {
            std::ranges::copy(*text, std::back_inserter(encoded));
        }
    }
    if (auto text_and_size = encoder2.flush()) {
        const auto& [text, size] = *text_and_size;
        std::ranges::copy(text.begin(), text.begin() + size, std::back_inserter(encoded));
    }
    if (encoded != text_vector) {
        std::abort();
    }

    // Sizes known fully at compile-time. Decode into a std::span backed by a std::array.
    data_buffer.fill(std::byte{0});
    std::expected<decode_success, decode_error> result_1 = decode<E>(
        std::span<std::byte, data_buffer_size>{data_buffer}, std::span<const char, NChars>{text});
    if (!result_1.has_value()) {
        std::abort();
    }
    if (result_1.value().pad_bytes != data_buffer_size - NBytes) {
        std::abort();
    }
    std::copy_n(data_buffer.begin(), NBytes, data_buffer_sized.data());
    if (data_buffer_sized != data) {
        std::abort();
    }

    // Sizes known fully at compile-time. Decode into a new std::array.
    std::expected<std::tuple<data_buffer_block, decode_success>, decode_error> result_2 =
        decode<E, std::array>(std::span<const char, NChars>{text});
    if (!result_2.has_value()) {
        std::abort();
    }
    if (std::get<decode_success>(result_2.value()).pad_bytes != data_buffer_size - NBytes) {
        std::abort();
    }
    std::copy_n(std::get<0>(result_2.value()).begin(), NBytes, data_buffer_sized.data());
    if (data_buffer_sized != data) {
        std::abort();
    }

    // Sizes known only at run-time. Decode into a std::span backed by a std::array.
    data_buffer.fill(std::byte{0});
    std::expected<decode_success, decode_error> result_3 = decode<E>(data_buffer, text);
    if (!result_3.has_value()) {
        std::abort();
    }
    if (result_3.value().pad_bytes != data_buffer_size - NBytes) {
        std::abort();
    }
    std::copy_n(data_buffer.begin(), NBytes, data_buffer_sized.data());
    if (data_buffer_sized != data) {
        std::abort();
    }

    // Sizes known only at run-time. Decode into a new std::vector.
    std::expected<std::vector<std::byte>, decode_error> result_4 =
        decode<E, std::vector>(std::span<const char, NChars>{text});
    if (!result_4.has_value()) {
        std::abort();
    }
    std::copy_n(result_4.value().begin(), NBytes, data_buffer_sized.data());
    if (data_buffer_sized != data) {
        std::abort();
    }

    // Single block decoder.
    decoded.clear();
    decoder<E, 1> decoder1;
    for (const char c : text) {
        if (auto result_5 = decoder1.push(c)) {
            if (result_5.value().has_value()) {
                const auto [decoded_block, result] = result_5.value().value();
                const std::size_t length = decoded_block.size() - result.pad_bytes;
                std::ranges::copy_n(decoded_block.begin(), length, std::back_inserter(decoded));
            }
        }
    }
    if (auto result_5 = decoder1.flush()) {
        if (result_5.value().has_value()) {
            const auto [decoded_block, result] = result_5.value().value();
            const std::size_t length = decoded_block.size() - result.pad_bytes;
            std::ranges::copy_n(decoded_block.begin(), length, std::back_inserter(decoded));
        }
    }
    if (decoded != data_vector) {
        std::abort();
    }

    // Multi Block Decoder.
    decoded.clear();
    decoder<E, 2> decoder2;
    for (const char c : text) {
        if (auto result_6 = decoder2.push(c)) {
            if (result_6.value().has_value()) {
                const auto [decoded_block, result] = result_6.value().value();
                const std::size_t length = decoded_block.size() - result.pad_bytes;
                std::ranges::copy_n(decoded_block.begin(), length, std::back_inserter(decoded));
            }
        }
    }
    if (auto result_6 = decoder2.flush()) {
        if (result_6.value().has_value()) {
            const auto [decoded_block, result] = result_6.value().value();
            const std::size_t length = decoded_block.size() - result.pad_bytes;
            std::ranges::copy_n(decoded_block.begin(), length, std::back_inserter(decoded));
        }
    }
    if (decoded != data_vector) {
        std::abort();
    }

    return true;
}


/// Negative tests for decoding.
template <encoding E, std::size_t NChars>
consteval bool test_decode_error(
    const std::array<char, NChars> text,
    const decode_error expected
) noexcept {
    std::expected<std::vector<std::byte>, decode_error> result =
        decode<E, std::vector>(std::span<const char>{text});
    if (result.has_value()) {
        std::abort();
    }
    if (result.error() != expected) {
        std::abort();
    }
    return true;
}


/// Negative tests for encoding buffer size.
template <encoding E>
consteval bool test_encode_buffer_size_error(
    std::size_t text_chars,
    std::size_t data_bytes,
    std::size_t expected
) noexcept {
    std::vector<char> text(text_chars);
    std::vector<std::byte> data(data_bytes);
    std::expected<void, encode_error_buffer_size> result = encode<E>(text, data);
    if (result.has_value()) {
        std::abort();
    }
    if (result.error() != (encode_error_buffer_size { text_chars, expected })) {
        std::abort();
    }
    return true;
}


/// Negative tests for decoding buffer size.
template <encoding E>
consteval bool test_decode_buffer_size_error(
    std::size_t text_chars,
    std::size_t data_bytes,
    std::size_t expected
) noexcept {
    std::vector<char> text(text_chars);
    std::vector<std::byte> data(data_bytes);
    std::expected<decode_success, decode_error> result = decode<E>(data, text);
    if (result.has_value()) {
        std::abort();
    }
    if (result.error() != decode_error{decode_error_buffer_size { data_bytes, expected }}) {
        std::abort();
    }
    return true;
}


// Run through the examples from section 9 of the RFC.
constexpr std::array<std::byte, 6> test_data_6 = to_bytes(0x14, 0xfb, 0x9c, 0x03, 0xd9, 0x7e);
constexpr std::array<std::byte, 5> test_data_5 = to_bytes(0x14, 0xfb, 0x9c, 0x03, 0xd9);
constexpr std::array<std::byte, 4> test_data_4 = to_bytes(0x14, 0xfb, 0x9c, 0x03);

static_assert(test_encoding<base64   >(test_data_6, to_text("FPucA9l+"        )));
static_assert(test_encoding<base64   >(test_data_5, to_text("FPucA9k="        )));
static_assert(test_encoding<base64   >(test_data_4, to_text("FPucAw=="        )));
static_assert(test_encoding<base64url>(test_data_6, to_text("FPucA9l-"        )));
static_assert(test_encoding<base64url>(test_data_5, to_text("FPucA9k="        )));
static_assert(test_encoding<base64url>(test_data_4, to_text("FPucAw=="        )));
static_assert(test_encoding<base32   >(test_data_6, to_text("CT5ZYA6ZPY======")));
static_assert(test_encoding<base32   >(test_data_5, to_text("CT5ZYA6Z"        )));
static_assert(test_encoding<base32   >(test_data_4, to_text("CT5ZYAY="        )));
static_assert(test_encoding<base32hex>(test_data_6, to_text("2JTPO0UPFO======")));
static_assert(test_encoding<base32hex>(test_data_5, to_text("2JTPO0UP"        )));
static_assert(test_encoding<base32hex>(test_data_4, to_text("2JTPO0O="        )));
static_assert(test_encoding<base16   >(test_data_6, to_text("14FB9C03D97E"    )));
static_assert(test_encoding<base16   >(test_data_5, to_text("14FB9C03D9"      )));
static_assert(test_encoding<base16   >(test_data_4, to_text("14FB9C03"        )));

// Run through the test vectors from section 10 of the RFC.
static_assert(test_encoding<base64   >(to_bytes(                            ), to_text(""                )));
static_assert(test_encoding<base64   >(to_bytes('f'                         ), to_text("Zg=="            )));
static_assert(test_encoding<base64   >(to_bytes('f', 'o'                    ), to_text("Zm8="            )));
static_assert(test_encoding<base64   >(to_bytes('f', 'o', 'o'               ), to_text("Zm9v"            )));
static_assert(test_encoding<base64   >(to_bytes('f', 'o', 'o', 'b'          ), to_text("Zm9vYg=="        )));
static_assert(test_encoding<base64   >(to_bytes('f', 'o', 'o', 'b', 'a'     ), to_text("Zm9vYmE="        )));
static_assert(test_encoding<base64   >(to_bytes('f', 'o', 'o', 'b', 'a', 'r'), to_text("Zm9vYmFy"        )));
static_assert(test_encoding<base64url>(to_bytes(                            ), to_text(""                )));
static_assert(test_encoding<base64url>(to_bytes('f'                         ), to_text("Zg=="            )));
static_assert(test_encoding<base64url>(to_bytes('f', 'o'                    ), to_text("Zm8="            )));
static_assert(test_encoding<base64url>(to_bytes('f', 'o', 'o'               ), to_text("Zm9v"            )));
static_assert(test_encoding<base64url>(to_bytes('f', 'o', 'o', 'b'          ), to_text("Zm9vYg=="        )));
static_assert(test_encoding<base64url>(to_bytes('f', 'o', 'o', 'b', 'a'     ), to_text("Zm9vYmE="        )));
static_assert(test_encoding<base64url>(to_bytes('f', 'o', 'o', 'b', 'a', 'r'), to_text("Zm9vYmFy"        )));
static_assert(test_encoding<base32   >(to_bytes(                            ), to_text(""                )));
static_assert(test_encoding<base32   >(to_bytes('f'                         ), to_text("MY======"        )));
static_assert(test_encoding<base32   >(to_bytes('f', 'o'                    ), to_text("MZXQ===="        )));
static_assert(test_encoding<base32   >(to_bytes('f', 'o', 'o'               ), to_text("MZXW6==="        )));
static_assert(test_encoding<base32   >(to_bytes('f', 'o', 'o', 'b'          ), to_text("MZXW6YQ="        )));
static_assert(test_encoding<base32   >(to_bytes('f', 'o', 'o', 'b', 'a'     ), to_text("MZXW6YTB"        )));
static_assert(test_encoding<base32   >(to_bytes('f', 'o', 'o', 'b', 'a', 'r'), to_text("MZXW6YTBOI======")));
static_assert(test_encoding<base32hex>(to_bytes(                            ), to_text(""                )));
static_assert(test_encoding<base32hex>(to_bytes('f'                         ), to_text("CO======"        )));
static_assert(test_encoding<base32hex>(to_bytes('f', 'o'                    ), to_text("CPNG===="        )));
static_assert(test_encoding<base32hex>(to_bytes('f', 'o', 'o'               ), to_text("CPNMU==="        )));
static_assert(test_encoding<base32hex>(to_bytes('f', 'o', 'o', 'b'          ), to_text("CPNMUOG="        )));
static_assert(test_encoding<base32hex>(to_bytes('f', 'o', 'o', 'b', 'a'     ), to_text("CPNMUOJ1"        )));
static_assert(test_encoding<base32hex>(to_bytes('f', 'o', 'o', 'b', 'a', 'r'), to_text("CPNMUOJ1E8======")));
static_assert(test_encoding<base16   >(to_bytes(                            ), to_text(""                )));
static_assert(test_encoding<base16   >(to_bytes('f'                         ), to_text("66"              )));
static_assert(test_encoding<base16   >(to_bytes('f', 'o'                    ), to_text("666F"            )));
static_assert(test_encoding<base16   >(to_bytes('f', 'o', 'o'               ), to_text("666F6F"          )));
static_assert(test_encoding<base16   >(to_bytes('f', 'o', 'o', 'b'          ), to_text("666F6F62"        )));
static_assert(test_encoding<base16   >(to_bytes('f', 'o', 'o', 'b', 'a'     ), to_text("666F6F6261"      )));
static_assert(test_encoding<base16   >(to_bytes('f', 'o', 'o', 'b', 'a', 'r'), to_text("666F6F626172"    )));

// Encode buffer size errors.
static_assert(test_encode_buffer_size_error<base64   >( 1,  4,  8));
static_assert(test_encode_buffer_size_error<base64   >( 2,  8, 12));
static_assert(test_encode_buffer_size_error<base64url>( 1,  4,  8));
static_assert(test_encode_buffer_size_error<base64url>( 2,  8, 12));
static_assert(test_encode_buffer_size_error<base32   >( 1,  8, 16));
static_assert(test_encode_buffer_size_error<base32   >( 2, 16, 32));
static_assert(test_encode_buffer_size_error<base32hex>( 1,  8, 16));
static_assert(test_encode_buffer_size_error<base32hex>( 2, 16, 32));
static_assert(test_encode_buffer_size_error<base16   >( 1,  1,  2));
static_assert(test_encode_buffer_size_error<base16   >( 2,  2,  4));

// Decode buffer size errors.
static_assert(test_decode_buffer_size_error<base64   >( 4, 2,  3));
static_assert(test_decode_buffer_size_error<base64   >( 8, 3,  6));
static_assert(test_decode_buffer_size_error<base64url>( 4, 2,  3));
static_assert(test_decode_buffer_size_error<base64url>( 8, 3,  6));
static_assert(test_decode_buffer_size_error<base32   >( 8, 2,  5));
static_assert(test_decode_buffer_size_error<base32   >(16, 3, 10));
static_assert(test_decode_buffer_size_error<base32hex>( 8, 2,  5));
static_assert(test_decode_buffer_size_error<base32hex>(16, 3, 10));
static_assert(test_decode_buffer_size_error<base16   >( 4, 1,  2));
static_assert(test_decode_buffer_size_error<base16   >( 6, 2,  3));

// Message size errors.
static_assert(test_decode_error<base64   >(to_text("A" ), decode_error_message_size{ 1, 4 }));
static_assert(test_decode_error<base64   >(to_text("AA"), decode_error_message_size{ 2, 4 }));
static_assert(test_decode_error<base64url>(to_text("A" ), decode_error_message_size{ 1, 4 }));
static_assert(test_decode_error<base64url>(to_text("AA"), decode_error_message_size{ 2, 4 }));
static_assert(test_decode_error<base32   >(to_text("A" ), decode_error_message_size{ 1, 8 }));
static_assert(test_decode_error<base32   >(to_text("AA"), decode_error_message_size{ 2, 8 }));
static_assert(test_decode_error<base32hex>(to_text("A" ), decode_error_message_size{ 1, 8 }));
static_assert(test_decode_error<base32hex>(to_text("AA"), decode_error_message_size{ 2, 8 }));
static_assert(test_decode_error<base16   >(to_text("A" ), decode_error_message_size{ 1, 2 }));

// Illegal character errors.
static_assert(test_decode_error<base64   >(to_text("AA^AAAAA"), decode_error_character{ '^', 2 }));
static_assert(test_decode_error<base64   >(to_text("AAAAAAA^"), decode_error_character{ '^', 7 }));
static_assert(test_decode_error<base64url>(to_text("AA^AAAAA"), decode_error_character{ '^', 2 }));
static_assert(test_decode_error<base64url>(to_text("AAAAAAA^"), decode_error_character{ '^', 7 }));
static_assert(test_decode_error<base32   >(to_text("AA^AAAAA"), decode_error_character{ '^', 2 }));
static_assert(test_decode_error<base32   >(to_text("AAAAAAA^"), decode_error_character{ '^', 7 }));
static_assert(test_decode_error<base32hex>(to_text("AA^AAAAA"), decode_error_character{ '^', 2 }));
static_assert(test_decode_error<base32hex>(to_text("AAAAAAA^"), decode_error_character{ '^', 7 }));
static_assert(test_decode_error<base16   >(to_text("AA^AAAAA"), decode_error_character{ '^', 2 }));
static_assert(test_decode_error<base16   >(to_text("AAAAAAA^"), decode_error_character{ '^', 7 }));

// Illegal padding errors (characters after padding character).
static_assert(test_decode_error<base64   >(to_text("AA=AAAAA"),  decode_error_pad{ 3 }));
static_assert(test_decode_error<base64url>(to_text("AA=AAAAA"),  decode_error_pad{ 3 }));
static_assert(test_decode_error<base32   >(to_text("AA=AAAAA"),  decode_error_pad{ 3 }));
static_assert(test_decode_error<base32hex>(to_text("AA=AAAAA"),  decode_error_pad{ 3 }));

// Illegal padding errors (nonsensical padding length).
static_assert(test_decode_error<base64   >(to_text("===="),     decode_error_pad_length{ 4 }));
static_assert(test_decode_error<base64   >(to_text("A==="),     decode_error_pad_length{ 3 }));
static_assert(test_decode_error<base64url>(to_text("===="),     decode_error_pad_length{ 4 }));
static_assert(test_decode_error<base64url>(to_text("A==="),     decode_error_pad_length{ 3 }));
static_assert(test_decode_error<base32   >(to_text("========"), decode_error_pad_length{ 8 }));
static_assert(test_decode_error<base32   >(to_text("A======="), decode_error_pad_length{ 7 }));
static_assert(test_decode_error<base32   >(to_text("AAA====="), decode_error_pad_length{ 5 }));
static_assert(test_decode_error<base32   >(to_text("AAAAAA=="), decode_error_pad_length{ 2 }));
static_assert(test_decode_error<base32hex>(to_text("========"), decode_error_pad_length{ 8 }));
static_assert(test_decode_error<base32hex>(to_text("A======="), decode_error_pad_length{ 7 }));
static_assert(test_decode_error<base32hex>(to_text("AAA====="), decode_error_pad_length{ 5 }));
static_assert(test_decode_error<base32hex>(to_text("AAAAAA=="), decode_error_pad_length{ 2 }));

// Illegal padding errors (nonsensical padding length, but bigger).
static_assert(test_decode_error<base64   >(to_text("AAA====="        ), decode_error_pad_length{  5 }));
static_assert(test_decode_error<base64   >(to_text("AA======"        ), decode_error_pad_length{  6 }));
static_assert(test_decode_error<base64   >(to_text("A======="        ), decode_error_pad_length{  7 }));
static_assert(test_decode_error<base64   >(to_text("========"        ), decode_error_pad_length{  8 }));
static_assert(test_decode_error<base64url>(to_text("AAA====="        ), decode_error_pad_length{  5 }));
static_assert(test_decode_error<base64url>(to_text("AA======"        ), decode_error_pad_length{  6 }));
static_assert(test_decode_error<base64url>(to_text("A======="        ), decode_error_pad_length{  7 }));
static_assert(test_decode_error<base64url>(to_text("========"        ), decode_error_pad_length{  8 }));
static_assert(test_decode_error<base32   >(to_text("AAAAAAA========="), decode_error_pad_length{  9 }));
static_assert(test_decode_error<base32   >(to_text("AAAAAA=========="), decode_error_pad_length{ 10 }));
static_assert(test_decode_error<base32   >(to_text("A==============="), decode_error_pad_length{ 15 }));
static_assert(test_decode_error<base32   >(to_text("================"), decode_error_pad_length{ 16 }));
static_assert(test_decode_error<base32hex>(to_text("AAAAAAA========="), decode_error_pad_length{  9 }));
static_assert(test_decode_error<base32hex>(to_text("AAAAAA=========="), decode_error_pad_length{ 10 }));
static_assert(test_decode_error<base32hex>(to_text("A==============="), decode_error_pad_length{ 15 }));
static_assert(test_decode_error<base32hex>(to_text("================"), decode_error_pad_length{ 16 }));


// Validate that non-canonical padding (non-zero trailing bits) is rejected.
// Base64: 'R' = 17 = 010001, low 4 bits are 0001 (non-zero), vs 'Q' = 16 = 010000 (canonical).
static_assert(test_decode_error<base64   >(to_text("AR=="), decode_error_non_canonical{ 1 }));
static_assert(test_decode_error<base64url>(to_text("AR=="), decode_error_non_canonical{ 1 }));
// Base64: 'B' = 1 = 000001, low 2 bits are 01 (non-zero), vs 'A' = 0 (canonical).
static_assert(test_decode_error<base64   >(to_text("AAB="), decode_error_non_canonical{ 2 }));
static_assert(test_decode_error<base64url>(to_text("AAB="), decode_error_non_canonical{ 2 }));
// Base32: 'B' = 1 = 00001, low 3 bits in the pad-adjacent position (6 pads).
static_assert(test_decode_error<base32   >(to_text("AB======"), decode_error_non_canonical{ 1 }));
static_assert(test_decode_error<base32hex>(to_text("01======"), decode_error_non_canonical{ 1 }));
// Base32 non-canonical: 4 pad chars (2 data bytes, 4 extra bits).
static_assert(test_decode_error<base32   >(to_text("AAAB===="), decode_error_non_canonical{ 3 }));
static_assert(test_decode_error<base32hex>(to_text("0001===="), decode_error_non_canonical{ 3 }));
// Base32 non-canonical: 3 pad chars (3 data bytes, 1 extra bit).
static_assert(test_decode_error<base32   >(to_text("AAAAB==="), decode_error_non_canonical{ 4 }));
static_assert(test_decode_error<base32hex>(to_text("00001==="), decode_error_non_canonical{ 4 }));
// Base32 non-canonical: 1 pad char (4 data bytes, 3 extra bits).
static_assert(test_decode_error<base32   >(to_text("AAAAAAB="), decode_error_non_canonical{ 6 }));
static_assert(test_decode_error<base32hex>(to_text("0000001="), decode_error_non_canonical{ 6 }));

// Boundary byte values: all-zeros and all-ones, single byte.
static_assert(test_encoding<base64   >(to_bytes(0x00), to_text("AA=="    )));
static_assert(test_encoding<base64url>(to_bytes(0x00), to_text("AA=="    )));
static_assert(test_encoding<base32   >(to_bytes(0x00), to_text("AA======")));
static_assert(test_encoding<base32hex>(to_bytes(0x00), to_text("00======")));
static_assert(test_encoding<base16   >(to_bytes(0x00), to_text("00"      )));
static_assert(test_encoding<base64   >(to_bytes(0xFF), to_text("/w=="    )));
static_assert(test_encoding<base64url>(to_bytes(0xFF), to_text("_w=="    )));
static_assert(test_encoding<base32   >(to_bytes(0xFF), to_text("74======")));
static_assert(test_encoding<base32hex>(to_bytes(0xFF), to_text("VS======")));
static_assert(test_encoding<base16   >(to_bytes(0xFF), to_text("FF"      )));

// Boundary byte values: all-zeros and all-ones, full block (no padding).
static_assert(test_encoding<base64   >(to_bytes(0x00, 0x00, 0x00),                   to_text("AAAA"    )));
static_assert(test_encoding<base64   >(to_bytes(0xFF, 0xFF, 0xFF),                   to_text("////"    )));
static_assert(test_encoding<base32   >(to_bytes(0x00, 0x00, 0x00, 0x00, 0x00),       to_text("AAAAAAAA")));
static_assert(test_encoding<base32   >(to_bytes(0xFF, 0xFF, 0xFF, 0xFF, 0xFF),       to_text("77777777")));

// Custom base2 encoding (binary). No padding required.
constexpr encoding base2(chars<'0', '1'>);

// Custom base4 encoding (quaternary). No padding required.
constexpr encoding base4(chars<'0', '3'>);

// Custom base8 encoding (octal). Padding required.
constexpr encoding base8(chars<'0', '7'>, '=');

// Base2 encoding tests.
static_assert(test_encoding<base2>(to_bytes(            ), to_text(""                )));
static_assert(test_encoding<base2>(to_bytes(0x00        ), to_text("00000000"        )));
static_assert(test_encoding<base2>(to_bytes(0xFF        ), to_text("11111111"        )));
static_assert(test_encoding<base2>(to_bytes(0xA5        ), to_text("10100101"        )));
static_assert(test_encoding<base2>(to_bytes(0x00, 0xFF  ), to_text("0000000011111111")));

// Base4 encoding tests.
static_assert(test_encoding<base4>(to_bytes(            ), to_text(""        )));
static_assert(test_encoding<base4>(to_bytes(0x00        ), to_text("0000"    )));
static_assert(test_encoding<base4>(to_bytes(0xFF        ), to_text("3333"    )));
static_assert(test_encoding<base4>(to_bytes(0xA5        ), to_text("2211"    )));
static_assert(test_encoding<base4>(to_bytes(0x00, 0xFF  ), to_text("00003333")));

// Base8 encoding tests.
static_assert(test_encoding<base8>(to_bytes(                ), to_text(""        )));
static_assert(test_encoding<base8>(to_bytes(0x00            ), to_text("000=====")));
static_assert(test_encoding<base8>(to_bytes(0xFF            ), to_text("776=====")));
static_assert(test_encoding<base8>(to_bytes(0x00, 0x00      ), to_text("000000==")));
static_assert(test_encoding<base8>(to_bytes(0xFF, 0xFF      ), to_text("777774==")));
static_assert(test_encoding<base8>(to_bytes(0x00, 0x00, 0x00), to_text("00000000")));
static_assert(test_encoding<base8>(to_bytes(0xFF, 0xFF, 0xFF), to_text("77777777")));
static_assert(test_encoding<base8>(to_bytes(0x41            ), to_text("202=====")));

// Base8 non-canonical padding.
// 5 pads (1 data byte): 3 data chars = 9 bits, 8 data bits, last char's low 1 bit must be zero.
static_assert(test_decode_error<base8>(to_text("001====="), decode_error_non_canonical{ 2 }));
// 2 pads (2 data bytes): 6 data chars = 18 bits, 16 data bits, last char's low 2 bits must be zero.
static_assert(test_decode_error<base8>(to_text("000001=="), decode_error_non_canonical{ 5 }));

// Base8 invalid pad lengths.
static_assert(test_decode_error<base8>(to_text("========"), decode_error_pad_length{ 8 }));
static_assert(test_decode_error<base8>(to_text("0======="        ), decode_error_pad_length{ 7 }));
static_assert(test_decode_error<base8>(to_text("00======"        ), decode_error_pad_length{ 6 }));
static_assert(test_decode_error<base8>(to_text("0000===="        ), decode_error_pad_length{ 4 }));
static_assert(test_decode_error<base8>(to_text("00000==="        ), decode_error_pad_length{ 3 }));
static_assert(test_decode_error<base8>(to_text("0000000="        ), decode_error_pad_length{ 1 }));

// Pad at position 0 (data character after leading pad).
static_assert(test_decode_error<base64   >(to_text("=AAAAAAA"), decode_error_pad{ 1 }));
static_assert(test_decode_error<base64url>(to_text("=AAAAAAA"), decode_error_pad{ 1 }));
static_assert(test_decode_error<base32   >(to_text("=AAAAAAA"), decode_error_pad{ 1 }));
static_assert(test_decode_error<base32hex>(to_text("=0000000"), decode_error_pad{ 1 }));
static_assert(test_decode_error<base8    >(to_text("=0000000"), decode_error_pad{ 1 }));

// Invalid character error in second block of a multi-block message.
static_assert(test_decode_error<base64   >(to_text("AAAAAAAA" "AA^AAAAA"), decode_error_character{ '^', 10 }));
static_assert(test_decode_error<base64url>(to_text("AAAAAAAA" "AA^AAAAA"), decode_error_character{ '^', 10 }));
static_assert(test_decode_error<base32   >(to_text("AAAAAAAA" "AA^AAAAA"), decode_error_character{ '^', 10 }));
static_assert(test_decode_error<base32hex>(to_text("00000000" "00^00000"), decode_error_character{ '^', 10 }));
static_assert(test_decode_error<base16   >(to_text("AA" "A^"            ), decode_error_character{ '^', 3 }));

// Base16 case sensitivity: lowercase a-f should be rejected.
static_assert(test_decode_error<base16>(to_text("0a"), decode_error_character{ 'a', 1 }));
static_assert(test_decode_error<base16>(to_text("0b"), decode_error_character{ 'b', 1 }));
static_assert(test_decode_error<base16>(to_text("0c"), decode_error_character{ 'c', 1 }));
static_assert(test_decode_error<base16>(to_text("0d"), decode_error_character{ 'd', 1 }));
static_assert(test_decode_error<base16>(to_text("0e"), decode_error_character{ 'e', 1 }));
static_assert(test_decode_error<base16>(to_text("0f"), decode_error_character{ 'f', 1 }));


/// Test that decoder flush with non-block-aligned buffered data produces a message size error.
template <encoding E>
consteval bool test_decoder_flush_partial() noexcept {
    decoder<E, 2> dec;
    // Push one character (not enough for a block).
    auto push_result = dec.push('A');
    if (push_result.has_value()) {
        std::abort(); // Should not complete a block from one char.
    }
    // Flush with partial data.
    auto flush_result = dec.flush();
    if (!flush_result.has_value()) {
        std::abort(); // Should return something (non-empty buffer).
    }
    // The result should be a decode error (message size).
    if (flush_result.value().has_value()) {
        std::abort(); // Should be an error, not success.
    }
    if (flush_result.value().error() != decode_error{decode_error_message_size{ 1, E.block_chars }}) {
        std::abort();
    }
    return true;
}

// Decoder flush with partial block should produce a message size error.
static_assert(test_decoder_flush_partial<base64   >());
static_assert(test_decoder_flush_partial<base64url>());
static_assert(test_decoder_flush_partial<base32   >());
static_assert(test_decoder_flush_partial<base32hex>());
static_assert(test_decoder_flush_partial<base16   >());
static_assert(test_decoder_flush_partial<base8    >());


/// Test that pushing invalid data that completes a block triggers a decode error through the decoder.
template <encoding E, std::size_t NChars>
consteval bool test_decoder_push_error(
    const std::array<char, NChars> text,
    const decode_error expected
) noexcept {
    static_assert(NChars == E.block_chars);
    decoder<E, 1> dec;
    // Push all chars; the last one completes the block.
    for (std::size_t i = 0; i < NChars - 1; ++i) {
        auto result = dec.push(text[i]);
        if (result.has_value()) {
            std::abort(); // Should not fire until block is complete.
        }
    }
    // The last push completes the block and should return a result.
    auto result = dec.push(text[NChars - 1]);
    if (!result.has_value()) {
        std::abort(); // Should return a result.
    }
    if (result.value().has_value()) {
        std::abort(); // Should be an error.
    }
    if (result.value().error() != expected) {
        std::abort();
    }
    return true;
}

// Decoder push completing a block with an invalid character.
static_assert(test_decoder_push_error<base64   >(to_text("AA^A"), decode_error{ decode_error_character{ '^', 2 } }));
static_assert(test_decoder_push_error<base64url>(to_text("AA^A"), decode_error{ decode_error_character{ '^', 2 } }));
static_assert(test_decoder_push_error<base16   >(to_text("A^"  ), decode_error{ decode_error_character{ '^', 1 } }));

// Decoder push completing a block with non-canonical padding.
static_assert(test_decoder_push_error<base64   >(to_text("AR=="), decode_error{ decode_error_non_canonical{ 1 } }));

// Decoder push completing a block that is all padding.
static_assert(test_decoder_push_error<base64   >(to_text("===="), decode_error{ decode_error_pad_length{ 4 } }));


} // End namespace based::test.


} // End namespace based.


#endif // End BASED_TEST_HPP include guard.
