/// Single-header C++23 implementation of the base16, base32, base32hex, base64, and base64url
/// encodings as specified in RFC 4648 - https://datatracker.ietf.org/doc/html/rfc4648.
///
/// Code Features:
///   - Fixed-sized and dynamically-sized encoding and decoding of contiguous memory regions.
///   - Buffered encoding and decoding for non-contiguous memory.
///   - Tables for encoding and decoding generated at compile-time.
///   - Usable at both compile-time and run-time.
///   - Can specify additional arbitrary encodings that follow the basic rules laid out in the RFC.
///   - Cannot specify invalid encodings, since they result in compilation errors.
///   - A single encoding path and decoding path for all encodings, with `if constexpr` used where
///     possible to optimize particular encodings.
///   - No dependencies other than the C++23 standard library.
///   - Can work with or without dynamic memory allocation.
///   - Works with -fno-exceptions.
///   - Works with -fno-rtti.


#ifndef BASED_HPP
#define BASED_HPP


#include <algorithm>
#include <array>
#include <bit>
#include <climits>
#include <cstdint>
#include <expected>
#include <numeric>
#include <span>
#include <utility>
#include <tuple>
#include <variant>


namespace based {


/// Rounds up a number to the nearest multiple of a different number.
///
/// @param number   The number to round up.
/// @param multiple The multiple to round up to.
///
/// @returns `number` rounded up to the nearest multiple of `multiple`.
[[nodiscard]] constexpr std::size_t round_up(std::size_t number, std::size_t multiple) noexcept {
    return ((number + multiple - 1) / multiple) * multiple;
}


/// Concatenate two arrays.
///
/// @tparam T    The type of element that both arrays contain.
/// @tparam NLhs The size of the array on the left-hand side of the equation.
/// @tparam NRhs The size of the array on the right-hand side of the equation.
///
/// @param lhs The first array to concatenate.
/// @param rhs The second array to concatenate.
///
/// @returns The array that results from concatenating lhs and rhs.
template <typename T, std::size_t NLhs, std::size_t NRhs>
[[nodiscard]] consteval std::array<T, NLhs + NRhs> operator+(
    const std::array<T, NLhs>& lhs,
    const std::array<T, NRhs>& rhs
) noexcept(std::is_nothrow_copy_constructible_v<T>) {
    std::array<T, NLhs + NRhs> result;
    std::ranges::copy(lhs, result.begin());
    std::ranges::copy(rhs, result.begin() + NLhs);
    return result;
}


/// An array containing sequential characters.
///
/// @tparam CBegin The character to begin with, inclusive.
/// @tparam CEnd   The character to end with, inclusive. Defaults to CBegin.
template <char CBegin, char CEnd = CBegin>
requires (CBegin <= CEnd)
constexpr std::array<char, CEnd - CBegin + 1> chars = []{
    std::array<char, CEnd - CBegin + 1> result;
    for (std::size_t i = 0; i < result.size(); ++i) {
        result[i] = CBegin + i;
    }
    return result;
}();


/// Constrain the allowed encoding sizes to powers of two between 2 and 64, which is more than
/// sufficient to implement RFC4648, but not base58 or some other schemes.
///
/// @tparam N The number of symbols in the encoding.
template <std::size_t N>
concept valid_encoding_size =
    (std::has_single_bit(N) && N > 1 && N < std::numeric_limits<char>::max());


/// The number of bits each encoded character represents.
///
/// @tparam NEncoding The number of symbols in the encoding.
template <std::size_t NEncoding>
requires valid_encoding_size<NEncoding>
constexpr std::size_t bits_per_char = std::bit_width(NEncoding) - 1;


/// The number of bytes in an encoding block for a given encoding size.
///
/// If this isn't 1, then a padding character will be required for the encoding, since a message end
/// may not align with a block's end.
///
/// @tparam NEncoding The number of symbols in the encoding.
template <std::size_t NEncoding>
constexpr std::size_t block_bytes = std::lcm(bits_per_char<NEncoding>, CHAR_BIT) / CHAR_BIT;


/// The number of characters in an encoding block for a given encoding size.
///
/// Valid encoded messages will always be a multiple of this number of characters.
///
/// @tparam NEncoding The number of symbols in the encoding.
template <std::size_t NEncoding>
constexpr std::size_t block_chars =
    std::lcm(bits_per_char<NEncoding>, CHAR_BIT) / bits_per_char<NEncoding>;


/// The size in characters of a num_bytes-length binary message encoded with a given encoding size.
///
/// @tparam NEncoding The number of symbols in the encoding.
///
/// @param num_bytes The number of bytes in the original message.
///
/// @returns The number of characters in the encoded message.
template <std::size_t NEncoding>
[[nodiscard]] constexpr std::size_t encoded_size(std::size_t num_bytes) noexcept {
    return round_up(CHAR_BIT * num_bytes / bits_per_char<NEncoding>, block_chars<NEncoding>);
}


/// The size in bytes of an num_chars-length text message decoded from an encoding of a given size.
///
/// @tparam NEncoding The number of symbols in the encoding.
///
/// @param num_chars The number of characters in the encoded message.
///
/// @returns The number of characters in the encoded message.
template <std::size_t NEncoding>
[[nodiscard]] constexpr std::size_t decoded_size(std::size_t num_chars) noexcept {
    return round_up(bits_per_char<NEncoding> * num_chars / CHAR_BIT, block_bytes<NEncoding>);
}


/// Whether or not a padding character is required, depending on the encoding size.
///
/// @tparam NEncoding The number of symbols in the encoding.
template <std::size_t NEncoding>
concept needs_pad = block_bytes<NEncoding> != 1;


/// The type of table to use to define an encoding, not including any pad character.
///
/// It's an array where the index is the binary value to encode (between 0 and
/// bits_per_char<NEncoding>], and the value at that index is the encoded symbol representing
/// that value.
///
/// For example, the following is the encode table for base64:
///
///     Index|0000000000111111111122222222223333333333444444444455555555556666
///     Index|0123456789012345678901234567890123456789012345678901234567890123
///     -----+----------------------------------------------------------------
///     Value|ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/
///
/// @tparam NEncoding The number of symbols in the encoding.
template <std::size_t NEncoding>
using encode_table_array = std::array<char, NEncoding>;


/// The types of result that can arise when decoding a single character from an encoded string.
enum class char_decode_result_type {
    /// The encoded character represented a valid value.
    value,
    /// The encoded character was the padding character.
    pad,
    /// The encoded character was not a valid character for the encoding.
    invalid
};


/// The result of decoding a single character from an encoded string.
///
/// Something like a `variant<std::byte, pad, invalid>` would be better to use here, but since
/// these are contained by the `encoding` class below, and that class is used as a non-type
/// template parameter, we need to use a less sophisticated type here.
struct char_decode_result {
    /// The type of the result of decoding a single character. Basically the union discriminator.
    char_decode_result_type type = char_decode_result_type::invalid;

    /// The value extracted in the decoding. This is only valid if `type` is `value`.
    std::byte value = std::byte{0xfe};
};


/// The table used to decode encoded text.
///
/// It's an array that has an index for every possible byte. Each byte can map to one of the
/// above `decode_result`s, i.e. a value, a pad character, or an invalid character.
using char_decode_result_array =
    std::array<char_decode_result, std::numeric_limits<std::uint8_t>::max()>;


/// Type returned in the event that a buffer does not have a valid size.
struct encode_error_buffer_size {
    /// The size of the invalid buffer.
    std::size_t buffer_size;

    /// The expected minimum size of the buffer.
    std::size_t expected_size;

    /// Allow comparisons of these objects.
    constexpr std::strong_ordering operator<=>(
        const encode_error_buffer_size& other) const noexcept = default;
};


/// Type returned in the event of a successful decoding.
struct decode_success {
    /// The number of padding bytes in the original message.
    std::size_t pad_bytes;

    /// Allow comparisons of these objects.
    constexpr std::strong_ordering operator<=>(
        const decode_success& other) const noexcept = default;
};


/// Type returned in the event that a message does not have a valid size.
struct decode_error_message_size {
    /// The size of the invalid message.
    std::size_t message_size;

    /// The expected block-size that the message violated.
    std::size_t block_chars;

    /// Allow comparisons of these objects.
    constexpr std::strong_ordering operator<=>(
        const decode_error_message_size& other) const noexcept = default;
};


/// Type returned in the event that a destination buffer does not have enough space.
struct decode_error_buffer_size {
    /// The size of the invalid message.
    std::size_t buffer_size;

    /// The expected minimum size of the buffer.
    std::size_t expected_size;

    /// Allow comparisons of these objects.
    constexpr std::strong_ordering operator<=>(
        const decode_error_buffer_size& other) const noexcept = default;
};


/// Type returned in the event that a message has an invalid character for the encoding.
struct decode_error_character {
    /// The invalid character that was in the message.
    char character;

    /// The character index within the original message where the invalid character was found.
    std::size_t index;

    /// Allow comparisons of these objects.
    constexpr std::strong_ordering operator<=>(
        const decode_error_character& other) const noexcept = default;
};


/// Type returned in the event that a message has invalid padding.
struct decode_error_pad {
    /// The location of the invalid character which follows a padding character.
    std::size_t index;

    /// Allow comparisons of these objects.
    constexpr std::strong_ordering operator<=>(
        const decode_error_pad& other) const noexcept = default;
};


/// Type returned in the event that a message has invalid padding.
struct decode_error_pad_length {
    /// The illegal padding length in characters in the encoded string.
    std::size_t length;

    /// Allow comparisons of these objects.
    constexpr std::strong_ordering operator<=>(
        const decode_error_pad_length& other) const noexcept = default;
};


/// Type that can represent any decoding error.
using decode_error = std::variant<
    decode_error_message_size,
    decode_error_buffer_size,
    decode_error_character,
    decode_error_pad,
    decode_error_pad_length
>;


/// Constructs a decoding table from an encoding table.
///
/// @tparam NEncoding The number of symbols in the encoding.
///
/// @param encode_table The encoding table from which the decoding table should be constructed.
///
/// @returns The decoding table derived from the given encoding table.
///
/// @note This function calls `std::abort`, but it is `consteval` so it cannot result in a runtime
///       abort.
template <std::size_t NEncoding>
requires valid_encoding_size<NEncoding>
[[nodiscard]] consteval char_decode_result_array make_decode_table(
    encode_table_array<NEncoding> encode_table
) noexcept {
    char_decode_result_array decode_table;
    for (std::uint8_t symbol_index = 0; symbol_index < encode_table.size(); ++symbol_index) {
        if (decode_table[encode_table[symbol_index]].type != char_decode_result_type::invalid) {
            std::abort(); // Duplicate character.
        }
        decode_table[encode_table[symbol_index]].type = char_decode_result_type::value;
        decode_table[encode_table[symbol_index]].value = std::byte{symbol_index};
    }
    return decode_table;
}


/// Constructs a decoding table from an encoding table, including a pad character.
///
/// @tparam NEncoding The number of symbols in the encoding.
///
/// @param encode_table The encoding table from which the decoding table should be constructed.
///
/// @returns The decoding table derived from the given encoding table.
/// 
/// @note This function calls `std::abort`, but it is `consteval` so it cannot result in a runtime
///       abort.
template <std::size_t NEncoding>
requires valid_encoding_size<NEncoding>
[[nodiscard]] consteval char_decode_result_array make_decode_table(
    encode_table_array<NEncoding> encode_table,
    char pad_char
) noexcept requires (needs_pad<NEncoding>) {
    char_decode_result_array decode_table = make_decode_table(encode_table);
    if (decode_table[pad_char].type != char_decode_result_type::invalid) {
        std::abort(); // Duplicate character.
    }
    decode_table[pad_char].type = char_decode_result_type::pad;
    return decode_table;
}


/// Package up all information about a particular encoding. It is ensured that the encoding is valid
/// at compile-time -- it has a valid size, there are no duplicate characters, and a padding
/// character was provided, if required. It should hopefully not be possible to produce a compiling
/// example of an invalid encoding using this struct.
///
/// This struct can help in writing encoders and decoders of different types.
///
/// This is used as a non-type template parameter, making it easy to specialize things based on a
/// particular encoding. That is powerful, but as such this struct has many restrictions placed on
/// it, and convenient classes like std::array and std::variant cannot be members.
///
/// @tparam NEncoding The number of symbols in the encoding.
template <std::size_t NEncoding>
struct encoding {
    /// The number of symbols in the encoding not including any pad character.
    static constexpr std::size_t size = NEncoding;

    /// The number of bytes required for one encoding block.
    static constexpr std::size_t block_bytes = block_bytes<NEncoding>;

    /// The number of encoded characters required for one encoding block.
    static constexpr std::size_t block_chars = block_chars<NEncoding>;

    /// The number of bits each encoded character represents.
    static constexpr std::size_t bits_per_char = bits_per_char<NEncoding>;

    // A bitmask that is bits_per_char bits long starting in the least-significant position.
    static constexpr std::byte mask = std::byte{size - 1};

    /// Whether or not the encoding may require padding character(s) at the end.
    static constexpr bool needs_pad = needs_pad<NEncoding>;

    /// The size in characters of the result of encoding num_bytes bytes of data.
    static constexpr std::size_t encoded_chars(std::size_t num_bytes) noexcept {
        return encoded_size<NEncoding>(num_bytes);
    }

    /// The size in bytes of the result of decoding num_chars chars of data.
    /// Note, this is the maximum size. Due to padding the actual size may be less.
    static constexpr std::size_t decoded_bytes(std::size_t num_chars) noexcept {
        return decoded_size<NEncoding>(num_chars);
    }

    /// The type of table used to construct the encoding.
    using encode_table_type = encode_table_array<NEncoding>;

    /// Constructor for encodings that don't require padding.
    constexpr encoding(encode_table_type table) noexcept requires (!needs_pad)
        : pad_char(std::monostate{}) {
        std::ranges::copy(table, encode_table);
        std::ranges::copy(make_decode_table(table), decode_table);
    }

    /// Constructor for encodings that do require padding.
    constexpr encoding(encode_table_type table, char pad_char_value) noexcept requires (needs_pad)
        : pad_char(pad_char_value) {
        std::ranges::copy(table, encode_table);
        std::ranges::copy(make_decode_table(table, pad_char), decode_table);
    }

    /// Table used for encoding. Using a bits_per_char binary value as an index, the value is the
    /// encoded character. Must be raw array because std::array can't be used in non-type template
    /// parameters.
    char encode_table[NEncoding];

    /// Table used for decoding. Using an encoded character as an index, the value is the decoded
    /// bits_per_char binary value. Must be raw array because std::array can't be used in non-type
    /// template parameters.
    char_decode_result decode_table[std::numeric_limits<std::uint8_t>::max()];

    /// The padding character (if applicable to this encoding.)
    const std::conditional_t<needs_pad, char, std::monostate> pad_char;
};

// Negative compilation tests, to ensure that invalid encodings result in compilation errors.
// constexpr encoding bad_size(chars<'0', '9'> + chars<'A', 'E'>);
// constexpr encoding bad_repeat(chars<'0'> + chars<'0', '9'> + chars<'A', 'E'>);
// constexpr encoding bad_pad_given(chars<'0', '9'> + chars<'A', 'F'>, '=');
// constexpr encoding bad_pad_omitted(chars<'0', '9'> + chars<'A', 'V'>);
// constexpr encoding bad_pad_repeat(chars<'='> + chars<'1', '9'> + chars<'A', 'V'>, '=');


/// The unsafe base encoding algorithm. This is the main encoding logic.
///
/// @tparam E         The encoding to use.
/// @tparam NSizeHint Avoids calculating the encoded size if it is already known at compile-time.
///
/// @param destination   The memory to which the encoded string should be written.
/// @param source        The memory to encode.
///
/// @pre The caller has validated that the destination is large enough to contain
///      `E.encoded_chars(source.size())` characters. This is _not_ checked here.
/// @post The `destination` contains the encoded `source`.
template <encoding E, std::size_t NSizeHint = 0>
constexpr void encode_unchecked(
    const std::span<char> destination,
    const std::span<const std::byte> source
) noexcept {
    // Determine the number of characters we need to encode.
    std::size_t encoded_chars = NSizeHint;
    if constexpr (NSizeHint == 0) {
        encoded_chars = E.encoded_chars(source.size());
    }

    // Encode the data character by character by extracting bits from the source.
    for (std::size_t i = 0; i < encoded_chars; ++i) {
        // Calculate properties about the location of the bits to extract.
        const std::size_t bit_start    = i * E.bits_per_char;
        const std::size_t bit_end      = bit_start + E.bits_per_char - 1;
        const std::size_t byte_start   = bit_start / CHAR_BIT;
        const std::size_t byte_end     = bit_end / CHAR_BIT;
        const std::size_t offset_start = bit_start % CHAR_BIT;
        const std::size_t offset_end   = bit_end % CHAR_BIT;

        // Encode a single character. If the encoding requires no padding, which is known at
        // compile-time, then the data to extract is always within a single byte, and we don't need
        // to check if we're beyond the source buffer. If the encoding requires padding then we
        // first need to check if we're beyond the source buffer, and if so then the encoded
        // character is the padding character. Otherwise, there is some data to extract. There are
        // two possibilities, the data to extract is in a single byte or it is spread across two
        // bytes. The first (or only) byte is guaranteed to exist, but the second byte may not. If
        // the second byte does not exist, then zero should be used in its place.
        if constexpr (!E.needs_pad) {
            const std::size_t shift = CHAR_BIT - E.bits_per_char - offset_start;
            const std::byte value = (source[byte_start] & (E.mask << shift)) >> shift;
            destination[i] = E.encode_table[std::to_underlying(value)];
        } else if (byte_start >= source.size()) {
            destination[i] = E.pad_char;
        } else if (byte_start == byte_end) {
            const std::size_t shift = CHAR_BIT - E.bits_per_char - offset_start;
            const std::byte value = (source[byte_start] & (E.mask << shift)) >> shift;
            destination[i] = E.encode_table[std::to_underlying(value)];
        } else {
            const std::size_t upper_shift = E.bits_per_char - (CHAR_BIT - offset_start);
            const std::size_t lower_shift = CHAR_BIT - offset_end - 1;
            const std::byte upper = (source[byte_start] & (E.mask >> upper_shift)) << upper_shift;
            const std::byte lower = byte_end >= source.size() ?
                std::byte{0} : (source[byte_end] & (E.mask << lower_shift)) >> lower_shift;
            destination[i] = E.encode_table[std::to_underlying(upper | lower)];
        }
    }
}


/// Convert binary data to a text encoding. Fixed-size, unowned memory overload.
///
/// @tparam E      The encoding to use.
/// @tparam NBytes The number of bytes in the message to encode.
///
/// @param destination   The memory to which the encoded string should be written.
/// @param source        The memory to encode.
///
/// @post The `destination` contains the encoded `source`.
template <encoding E, std::size_t NBytes, std::size_t NChars>
requires (NBytes != std::dynamic_extent && NChars >= E.encoded_chars(NBytes))
constexpr void encode(
    const std::span<char, NChars> destination,
    const std::span<const std::byte, NBytes> source
) noexcept {
    encode_unchecked<E, E.encoded_chars(NBytes)>(destination, source);
}


/// Convert binary data to a text encoding. Fixed-size, stack-memory overload.
///
/// @tparam E          The encoding to use.
/// @tparam TContainer The type of fixed-size container to return.
/// @tparam NBytes     The number of bytes in the message to encode.
///
/// @param source The memory to encode.
///
/// @returns A `TContainer` containing the encoded `source` data.
///
/// @throws May throw if the container's constructor throws; however, this is often not the case
///         for sized containers of characters, including std::array.
///
/// @note `TContainer` can be `std::array` and similar containers.
template <encoding E, template <typename, std::size_t> class TContainer, std::size_t NBytes>
requires (NBytes != std::dynamic_extent)
[[nodiscard]] constexpr TContainer<char, E.encoded_chars(NBytes)> encode(
    const std::span<const std::byte, NBytes> source
) noexcept(std::is_nothrow_constructible_v<TContainer<char, E.encoded_chars(NBytes)>>) {
    TContainer<char, E.encoded_chars(NBytes)> encoded;
    encode_unchecked<E, E.encoded_chars(NBytes)>(std::span<char>{encoded}, source);
    return encoded;
}


/// Convert binary data to a text encoding. Dynamically-sized, unowned memory overload.
///
/// @tparam E The encoding to use.
///
/// @param destination   The memory to which the encoded string should be written.
/// @param source        The memory to encode.
///
/// @returns True if the encoding succeeded, false otherwise. The only reason for failure
///          is if the destination buffer is too small.
///
/// @post If the function returns true, the `destination` contains the encoded `source`.
///       If the function returns false, the `destination` buffer is untouched.
template <encoding E>
[[nodiscard]] constexpr std::expected<void, encode_error_buffer_size> encode(
    const std::span<char> destination,
    const std::span<const std::byte> source
) noexcept {
    if (destination.size() < E.encoded_chars(source.size())) {
        return std::unexpected(
            encode_error_buffer_size{ destination.size(),  E.encoded_chars(source.size()) });
    }
    encode_unchecked<E>(destination, source);
    return std::expected<void, encode_error_buffer_size>{};
}


/// Convert binary data to a text encoding. Dynamically-sized, heap-memory overload.
///
/// @tparam E          The encoding to use.
/// @tparam TContainer The type of container to return.
///
/// @param source The memory to encode.
///
/// @throws May throw if the container's constructor throws, which it likely does since
///         it typically requires heap-memory allocations.
///
/// @note `TContainer` can be `std::basic_string`, `std::vector`, and similar containers.
template <encoding E, template <typename> class TContainer>
[[nodiscard]] constexpr TContainer<char> encode(
    const std::span<const std::byte> source
) noexcept(std::is_nothrow_constructible_v<TContainer<char>, std::size_t, char>) {
    TContainer<char> encoded(E.encoded_chars(source.size()), '\0');
    encode_unchecked<E>(std::span<char>{encoded}, source);
    return encoded;
}


/// A class that assits in encoding binary data to text byte-by-byte, without needing to keep the
/// entire message or encoded text in memory all at once. A buffer of some number of encoding blocks
/// is kept. Once the buffer fills up, an equivalent block of encoded text is returned.
///
/// In the future, an interface that allows pushing chunks of data instead of single bytes would be
/// good, but that is omitted for now.
///
/// @tparam E       The encoding to use.
/// @tparam NBlocks The number of blocks of data to keep before encoding.
template <encoding E, std::size_t NBlocks = 1>
class encoder {
public:
    /// The size in bytes of the data buffer.
    static constexpr std::size_t buffer_bytes = NBlocks * E.block_bytes;

    /// The size in characters of the returned text buffers.
    static constexpr std::size_t encoded_chars = NBlocks * E.block_chars;

    /// The result type returned when a block of data is completed.
    using text_block = std::array<char, encoded_chars>;

    /// Initializes the encoder, ready to accept data.
    constexpr encoder() noexcept
        : data_cursor(data.begin()) {
    }

    /// Copying an encoder simply copies the full state and recreates the iterator.
    constexpr encoder(const encoder& other) noexcept
        : data(other.data),
          data_cursor(data.begin() + (other.data_cursor - other.data.begin())) {
    }

    /// Adds a new byte of data to the encoder.
    ///
    /// @param input The byte of data to add to the encoder.
    ///
    /// @returns An optional text block, returned when a data block has been completed.
    [[nodiscard]] constexpr std::optional<text_block> push(std::byte input) noexcept {
        *data_cursor++ = input;
        if constexpr (buffer_bytes > 1) {
            if (data_cursor == data.end()) {
                data_cursor = data.begin();
                return encode<E, std::array>(std::span<const std::byte, buffer_bytes>{data});
            }
            return std::nullopt;
        } else {
            data_cursor = data.begin();
            return encode<E, std::array>(std::span<const std::byte, buffer_bytes>{data});
        }
    }

    /// Flushes any data currently in the encoder without adding any new data.
    ///
    /// @returns An optional text block, if an in-progress data block was completed.
    ///
    /// @note The encoder can be reused after this to encode another message.
    [[nodiscard]] constexpr std::optional<std::tuple<text_block, std::size_t>> flush() noexcept {
        if constexpr (buffer_bytes > 1) {
            if (data_cursor != data.begin()) {
                std::span<const std::byte> data_span(data.begin(), data_cursor - data.begin());
                text_block text;
                encode_unchecked<E>(std::span<char>{text}, data_span);
                return std::make_tuple(text, E.encoded_chars(data_span.size()));
            }
        }
        return std::nullopt;
    }

private:
    /// The type used to store the data buffer.
    using data_buffer = std::array<std::byte, buffer_bytes>;

    /// The buffered data.
    data_buffer data{};

    /// The location within the data buffer where new data should be added.
    data_buffer::iterator data_cursor;
};


/// The unsafe base decoding algorithm. This is the main decoding logic.
///
/// @tparam E The encoding to use.
///
/// @param destination The memory to which the decoded data should be written.
/// @param source      The text to decode.
///
/// @returns An expected decode_success representing the result of the decoding operation, with
///          a decode_error being returned in the error case.
///
/// @pre The source buffer is known to be a multple of the encoding's block characters size.
/// @pre The destination buffer is known to be at least large enough to contain the decoded source.
/// @pre The destination buffer contains all zeroes data. If the destination buffer does not contain
///      zeroes, then the result will actually be the bitwise-or with the data that's already there.
///
/// @post The `destination` contains the decoded `source`.
template <encoding E, std::size_t NSizeHint = 0>
[[nodiscard]] constexpr std::expected<decode_success, decode_error> decode_unchecked(
    const std::span<std::byte> destination,
    const std::span<const char> source
) noexcept {
    // Determine the number of characters we need to decode.
    std::size_t decoded_bytes = NSizeHint;
    if constexpr (NSizeHint == 0) {
        decoded_bytes = E.decoded_bytes(source.size());
    }

    // Decode the data character by character.
    std::size_t pad_count = 0;
    for (std::size_t i = 0; i < source.size(); ++i) {
        const char_decode_result result = E.decode_table[static_cast<unsigned char>(source[i])];
        if (result.type == char_decode_result_type::invalid) {
            return std::unexpected(decode_error_character{ source[i] , i });
        } else if (result.type == char_decode_result_type::pad) {
            ++pad_count;
        } else if (pad_count) {
            // This indicates that we found a data character after seeing a padding character, which
            // is not valid. Padding characters must be found only at the end of an encoded string.
            return std::unexpected(decode_error_pad{ i });
        } else {
            // Calculate properties about the location where the decoded bits will be placed.
            const std::size_t bit_start    = i * E.bits_per_char;
            const std::size_t bit_end      = bit_start + E.bits_per_char - 1;
            const std::size_t byte_start   = bit_start / CHAR_BIT;
            const std::size_t byte_end     = bit_end / CHAR_BIT;
            const std::size_t offset_start = bit_start % CHAR_BIT;
            const std::size_t offset_end   = bit_end % CHAR_BIT;

            // If the encoding doesn't use padding then we know that the decoded character's value
            // fits in exactly one byte. If it doesn't, then we need to check if the decoded bits
            // fit in exactly one byte or are split across two bytes.
            if constexpr (!E.needs_pad) {
                destination[byte_start] |=
                    result.value << (CHAR_BIT - (E.bits_per_char + offset_start));
            } else if (byte_start == byte_end) {
                destination[byte_start] |=
                    result.value << (CHAR_BIT - (E.bits_per_char + offset_start));
            } else {
                destination[byte_start] |=
                    result.value >> (E.bits_per_char - (CHAR_BIT - offset_start));
                destination[byte_end] |= result.value << (CHAR_BIT - offset_end - 1);
            }
        }
    }

    if constexpr (E.needs_pad) {
        if (pad_count) {
            if (pad_count >= E.block_chars) {
                return std::unexpected(decode_error_pad_length{ pad_count });
	    }

            // If adding another character to the pad doesn't cross a byte boundary, it's an invalid
            // padding length.
            const std::size_t pad_bits  = E.bits_per_char * pad_count;
            const std::size_t pad_byte  = pad_bits / CHAR_BIT;
            const std::size_t next_byte = (pad_bits + E.bits_per_char) / CHAR_BIT;
            if (next_byte == pad_byte || next_byte % E.block_bytes == 0) {
                return std::unexpected(decode_error_pad_length{ pad_count });
            } else {
                return decode_success { (E.bits_per_char * pad_count + CHAR_BIT - 1) / CHAR_BIT };
            }
        } else {
            return decode_success { 0 };
        }
    } else {
        return decode_success { 0 };
    }
}


/// Convert text-encoded data back into binary data. Fixed-size, unowned memory overload.
///
/// @tparam E      The encoding to use.
/// @tparam NChars The number of characters in the encoded message.
///
/// @param destination   The memory to which the decoded data should be written.
/// @param source        The memory containing the text to decode.
///
/// @post The `destination` contains the decoded `source`.
template <encoding E, std::size_t NChars, std::size_t NBytes>
requires (
    NChars != std::dynamic_extent &&
    NChars % E.block_chars == 0 &&
    NBytes >= E.decoded_bytes(NChars)
)
[[nodiscard]] constexpr std::expected<decode_success, decode_error> decode(
    const std::span<std::byte, NBytes> destination,
    const std::span<const char, NChars> source
) noexcept {
    return decode_unchecked<E>(destination, source);
}


/// Convert text-encoded data back into binary. Fixed-size, stack-memory overload.
///
/// @tparam E          The encoding to use.
/// @tparam TContainer The type of fixed-size container to return.
/// @tparam NChars     The number of characters in the encoded message.
///
/// @param source The memory containing the text to decode.
///
/// @returns A `TContainer` containing the decoded `source` text.
///
/// @throws May throw if the container's constructor throws; however, this is often not the case
///         for sized containers of characters, including `std::array`.
///
/// @note `TContainer` can be `std::array` and similar containers.
template <encoding E, template <typename, std::size_t> class TContainer, std::size_t NChars>
requires (NChars != std::dynamic_extent && NChars % E.block_chars == 0)
[[nodiscard]] constexpr
std::expected<
    std::tuple<TContainer<std::byte, E.decoded_bytes(NChars)>, decode_success>,
    decode_error
>
decode(
    const std::span<const char, NChars> source
) noexcept(std::is_nothrow_constructible_v<TContainer<std::byte, E.decoded_bytes(NChars)>>) {
    using return_type = std::expected<
        std::tuple<TContainer<std::byte, E.decoded_bytes(NChars)>, decode_success>, decode_error>;
    TContainer<std::byte, E.decoded_bytes(NChars)> decoded;
    std::ranges::fill(decoded, std::byte{0});
    return decode_unchecked<E>(std::span<std::byte>{decoded}, source)
        .and_then([&decoded](decode_success success) { 
            return return_type{std::make_tuple(decoded, success)};
        })
        .or_else([](decode_error error) {
            return return_type{std::unexpected(error)};
        });
}


/// Convert text-encoded data back into binary. Dynamically-sized, unowned memory overload.
///
/// @tparam E The encoding to use.
///
/// @param destination The memory to which the encoded string should be written.
/// @param source      The memory to encode.
///
/// @returns True if the encoding succeeded, false otherwise. The only reason for failure
///          is if the destination buffer is too small.
///
/// @post If the function returns true, the `destination` contains the encoded `source`.
///       If the function returns false, the `destination` buffer is untouched.
template <encoding E>
[[nodiscard]] constexpr std::expected<decode_success, decode_error> decode(
    const std::span<std::byte> destination,
    const std::span<const char> source
) noexcept {
    if (source.size() % E.block_chars != 0) {
        return std::unexpected(decode_error_message_size{ source.size(), E.block_chars });
    }

    if (destination.size() < E.decoded_bytes(source.size())) {
        return std::unexpected(
            decode_error_buffer_size{ destination.size(), E.decoded_bytes(source.size()) });
    }

    return decode_unchecked<E>(destination, source);
}


/// Convert binary data to a text encoding. Dynamically-sized, heap-memory overload.
///
/// @tparam E          The encoding to use.
/// @tparam TContainer The type of container to return.
///
/// @param source The memory to encode.
///
/// @throws May throw if the container's constructor throws, or resizing the container throws, which
///         it likely does since both may typically require heap-memory allocations.
///
/// @note `TContainer` can be `std::vector` and similar containers.
template <encoding E, template <typename> class TContainer>
[[nodiscard]] constexpr std::expected<TContainer<std::byte>, decode_error> decode(
    const std::span<const char> source
) noexcept(
    std::is_nothrow_constructible_v<TContainer<char>, std::size_t, char> &&
    // This monstrosity essentially asks the question "does container.resize(std::size_t) throw?"
    // The cast is required since the "resize" function might be overloaded, and we need to ensure
    // the overload is selected. Even though "std::size_t" is specified as the argument, it needs to
    // be disambiguated ahead of time.
    std::is_nothrow_invocable_v<
        decltype(static_cast<void(TContainer<char>::*)(std::size_t)>(&TContainer<char>::resize)),
        std::size_t
    >
) {
    if (source.size() % E.block_chars != 0) {
        return std::unexpected(decode_error_message_size{ source.size(), E.block_chars });
    }

    TContainer<std::byte> decoded(E.decoded_bytes(source.size()), std::byte{0});
    return decode_unchecked<E>(std::span<std::byte>{decoded}, source)
        .and_then([&decoded](decode_success success) {
            if constexpr (E.needs_pad) {
                if (success.pad_bytes) {
                    decoded.resize(decoded.size() - success.pad_bytes);
                }
            }
            return std::expected<TContainer<std::byte>, decode_error>(decoded);
        })
        .or_else([](decode_error error) {
            return std::expected<TContainer<std::byte>, decode_error>(std::unexpected(error));
        });
}


/// A class that assits in decoding text back into binary, without needing to keep the entire
/// message or encoded text in memory all at once. A buffer of some number of encoding blocks is
/// kept. Once the buffer fills up, an equivalent block of decoded data is returned.
///
/// In the future, an interface that allows pushing chunks of text instead of single chars would be
/// good, but that is omitted for now.
///
/// @tparam E       The encoding to use.
/// @tparam NBlocks The number of blocks of data to keep before encoding.
template <encoding E, std::size_t NBlocks = 1>
class decoder {
public:
    /// The size in bytes of the data buffer.
    static constexpr std::size_t data_bytes = NBlocks * E.block_bytes;

    /// The size in characters of the returned text buffers.
    static constexpr std::size_t buffer_chars = NBlocks * E.block_chars;

    /// The block of data returned when a block of text is successfully decoded.
    using data_block = std::array<std::byte, data_bytes>;

    /// The result type returned when a block of data is completed.
    using result_type = std::expected<std::tuple<data_block, decode_success>, decode_error>;

    /// Initializes the encoder, ready to accept data.
    constexpr decoder() noexcept
        : text_cursor(text.begin()) {
    }

    /// Copying a decoder simply copies the full state and recreates the iterator.
    constexpr decoder(const decoder& other) noexcept
        : text(other.text),
          text_cursor(text.begin() + (other.text_cursor - other.text.begin())) {
    }

    /// Adds a new byte of data to the encoder.
    ///
    /// @param input The byte of data to add to the encoder.
    ///
    /// @returns An optional text block, returned when a data block has been completed.
    [[nodiscard]] constexpr std::optional<result_type> push(char input) noexcept {
        *text_cursor++ = input;
        if (text_cursor == text.end()) {
            text_cursor = text.begin();
            return decode<E, std::array>(std::span<const char, buffer_chars>{text});
        }
        return std::nullopt;
    }

    /// Flushes any data currently in the decoder without adding any new data.
    ///
    /// @returns An optional data block, if an in-progress data block was completed.
    ///
    /// @note The decoder can be reused after this to decode another message.
    [[nodiscard]] constexpr std::optional<result_type> flush() noexcept {
        if (text_cursor != text.begin()) {
            const std::size_t encoded_size = text_cursor - text.begin();
            const std::span<const char> text_span(text.begin(), encoded_size);

            data_block data;
            data.fill(std::byte{0});
            const std::expected<decode_success, decode_error> result = 
                decode_unchecked<E>(std::span<std::byte>{data}, text_span);

            if (result.has_value()) {
                return std::make_tuple(data, decode_success{
                    result.value().pad_bytes + data_bytes - E.decoded_bytes(encoded_size)
                });
            } else {
                return std::unexpected(result.error());
            }
        }
        return std::nullopt;
    }

private:
    /// The type used to store the text buffer.
    using text_buffer = std::array<char, buffer_chars>;

    /// The buffered text.
    text_buffer text{};

    /// The location within the text buffer where new text should be added.
    text_buffer::iterator text_cursor;
};


// Define all of the encodings from RFC 4648.
// These can be used as a guide for defining your own encoding if needed.
constexpr encoding base64(
    chars<'A', 'Z'> + chars<'a', 'z'> + chars<'0', '9'> + chars<'+'> + chars<'/'>, '=');
constexpr encoding base64url(
    chars<'A', 'Z'> + chars<'a', 'z'> + chars<'0', '9'> + chars<'-'> + chars<'_'>, '=');
constexpr encoding base32(chars<'A', 'Z'> + chars<'2', '7'>, '=');
constexpr encoding base32hex(chars<'0', '9'> + chars<'A', 'V'>, '=');
constexpr encoding base16(chars<'0', '9'> + chars<'A', 'F'>);


} // End namespace based.


#endif // End BASED_HPP include guard.
