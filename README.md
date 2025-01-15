# based

Single-header C++23 implementation of the base16, base32, base32hex, base64, and base64url encodings as specified in [RFC 4648](https://datatracker.ietf.org/doc/html/rfc4648).

- Fixed-sized and dynamically-sized encoding and decoding of contiguous memory regions.
- Buffered encoding and decoding for non-contiguous memory.
- Tables for encoding and decoding generated at compile-time.
- Fully usable at both compile-time and run-time.
- Can specify additional arbitrary encodings that follow the basic rules laid out in the RFC.
- Cannot specify invalid encodings, since they result in compilation errors.
- A single encoding path and decoding path for all encodings, with `if constexpr` used where possible to optimize particular encodings.
- No dependencies other than the C++23 standard library.
- Can work with or without dynamic memory allocation.
- Works with `-fno-exceptions`.
- Works with `-fno-rtti`.
- Numerous `static_assert` checks to ensure validity and document correct usage. Aims to achieve 100% code coverage of the library at compile-time, but it's difficult to prove this with current tooling.

## Overview

Two functionally identical versions of the library are provided:

- [based.hpp](based.hpp): The library without `static_assert` tests.
- [based\_test.hpp](based_test.hpp): The library with `static_assert` tests.

This is because the `static_assert` tests add quite a bit of time to the compilation, and really you shouldn't need to constantly have them enabled.

An example `based.cpp` file is provided, with an example `Makefile` that can build a small demonstration with either version of the library using `g++`. Use `make` to make and execute the version with `static_assert` tests. Use `make notest` compile and run the version without `static_assert` tests. An `a.out` file is created by the build process, and `make clean` will clean it up.

To use the library, simply copy one of the libarary versions into your codebase and `#include` it.

## Usage

All code for the library is in the `based` namespace.

### Encodings

The following encodings are defined by the library:

- `based::base64` - The [base64](https://datatracker.ietf.org/doc/html/rfc4648#section-4) encoding.
- `based::base64url` - The [base64url](https://datatracker.ietf.org/doc/html/rfc4648#section-5) encoding.
- `based::base32` - The [base32](https://datatracker.ietf.org/doc/html/rfc4648#section-6) encoding.
- `based::base32hex` - The [base32hex](https://datatracker.ietf.org/doc/html/rfc4648#section-7) encoding.
- `based::base16` - The [base16](https://datatracker.ietf.org/doc/html/rfc4648#section-8) encoding.

If the named mappings from RFC 4648 are insufficient for your needs, you can create a custom mapping as long as it is a power of two in size. For example, to specify base16, but lower-case:

```cpp
constexpr based::encoding base16lower(based::chars<'0', '9'> + based::chars<'a', 'f'>);
```

This custom encoding can be used in place of the encodings defined by the library in all places.

### Encoding

The encoding process takes a buffer of data and converts it to a buffer of encoded text.

If the buffer sizes are known at compile-time, a fixed-sized version can be used to encode from an unowned buffer to an unowned buffer:

```cpp
std::span<const std::byte, 3> data;
std::span<char, 4> text;
based::encode<based::base64>(text, data);
```

If the buffer sizes are known at compile-time, a fixed-sized version can be used to encode from an unowned buffer to a sized container:

```cpp
std::span<const std::byte, 3> data;
std::array text = based::encode<based::base64, std::array>(data);
```

If the buffer sizes are dynamic, a different version can be used to encode from an unowned buffer to an unowned buffer:

```cpp
std::span<const std::byte> data;
std::span<char> text;
std::expected<void, encode_error_buffer_size> text = based::encode<based::base64>(text, data);
```

The `encode_error_buffer_size` has two members, `buffer_size` is the actual buffer size of data passed to `encode`, and `expected_size` is the minimum size the buffer needed to be to encode the given number of bytes. This type is returned only if the given destination buffer is too small to contain the encoded text.

If the buffer sizes are dynamic, a different version can be used to encode from an unowned buffer to a container:

```cpp
std::span<const std::byte> data;
std::string text = based::encode<based::base64, std::basic_string>(data);
```

If you'd rather use a byte-oriented interface, an `encoder` class exists to help with it:

```cpp
using text_block = based::encoder<based:base64>::text_block;
std::optional<text_block> result;

based::encoder<based:base64> encoder;
result = encoder.push(std::byte{0x01});
result = encoder.push(std::byte{0x02});
result = encoder.push(std::byte{0x03});
result = encoder.push(std::byte{0x04});
result = encoder.flush();
```

Use `push` to add data a byte at a time, and `flush` when done adding data. Both functions return an optional array ofdata that was encoded by the most recent byte pushed. By default the `encoder` buffers one block (as defined by the RFC) of text, but a second template parameter to the class allows a larger buffer size as a multiple of blocks. This will change the size of the array returned by the function.

There exists an `encode_unchecked` function, which is used internally to be a fast implementation that performs potentially unsafe operations if certain bounds aren't checked or certain conditions are not ensured.

### Decoding

The decoding process takes a buffer of encoded text and converts it to a buffer of data. Decoding is a little more complex than encoding, because not all strings are valid encoded messages, and the size of a decoded messages can vary based on the _content_ of the encoded string, not just its length. For decoding methods where a fixed-sized container is used, a `based::decode_success` structure is returned that has a single member `pad_bytes`, representing the number of bytes in the fixed-sized container which do _not_ contain decoded message bytes.

Several types of error can occur when decoding. Each type of error has its own type, and a `std::variant` of all of these types is created and returned in a `std::expected` whenever an error occurs.

- `based::decode_error_message_size` - The message to decode is not a multiple of the block character count. Contains `message_size` which is the size of the message given to `decode`, and `block_chars` which is the expected multiple.
- `based::decode_error_buffer_size` - The destination buffer given to `decode` was too small to contain the encoded text. Contains `buffer_size` which is the actual buffer size given to `decode`, and `expected_size` which is the _minimum_ size that would've worked.
- `based::decode_error_character` - A character that is not valid for the encoding was found in the encoded message. The `character` gives which character it was, and the `index` gives the location within the buffer where it was found.
- `based::decode_error_pad` - A non-pad character was found after a pad character, which is not legal. The `index` member gives the location of the encoding character found after the pad character.
- `based::decode_error_pad_length` - A nonsensical pad length was found in the encoded message. The `length` member gives the invalid length found.

If an error is encountered, `std::visit` with `if constexpr` can be used to get the details of the error.

If the buffer sizes are known at compile-time, a fixed-sized version can be used to decode from an unowned buffer to an unowned buffer:

```cpp
std::span<const std::byte, 3> data;
std::span<char, 4> text;
std::expected<based::decode_success, based::decode_error> result =
    based::decode<based::base64>(data, text);
```

If the buffer sizes are known at compile-time, a fixed-sized version can be used to decode from an unowned buffer to a sized container:

```cpp
std::span<const std::byte, 3> data;
std::expected<std::tuple<std::array<char, 4>, based::decode_success>, based::decode_error> result =
    based::decode<based::base64, std::array>(data);
```

If the buffer sizes are dynamic, a different version can be used to decode from an unowned buffer to an unowned buffer:

```cpp
std::span<const std::byte> data;
std::span<char> text;
std::expected<based::decode_success, based::decode_error> result =
    based::decode<based::base64>(data, text);
```

If the buffer sizes are dynamic, a different version can be used to decode from an unowned buffer to a container:

```cpp
std::span<const char> text;
std::expected<std::vector<std::byte>, based::decode_error> data =
    based::decode<based::base64, std::vector>(text);
```

If you'd rather use a byte-oriented interface, a `decoder` class exists to help with it:

```cpp
using data_block = based::decoder<based:base64>::data_block;
std::expected<std::tuple<data_block, decode_success>, decode_error> result;

based::decoder<based:base64> decoder;
result = decoder.push();
result = decoder.push();
result = decoder.push();
result = decoder.push();
result = decoder.flush();
```

Use `push` to add data a byte at a time, and `flush` when done adding data. Both functions return an optional array ofdata that was decoded by the most recent byte pushed, or a `decode_error` if the most recent byte pushed caused an error to be dtected. By default the `decoder` buffers one block (as defined by the RFC) of text, but a second template parameter to the class allows a larger buffer size as a multiple of blocks. This will change the size of the array returned by the function.

There exists a `decode_unchecked` function, which is used internally to be a fast implementation that performs potentially unsafe operations if certain bounds aren't checked or certain conditions are not ensured.

## Example

A compiling example of all examples and the `static_assert` tests using [Godbolt.org](https://godbolt.org/#z:OYLghAFBqd5TKALEBjA9gEwKYFFMCWALugE4A0BIEAZgQDbYB2AhgLbYgDkAjF%2BTXRMiAZVQtGIHgBYBQogFUAztgAKAD24AGfgCsp5eiyahUAUgBMAIUtXyKxqiIEh1ZpgDC6egFc2TEAsATnJ3ABkCJmwAOT8AI2xSKS1yAAd0JWIXJi9ff0CQ9MznIQio2LYEpJ4Uh2wnbJEiFlIiXL8A4PtsRxKmJpaiMpj4xOT7Ztb2/K6lSaHIkcqxmoBKe3QfUlROLksAZkjUXxwAajN9jyQiIlSlEAB6B9IWAHcAOmBiJB84nxVtvJmER3hg2A9dEJsJlWCRSA84iwVJhntgaEoHkhsCxMBi2CxIgikdhMAB9IjQkFIVKpC64MxaACCDMZT1OABE0YtTixTqh/iQ2KdmBhCCZ3iyMEw5th1KlSKdEciQCARVhIsBFcSeAA2ejoV6JCAs06m05zTAqlovACeZgA7FYwFwtM7yKdnTw3R6uBZvc79v6uNIgwBWIM6oP2oMADiDQSDLCDcSDqCDmCD2CDNGdDvZLNWFxsTJZkSIp3xkQgq3OjpNZrZABVXugeaRbUpTiQeU4eZ2iFitagANYa06YFjNB4U9TluZkaGnQQKgfYYXqdipRhKCVMs3moiWkDWlh2y4WlVxG0U937OnjycsB02e35/bFxn7i/H9uni4eVAkBad191OaR7xnIhnzzIsWXrU02Q8UhsQpU4mD8RJNk7JRUmMTt0AAN0SLtB0REcx3nZD8JoccCBoGhEmBJcCHUEkoiUDEpTmdid3gg8jxw4x/y4udD0va9sFve8J2aUlBKYUkRNJOhWMwZ8ZKfV9YL3M1v3k/9QP48Sb1OO99lwB9ZPk5SWJJdTHxg98%2BL03CmGEoQ5iMkAr1Q01pMfOTXMUjyiFJTAbVYNgCHMR0NMcj8vzEkB9MuQzvx8tc/PMyyWEC4wwoi9hovs5p4ucpKUoAkK%2BSAihDPA7LILyhSlJUuzHUgsqdNNFyhNS0DAOA%2BqINlULrLatSOtGrrP10irXPc6Vy0GhVDPORrRua4KloKyLiqm2cZsSgSFv6/cVrW9aLKa6zwr2mKrE6rSnJLbrTibJACHw1JEknbI%2BWMJh0HLGgCXod5TkZeh6HNAgAC9FxaNdhyB14mB5Zb0DYVIGGwABaZwOF3WbTSVEkVTVHB/zJo8yZ1BrcAgG6gom90NK21rbMwQsXuZN6Pq%2B050B%2Bl4%2BgBpggZBsGIahmHMgRzskdOFGDXRyc%2BSxnHGAJggifKo8TzPADavdBnTionx6FCw31vZLVlVVJhRWwaniVp4l6fdb9DbpCB2esznVJ5hKzQgAj0AIbmLat0kfd5viBe%2B36xfEdHQYYRV6hYf411XcdKUiP6hEVHx6OIwWSFbJR8Wh4njopuV6gpNTLnDyP3RpimnawbBSUSUgyFJP4y9IOT4Zd7LKd76PywufM3v3TvHed12Hbphmmc227Cqi1A2YCgOQt2orUGDviw4j7mp9JGftL5kn3oeU5G0%2BpPRf%2B1OlzB05Xm%2BCtsDYGQG0PJoboHEH0JQHcfDlklkLVcCpkJIg8l7I4udBxKE2NsNcw8GKkATk/eWa5BYqzRhjDW2NcZdl1pJHkTBMAkTXDgOYhcxY4PLp2VAiDm6KmAXnKeS4fBOz6HXOaR4iJODIMJWq94Z6knEXCW29tybLx7qvZR68vZJXkZI8yfsD5BUDiSYO%2B5z5tyjtCS2oVtF4PjvzJ%2BuBu44AVDQXwSgkDi3NtgIgWw1anFwq0AgEgS4jy9q2KIJJFa0RHkxZC3jSDoyIDaH6IiEJP2zt2Yw9C6J4xErKeUGtLb0ISKcHO9DuxAToYwRUwMkDujIHArECoOADiwIrTshpa58XSa2eUYDoRKD7o4xIpJUj/HcXPc4oYbChnZBAESXlhZ9AkP%2Bb2v4jYrVNnSLZFgdQ1mgs9EOppul%2BIHjsDigznajxcWMxRZgpl3NmfM78izsjLPPElbxW4J4eFWbaKRQ0GaaIEuPckWzzKWF2bWF8b5DlKKPFPGxHgl7r3vAiu%2B%2B5elnIGQikZYyIAIveKMtxEB0oSWfFodQWgeB5lWGfN6mL%2BkXJ7qPIlSB8VDNIISvFpKKTkspRYGldKH4MvOTi1l7LLlcuJTyl2joKVaDvK%2BWl6KzQiuxRy5Sri2UEuucS5VtiH6JyFiLIu6NP7pxhsuDWwgYnYgwdKKBMDgawwRp4pB0oUleTyU3Oylwl44GdnJHwqAsUdzdiqANPc%2B7th0RZSNOAb4WJjjwW2fFF7hpAPG75yKPYb39izLm7pmb5UMdzFVpoL6R1WFmxNShLGkmpQa/cRrhbJw/sYL%2BGcrVSgpMIN19rIElydXOce/aPKeu/N6pwvqfkfJ8F8lZSU46ztphJKS5kw0OxrXWkN/S6SbuUTW/usb86BtkRYVNC8zT%2BvqKov1GaNFeTjozYtLUj4TSFfuSt3Ma3nrvvg5%2Br9jVtuLua7%2B3agR9syS6tciCB0TqSlO5uaijzbuDaGuFEbb0JuPTYuN2Hp5JtCvsS9D901boIyhy8ubfb5vyndE%2BRat4GKPgx3en7Q5mOrQR2t9azIfgAy/QWrb36gY7Rapc9Se22vdYO6DHBAGkGARIfU4D/pwfHfrBuP1p0txXSqaxi7V28o3ZhzNPHcPSR47I6QpHLo3pXveteuagUGabrGzes4OasZ3tFDjFauO/qI6SBqAm7GAcYQRhUQFInp1IPQYBYIvnqE8XEhJSSaF/HLOrRgSJyzwKzFwfCTAEtCyiJ6/4Y4gt1qtooydjddNGZVJ8xgTWfx/PecZmhZlcAHtQzxndWL91maPTGvD5aeTQJ6acxlWaFQTLudMx51VnmpCWfQf81XLHbMhfsmFfEHPMqo95Gj2U5sTbVWFKLfsotSrZc6JMXB9Wwsu3Nm7krxXOhTE9/zJy%2BnnLe3Nu7EBnRph%2BxdmbAPrtA91Wy57cFXxPcMNwUM/AAgun4OgbgHhbC2HNJgnY5wLD7D4OQIg2gfvkGHCAe0PB3jSB4KGaQ%2BxQz01DEEfYOodT7GR8GfgbAQD03eLToINQeA8GJ%2Bz0MoYLDkHRzocgWOuD8HuCkcnGP1hwFgCgTYtxoGUGoEl3GpBojsF2OoGMOo8b01OMAENpwZDvAsPwEkxAyCRwMIIYQYgJCcBkHIYQyg1CaAx%2BQfQPByCvBeKkbgfB1j0BR2jinivuAAHloGjPLOgGiFurc27t6gU4TNSCCOHDWCAXgKGMHm8Tngqx%2BDq50LS8gWIcRjGrLz6Q/OQAs6dzGLQMYggxhjPaemWhpChgj/LzH3AVcgDVxT5v1OdRaHp0EYIoZ7T2mkPTe0w/7S8/2En0PSuG%2BL/IFr5AaBNbG4N3Mm/1eQDACUKwO4SBgYCAYBSUg9wIBxGT3EIXEprHvwIAawEpqnnELoE3CAeQGCBwMIKniVjaMnjgH8MAB4CpvcLwPwDgPiCYJIKHoQMhA0ERNgQrrKPUNArsArmWD0MnvQAQHEC8Epl4DgMnkQKQLrLAURKQHEBkNgJyPgcAIwSYOfi4iwM/gAGoEDYCvCp4/To6k5e6iDiCSD%2B4qFB4aDJ7h6GDGCmC47WCGBMH3CQDrAvIeTcB4wWhzzmDWC2A7KnB4wADqKmThzho0Lw7hP0E4wg0U7huGThNAQMeMsoOwa22QnYeMwR6AeMrQzgThqeR%2BiuvBXBTC8A6wdQDQrg7Kng3gHQBg4QiwFQVQBgRQWQQg0wAQEe5RfQwwJRKw3QvQjQ8wVRBgWRfQAwrQ9Row1QEwgwbREecwgwPRyw1QmRBOfu8eiecuyeSupwue1utmBeRenBpe5e%2BAbuNeJO9eZOi%2B6w1O%2BwQQ7woYWg9o4%2BFgFgoYLOFg4uPOCefOsxJ%2Bs%2B9g8%2BexGu0xvox%2BCup%2B7xTe6wvBmQrg0gQAA).
