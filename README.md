# Common Expression Language (Rust)

The [Common Expression Language (CEL)](https://github.com/google/cel-spec) is a non-Turing complete language designed
for simplicity, speed, safety, and
portability. CEL's C-like syntax looks nearly identical to equivalent expressions in C++, Go, Java, and TypeScript. CEL
is ideal for lightweight expression evaluation when a fully sandboxed scripting language is too resource intensive.

## Extensions

This version of CEL has some changes from the base Google language:

* `{}` delimited sets
* Numbers are 128 bit high precisiion decimal
* `#tag` hashtags
* Elimination of name:value maps
* `^^` exlcusive or operator
* `[low..high]` and `[0, 1, 2]` multiple list, set, and string slicing
