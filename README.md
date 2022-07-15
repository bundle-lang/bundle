# The Bundle Programming Language

An imperative, statically typed, general-purpose programming language intended to be safe yet simple.

**Note:** the language is under heavy development.

## Quick start

Bundle uses the usual Zig build system, so the steps to compile Bundle are as follows:

1. Clone the source code with *git*:

```
git clone https://github.com/bundle-lang/bundle
cd bundle
```

2. Build the final binary with the release options. Bundle is written in Zig, so you will need to have Zig installed or in the PATH variable.

```
zig build -Drelease-safe=true
```

The binary will be located in the *zig-out/bin/* directory.

## License

Bundle is licensed under the MIT license. See [LICENSE](LICENSE) for details.
