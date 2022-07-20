# The Bundle Programming Language

![zig version](https://img.shields.io/badge/zig-0.9.1_2022--02--14-orange)
![issues](https://img.shields.io/github/issues/bundle-lang/bundle)
![pull requests](https://img.shields.io/github/issues-pr/bundle-lang/bundle)

An imperative, statically typed, general-purpose programming language intended to be safe yet simple.

**Note:** the language is under heavy development.

## Quick start

Bundle uses the usual Zig build system, so the steps to compile Bundle are as follows:

1. Clone the source code with `git`:

```
git clone https://github.com/bundle-lang/bundle
cd bundle
```

2. Build the final binary with the release options. Bundle is written in Zig, so you will need to have Zig installed or in the PATH variable.

```
zig build -Drelease-safe=true
```

The binary will be located in the `zig-out/bin/` directory.

## Contributing

If you wish to contribute, we recommend reading the [grammar](grammar), as the examples may introduce doubts when programming some aspects of the implementation.

In addition, it may be useful to follow a commit message format, such as `directory or root file: changes`. If several directories or files have been changed, use the name `meta`.

## License

Bundle is licensed under the MIT license. See [LICENSE](LICENSE) for details.
