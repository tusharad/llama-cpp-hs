# llama-cpp-hs

Haskell bindings over [llama.cpp](https://github.com/ggml-org/llama.cpp)

This package provides both low-level and high-level interfaces to interact with the LLaMA C++ inference engine via Haskell FFI. 
It allows you to run LLMs locally in pure C/C++, with support for GPU acceleration and quantized models.

## Features

- Low-level access to the full LLaMA C API using Haskell FFI.
- Higher-level convenience functions for easier model interaction.
- Examples provided for quickly getting started.

---

## Example Usage

Check out the `/examples` directory to see how to load and query models directly from Haskell.

---

## Setup

### 1. Using Nix (Recommended)

Ensure that [Nix](https://nixos.org/download.html) is installed on your system.

Then, enter the development shell:

```bash
nix-shell
```

Build the project using Stack:

```bash
stack build
```

### 2. Using Stack (Manual Setup)

If you prefer not to use Nix, follow these steps:

1. Clone and install [`llama.cpp`](https://github.com/ggml-org/llama.cpp) manually.
2. Make sure `llama.h` is available at `/usr/local/include/` and compiled `libllama.a` or `libllama.so` at `/usr/local/lib/`.
3. Install Stack if you haven’t already: https://docs.haskellstack.org/en/stable/install_and_upgrade/
4. Then proceed with:

```bash
stack build
```

---

## Models

To use this library, you'll need to download one of the many open-source GGUF models available on Hugging Face

Search for compatible GGUF models:
- [Hugging Face GGUF Models](https://huggingface.co/models?search=gguf)

---

## Current State

The codebase is still under active development and may undergo breaking changes. Use it with caution in production environments.

Pull requests, issues, and community contributions are highly encouraged!

---

## Contributing

Contributions are welcome!

---

## License

This project is licensed under [MIT](LICENSE).

---

## Thank You

Thanks to [ggml-org/llama.cpp](https://github.com/ggml-org/llama.cpp) for making local LLM inference fast, lightweight, and accessible!
