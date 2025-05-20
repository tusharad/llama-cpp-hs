{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  packages = [
     (pkgs.llama-cpp.overrideAttrs (oldAttrs: {
      src = pkgs.fetchFromGitHub {
        owner = "ggml-org";
        repo = "llama.cpp";
        rev = "b5392"; # Replace with the desired commit hash or tag
        sha256 = "sha256-KPX4PypT+GfiNZlTsYZMMLEfu/T6AmyaiOdrGYAN8rE="; # Replace with the correct SHA256 hash
      };
    }))

   pkgs.gmp
   pkgs.stack
   pkgs.curl
  ];

  shellHook = ''
    FILE="Qwen3-0.6B-Q4_K_M.gguf"
    URL="https://huggingface.co/unsloth/Qwen3-0.6B-GGUF/resolve/main/$FILE"

    if [ ! -f "$FILE" ]; then
      echo "Downloading $FILE from HuggingFace..."
      curl -L -o "$FILE" "$URL"
    else
      echo "$FILE already exists."
    fi
  '';
}
