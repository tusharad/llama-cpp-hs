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
  ];
}
