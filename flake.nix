{
  description = "Haskell Vanity Address Library";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    git-hooks.url = "github:cachix/git-hooks.nix";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      git-hooks,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        lib = "vanity";

        pkgs = import nixpkgs {
          inherit system;
          config.allowBroken = true;
        };
        hlib = pkgs.haskell.lib;
        # GHC 9.10.3 is Nix 25.11's default # FIXME
        hpkgs = pkgs.haskell.packages.ghc9103.extend (
          new: old: {
            ${lib} = new.callCabal2nix lib ./. { };
            # fetch newer versions from Hackage for fast EC operations
            ppad-fixed = hlib.dontCheck (
              new.callHackageDirect {
                pkg = "ppad-fixed";
                ver = "0.1.3";
                sha256 = "sha256-BXnmFyl76Rs7YePqhlhNs/cC8PJp1Hpis53G4yb26uY=";
              } { }
            );
            ppad-sha256 = hlib.dontCheck (
              new.callHackageDirect {
                pkg = "ppad-sha256";
                ver = "0.2.4";
                sha256 = "sha256-VjB5vpqp6cVlW3GN92cjhc2QwI0Ksh9/K7ydrLGW6cY=";
              } { }
            );
            ppad-sha512 = hlib.dontCheck (
              new.callHackageDirect {
                pkg = "ppad-sha512";
                ver = "0.1.4";
                sha256 = "sha256-EifR7IIc6Dfe2GNvMhivdJVS7JY5QKofk5n93TMx4r0=";
              } { }
            );
            ppad-hmac-drbg = hlib.dontCheck (
              new.callHackageDirect {
                pkg = "ppad-hmac-drbg";
                ver = "0.1.3";
                sha256 = "sha256-QNFXFagLfVtV2gTFw9ckwiDCitdiYeKAr98ZIwvPzlY=";
              } { }
            );
            ppad-secp256k1 = hlib.dontCheck (
              new.callHackageDirect {
                pkg = "ppad-secp256k1";
                ver = "0.5.2";
                sha256 = "sha256-v5RyLLt8+DNVd+etX0WaDLdPhi/OCY/zdgFqPr7P3h4=";
              } { }
            );
            ppad-ripemd160 = hlib.dontCheck (
              new.callHackageDirect {
                pkg = "ppad-ripemd160";
                ver = "0.1.4";
                sha256 = "sha256-8KaiMfFuar8xUFnHy3mh3kofhkEG5pV9lv/yjl2EotE=";
              } { }
            );
            ppad-base58 = hlib.dontCheck (
              new.callHackageDirect {
                pkg = "ppad-base58";
                ver = "0.2.2";
                sha256 = "sha256-T38kjHPAiPCs+M9UAKBrax1qll7J4byqp/QFpMuLELM=";
              } { }
            );
            ppad-bech32 = hlib.dontCheck (
              new.callHackageDirect {
                pkg = "ppad-bech32";
                ver = "0.2.4";
                sha256 = "sha256-clj0c9gzUNh2RWRDlWnU7T1EyjWQVY2j9fkhgs/kuxk=";
              } { }
            );
          }
        );

        inherit (pkgs.stdenv) cc;
        inherit (hpkgs) ghc;
        cabal = hpkgs.cabal-install;
      in
      rec {
        packages.default = hpkgs.${lib};

        devShells.default = hpkgs.shellFor {
          packages = p: [
            (hlib.doBenchmark p.${lib})
          ];

          buildInputs = [
            cabal
            cc
            hpkgs.haskell-language-server
            hpkgs.fourmolu
            hpkgs.cabal-fmt
            hpkgs.hlint
            pkgs.just
          ]
          ++ checks.pre-commit-check.enabledPackages;

          inputsFrom = builtins.attrValues self.packages.${system};

          doBenchmark = true;

          shellHook = checks.pre-commit-check.shellHook + ''
            PS1="[${lib}] \w$ "
            echo "entering ${system} shell, using"
            echo "cc:    $(${cc}/bin/cc --version)"
            echo "ghc:   $(${ghc}/bin/ghc --version)"
            echo "cabal: $(${cabal}/bin/cabal --version)"
          '';
        };

        checks = {
          pre-commit-check = git-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              # Nix
              nixfmt-rfc-style.enable = true;
              statix.enable = true;
              flake-checker = {
                enable = true;
                args = [
                  "--check-outdated"
                  "false" # don't check for nixpkgs
                ];
              };

              # Haskell
              cabal2nix.enable = true;
              fourmolu.enable = true;
              cabal-fmt.enable = true;
              hlint.enable = true;

              # Tin-foil hat
              zizmor.enable = true;
            };
          };
        };
      }
    );
}
