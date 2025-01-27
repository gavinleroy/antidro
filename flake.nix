{
  inputs = {
    opam-nix.url = "github:tweag/opam-nix";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.follows = "opam-nix/nixpkgs";
  };
  outputs = { self, flake-utils, opam-nix, nixpkgs }@inputs:
    # Don't forget to put the package name instead of `throw':
    let package = "antidro";
    in flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        on = opam-nix.lib.${system};
        opamQuery = {
          # Dev dependencies
          utop = "*";
          merlin = "*";
          ocamlformat = "*";
          # Build dependencies
          ocaml-base-compiler = "*";
          dune = "*";
          alcotest = "*";
          cmdliner = "*";
          sexplib = "*";
          logs = "*";
          fmt = "*";
          mtime = "*";
          # PPXS
          ppx_deriving = "*";
          ppx_sexp_conv = "*";
          ppx_hash = "*";
        };
        scope =
          on.buildOpamProject' {
            resolveArgs.dev = true;
          } ./. opamQuery;
        overlay = final: prev:
          {
            # Your overrides go here
          };
      in {
        legacyPackages = scope.overrideScope overlay;

        packages.default = self.legacyPackages.${system}.${package};

        devShell = pkgs.mkShell {
          buildInputs = with scope; [
            ocaml
            dune
            utop
            merlin
            ocamlformat
            alcotest
            cmdliner
            sexplib
            logs
            fmt
            mtime
            ppx_deriving
            ppx_sexp_conv
            ppx_hash
            pkgs.jsbeautifier
          ];
        };
      });
}
