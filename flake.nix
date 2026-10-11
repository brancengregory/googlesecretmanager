{
  description = "Dev environment for the googlesecretmanager R package";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});
      # Runtime dependencies declared in DESCRIPTION
      runtimePackages = rPkgs: with rPkgs; [
        cli
        dplyr
        gargle
        glue
        httr
        jsonlite
        lifecycle
        purrr
        rlang
      ];
      # Extra tooling for development
      devPackages = rPkgs: with rPkgs; [
        devtools
        languageserver
        pkgdown
        rcmdcheck
        roxygen2
        testthat
        usethis
      ];
    in
    {
      devShells = forAllSystems (pkgs:
        let
          rPkgs = pkgs.rPackages;
        in
        {
          default = pkgs.mkShell {
            packages = [
              (pkgs.rWrapper.override {
                packages = runtimePackages rPkgs ++ devPackages rPkgs;
              })
              pkgs.pandoc
              pkgs.jarl
              pkgs.air-formatter
            ];
          };
        });
    };
}
