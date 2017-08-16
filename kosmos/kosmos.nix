let pkgs = import <nixpkgs> {};
    profiling = pkgs.haskellPackages.override {
      overrides = self: super: {
        mkDerivation = args: super.mkDerivation (args // {
          enableLibraryProfiling = true;
        });
      };
    };
in rec {
  kosmos = pkgs.haskellPackages.callPackage ./default.nix {};
  kosmos_prof = profiling.callPackage ./default.nix {};
}
