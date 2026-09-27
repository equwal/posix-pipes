{
  description = "Portably run a command with input/output to/from CL.";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/e158d9ed9b51c98974c5e66e1ba1c9e0255fecaa";
  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" ];
      forAll = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});
    in {
      packages = forAll (pkgs: with pkgs; rec {
        posix-pipes = sbcl.buildASDFSystem {
          pname = "posix-pipes";
          version = "0.0.1";
          src = (lib.cleanSourceWith { src = self; filter = p: _: !(lib.hasSuffix ".fasl" p || lib.hasPrefix ".#" (baseNameOf p)); });
          systems = [ "posix-pipes" ];
          lispLibs = [  ];
          meta = { description = "Portably run a command with input/output to/from CL."; homepage = "https://github.com/equwal/posix-pipes"; license = lib.licenses.lgpl21; };
        };
        default = posix-pipes;
        # an SBCL with this system (and its dependencies) preloaded: `nix run .#sbcl`
        sbcl-with = sbcl.withPackages (ps: [ posix-pipes ]);
      });
      apps = forAll (pkgs: {
        sbcl = { type = "app"; program = "${self.packages.${pkgs.system}.sbcl-with}/bin/sbcl"; };
      });
    };
}
