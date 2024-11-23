{
    inputs = {
        nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
        purescript-overlay.url = "github:thomashoneyman/purescript-overlay";
    };

    outputs = { self, nixpkgs, purescript-overlay }:
        let system = "x86_64-linux";
            pkgs = import nixpkgs {
                inherit system;
                overlays = [
                    purescript-overlay.overlays.default
                ];
            };
        in {
            devShells.${system}.default = pkgs.mkShell {
                buildInputs = [
                    pkgs.nodejs_20
                    pkgs.esbuild
                    pkgs.purs
                    pkgs.spago-unstable
                    pkgs.purs-backend-es
                    pkgs.pscid
                    (pkgs.writeShellScriptBin "i" ''
                        spago repl $@
                    '')
                    (pkgs.writeShellScriptBin "d" ''
                        spago build
                        pscid --include="vii;ffi;popper;extra;plain-text" \
                              --censor-codes="ShadowedName,ShadowedTypeVar,MissingKindDeclaration"
                    '')
                    (pkgs.writeShellScriptBin "t" ''
                        spago test $@
                    '')
                    (pkgs.writeShellScriptBin "ty" ''
                        spago test --package=''${1%/}
                    '')
                    (pkgs.writeShellScriptBin "x" ''
                        POPPER=popper/src/Popper/App
                        VII=vii/src/Vi/App
                        PLAIN_TEXT=plain-text/src/PlainText/App
                        TARGET=""

                        case ''${1%/} in
                            "vii")
                                TARGET=$VII
                                ;;
                            "popper")
                                TARGET=$POPPER
                                ;;
                            "plain-text")
                                TARGET=$PLAIN_TEXT
                                ;;
                            *)
                                echo "invalid target"
                                exit 1
                                ;;
                        esac

                        spago build

                        esbuild $TARGET/main.js --bundle   \
                            --outfile=$TARGET/dist/main.js \
                            --servedir=$TARGET
                    '')
                ];
            };
        };
}
