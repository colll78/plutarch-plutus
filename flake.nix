{
  description = "plutarch";

  inputs.haskell-nix.url = "github:mlabs-haskell/haskell.nix";
  inputs.nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
  inputs.hercules-ci-effects.url = "github:hercules-ci/hercules-ci-effects";

  inputs.iohk-nix.url = "github:input-output-hk/iohk-nix";
  inputs.iohk-nix.flake = false;
  # we use sphinxcontrib-haddock input
  inputs.plutus.url = "github:L-as/plutus?ref=ghc9";
  # https://github.com/input-output-hk/cardano-prelude/pull/162
  inputs.cardano-prelude.url = "github:locallycompact/cardano-prelude?rev=93f95047bb36a055bdd56fb0cafd887c072cdce2";
  inputs.cardano-prelude.flake = false;
  inputs.cardano-base.url = "github:input-output-hk/cardano-base";
  inputs.cardano-base.flake = false;
  inputs.cardano-crypto.url = "github:input-output-hk/cardano-crypto?rev=07397f0e50da97eaa0575d93bee7ac4b2b2576ec";
  inputs.cardano-crypto.flake = false;
  # https://github.com/Quid2/flat/pull/27
  inputs.flat.url = "github:Quid2/flat?rev=41a040c413351e021982bb78bd00f750628f8060";
  inputs.flat.flake = false;
  # https://github.com/locallycompact/protolude
  inputs.protolude.url = "github:protolude/protolude?rev=d821ef0ac7552cfa2c3e7a7bdf29539f57e3fae6";
  inputs.protolude.flake = false;
  # https://github.com/JonasDuregard/sized-functors/pull/10
  inputs.sized-functors.url = "github:JonasDuregard/sized-functors?rev=fe6bf78a1b97ff7429630d0e8974c9bc40945dcf";
  inputs.sized-functors.flake = false;
  inputs.Shrinker.url = "github:Plutonomicon/Shrinker";
  inputs.Shrinker.flake = false;
  inputs.haskell-language-server.url = "github:haskell/haskell-language-server";
  inputs.haskell-language-server.flake = false;

  # https://github.com/hspec/hspec/pull/648
  inputs.hspec.url = "github:hspec/hspec";
  inputs.hspec.flake = false;
  # Overriding hspec (above) necessitates overriding these for some reason.
  inputs.hspec-hedgehog.url = "github:parsonsmatt/hspec-hedgehog";
  inputs.hspec-hedgehog.flake = false;
  inputs.hspec-golden.url = "github:stackbuilders/hspec-golden";
  inputs.hspec-golden.flake = false;

  inputs.secp256k1-haskell.url = "github:haskoin/secp256k1-haskell";
  inputs.secp256k1-haskell.flake = false;

  inputs.inline-r.url = "github:tweag/haskellR";
  inputs.inline-r.flake = false;

  inputs.emanote.url = "github:srid/emanote/master";

  outputs = inputs@{ self, nixpkgs, iohk-nix, haskell-nix, plutus, hercules-ci-effects, ... }:
    let
      supportedSystems = with nixpkgs.lib.systems.supported; tier1 ++ tier2 ++ tier3;

      perSystem = nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [ haskell-nix.overlay (import "${iohk-nix}/overlays/crypto") ];
        inherit (haskell-nix) config;
      };
      nixpkgsFor' = system: import nixpkgs { inherit system; };

      ghcVersion = "ghc922";
      isGhc9 = x: builtins.trace "Checking whether ${x} is GHC 9.*" (true);

      # https://github.com/input-output-hk/haskell.nix/issues/1177
      nonReinstallablePkgs = [
        "rts"
        "ghc-heap"
        "ghc-prim"
        "integer-gmp"
        "integer-simple"
        "base"
        "deepseq"
        "array"
        "ghc-boot-th"
        "pretty"
        "template-haskell"
        # ghcjs custom packages
        "ghcjs-prim"
        "ghcjs-th"
        "ghc-bignum"
        "exceptions"
        "stm"
        "ghc-boot"
        "ghc"
        "Cabal"
        "Win32"
        "array"
        "binary"
        "bytestring"
        "containers"
        "directory"
        "filepath"
        "ghc-boot"
        "ghc-compact"
        "ghc-prim"
        # "ghci" "haskeline"
        "hpc"
        "mtl"
        "parsec"
        "process"
        "text"
        "time"
        "transformers"
        "unix"
        "xhtml"
        "terminfo"
      ];

      tools.fourmolu = { };
      tools.haskell-language-server = {
        modules = [
          { inherit nonReinstallablePkgs; }
        ];
        compiler-nix-name = ghcVersion;
        # For some reason it doesn't use the latest version automatically.
        index-state =
          let l = builtins.attrNames (import "${haskell-nix.inputs.hackage}/index-state-hashes.nix"); in
          builtins.elemAt l (builtins.length l - 1);
        name = "haskell-language-server";
        version = "latest";
        cabalProjectLocal = ''
          allow-newer: *:*

          constraints:
            primitive-unlifted < 1.0.0.0

          package haskell-language-server
            flags: +use-ghc-stub +pedantic +ignore-plugins-ghc-bounds -alternateNumberFormat -brittany -eval -haddockComments -hlint -retrie -splice -stylishhaskell -tactic
        '';
        src = "${inputs.haskell-language-server}";
      };

      haskellModules = [
        ({ config, pkgs, hsPkgs, ... }: {
          inherit nonReinstallablePkgs; # Needed only so we can use hspec; https://github.com/Plutonomicon/plutarch/issues/409
          packages = {
            cardano-binary.doHaddock = false;
            cardano-binary.ghcOptions = [ "-Wwarn" ];
            cardano-crypto-class.components.library.pkgconfig = pkgs.lib.mkForce [ [ pkgs.libsodium-vrf ] ];
            cardano-crypto-class.doHaddock = false;
            cardano-crypto-class.ghcOptions = [ "-Wwarn" ];
            cardano-crypto-praos.components.library.pkgconfig = pkgs.lib.mkForce [ [ pkgs.libsodium-vrf ] ];
            cardano-prelude.doHaddock = false; # somehow above options are not applied?
            cardano-prelude.ghcOptions = [ "-Wwarn" ];
            # Workaround missing support for build-tools:
            # https://github.com/input-output-hk/haskell.nix/issues/231
            plutarch-test.components.exes.plutarch-test.build-tools = [
              config.hsPkgs.hspec-discover
            ];
          };
          cabalProjectLocal = if isGhc9 config.compiler-nix-name then ''
            allow-newer:
              cardano-binary:base
              , cardano-crypto-class:base
              , cardano-prelude:base
              , canonical-json:bytestring
              , plutus-core:ral
              , plutus-core:some
              , monoidal-containers:base
              , hedgehog:mmorph
              , text:deepseq
              , hedgehog:template-haskell
              , protolude:base
              , protolude:ghc-prim
              , protolude:transformers-compat
              , protolude:hashable
              , protolude:bytestring
              , size-based:template-haskell
              , int-cast:base

            constraints:
              OneTuple >= 0.3.1
              , Only >= 0.1
              , QuickCheck >= 2.14.2
              , StateVar >= 1.2.2
              , Stream >= 0.4.7.2
              , adjunctions >= 4.4
              , aeson >= 2.0.3.0
              , algebraic-graphs >= 0.6
              , ansi-terminal >= 0.11.1
              , ansi-wl-pprint >= 0.6.9
              , assoc >= 1.0.2
              , async >= 2.2.4
              , attoparsec >= 0.14.4
              , barbies >= 2.0.3.1
              , base-compat >= 0.12.1
              , base-compat-batteries >= 0.12.1
              , base-orphans >= 0.8.6
              , base16-bytestring >= 1.0.2.0
              , basement >= 0.0.12
              , bifunctors >= 5.5.11
              , bimap >= 0.4.0
              , bin >= 0.1.2
              , boring >= 0.2
              , boxes >= 0.1.5
              , cabal-doctest >= 1.0.9
              , call-stack >= 0.4.0
              , canonical-json >= 0.6.0.0
              , cardano-binary >= 1.5.0
              , cardano-crypto >= 1.1.0
              , cardano-crypto-class >= 2.0.0
              , cardano-prelude >= 0.1.0.0
              , case-insensitive >= 1.2.1.0
              , cassava >= 0.5.2.0
              , cborg >= 0.2.6.0
              , clock >= 0.8.2
              , colour >= 2.3.6
              , comonad >= 5.0.8
              , composition-prelude >= 3.0.0.2
              , concurrent-output >= 1.10.14
              , constraints >= 0.13.2
              , constraints-extras >= 0.3.2.1
              , contravariant >= 1.5.5
              , cryptonite >= 0.29
              , data-default >= 0.7.1.1
              , data-default-class >= 0.1.2.0
              , data-default-instances-containers >= 0.0.1
              , data-default-instances-dlist >= 0.0.1
              , data-default-instances-old-locale >= 0.0.1
              , data-fix >= 0.3.2
              , dec >= 0.0.4
              , dependent-map >= 0.4.0.0
              , dependent-sum >= 0.7.1.0
              , dependent-sum-template >= 0.1.1.1
              , deriving-aeson >= 0.2.8
              , deriving-compat >= 0.6
              , dictionary-sharing >= 0.1.0.0
              , distributive >= 0.6.2.1
              , dlist >= 1.0
              , dom-lt >= 0.2.3
              , double-conversion >= 2.0.2.0
              , erf >= 2.0.0.0
              , exceptions >= 0.10.4
              , extra >= 1.7.10
              , fin >= 0.2.1
              , flat >= 0.4.5
              , foldl >= 1.4.12
              , formatting >= 7.1.3
              , foundation >= 0.0.26.1
              , free >= 5.1.7
              , half >= 0.3.1
              , hashable >= 1.4.0.2
              , haskell-lexer >= 1.1
              , hedgehog >= 1.0.5
              , indexed-traversable >= 0.1.2
              , indexed-traversable-instances >= 0.1.1
              , integer-logarithms >= 1.0.3.1
              , invariant >= 0.5.5
              , kan-extensions >= 5.2.3
              , lazy-search >= 0.1.2.1
              , lazysmallcheck >= 0.6
              , lens >= 5.1
              , lifted-async >= 0.10.2.2
              , lifted-base >= 0.2.3.12
              , list-t >= 1.0.5.1
              , logict >= 0.7.0.3
              , megaparsec >= 9.2.0
              , memory >= 0.16.0
              , microlens >= 0.4.12.0
              , mmorph >= 1.2.0
              , monad-control >= 1.0.3.1
              , mono-traversable >= 1.0.15.3
              , monoidal-containers >= 0.6.2.0
              , mtl-compat >= 0.2.2
              , newtype >= 0.2.2.0
              , newtype-generics >= 0.6.1
              , nothunks >= 0.1.3
              , old-locale >= 1.0.0.7
              , old-time >= 1.1.0.3
              , optparse-applicative >= 0.16.1.0
              , parallel >= 3.2.2.0
              , parser-combinators >= 1.3.0
              , plutus-core >= 0.1.0.0
              , plutus-ledger-api >= 0.1.0.0
              , plutus-tx >= 0.1.0.0
              , pretty-show >= 1.10
              , prettyprinter >= 1.7.1
              , prettyprinter-configurable >= 0.1.0.0
              , primitive >= 0.7.3.0
              , profunctors >= 5.6.2
              , protolude >= 0.3.0
              , quickcheck-instances >= 0.3.27
              , ral >= 0.2.1
              , random >= 1.2.1
              , rank2classes >= 1.4.4
              , recursion-schemes >= 5.2.2.2
              , reflection >= 2.1.6
              , resourcet >= 1.2.4.3
              , safe >= 0.3.19
              , safe-exceptions >= 0.1.7.2
              , scientific >= 0.3.7.0
              , semialign >= 1.2.0.1
              , semigroupoids >= 5.3.7
              , semigroups >= 0.20
              , serialise >= 0.2.4.0
              , size-based >= 0.1.2.0
              , some >= 1.0.3
              , split >= 0.2.3.4
              , splitmix >= 0.1.0.4
              , stm >= 2.5.0.0
              , strict >= 0.4.0.1
              , syb >= 0.7.2.1
              , tagged >= 0.8.6.1
              , tasty >= 1.4.2.1
              , tasty-golden >= 2.3.5
              , tasty-hedgehog >= 1.1.0.0
              , tasty-hunit >= 0.10.0.3
              , temporary >= 1.3
              , terminal-size >= 0.3.2.1
              , testing-type-modifiers >= 0.1.0.1
              , text-short >= 0.1.5
              , th-abstraction >= 0.4.3.0
              , th-compat >= 0.1.3
              , th-expand-syns >= 0.4.9.0
              , th-extras >= 0.0.0.6
              , th-lift >= 0.8.2
              , th-lift-instances >= 0.1.19
              , th-orphans >= 0.13.12
              , th-reify-many >= 0.1.10
              , th-utilities >= 0.2.4.3
              , these >= 1.1.1.1
              , time-compat >= 1.9.6.1
              , transformers-base >= 0.4.6
              , transformers-compat >= 0.7.1
              , type-equality >= 1
              , typed-process >= 0.2.8.0
              , unbounded-delays >= 0.1.1.1
              , universe-base >= 1.1.3
              , unliftio-core >= 0.2.0.1
              , unordered-containers >= 0.2.16.0
              , uuid-types >= 1.0.5
              , vector >= 0.12.3.1
              , vector-algorithms >= 0.8.0.4
              , void >= 0.7.3
              , wcwidth >= 0.0.2
              , witherable >= 0.4.2
              , wl-pprint-annotated >= 0.1.0.1
              , word-array >= 0.1.0.0
              , secp256k1-haskell >= 0.6
          '' else "";
          extraSources = [
            {
              src = inputs.protolude;
              subdirs = [ "." ];
            }
            {
              src = inputs.cardano-prelude;
              subdirs = [
                "cardano-prelude"
              ];
            }
            {
              src = inputs.cardano-crypto;
              subdirs = [ "." ];
            }
            {
              src = inputs.flat;
              subdirs = [ "." ];
            }
            {
              src = inputs.cardano-base;
              subdirs = [
                "binary"
                "cardano-crypto-class"
              ];
            }
            {
              src = inputs.sized-functors;
              subdirs = [ "." ];
            }
            {
              src = inputs.plutus;
              subdirs = [
                "plutus-core"
                "plutus-ledger-api"
                "plutus-tx"
                "prettyprinter-configurable"
                "word-array"
              ];
            }
            {
              src = inputs.hspec;
              subdirs = [
                "."
                "hspec-core"
                "hspec-contrib"
                "hspec-discover"
              ];
            }
            {
              src = inputs.hspec-hedgehog;
              subdirs = [ "." ];
            }
            {
              src = inputs.hspec-golden;
              subdirs = [ "." ];
            }
            {
              src = inputs.secp256k1-haskell;
              subdirs = [ "." ];
            }
          ] ++ (if isGhc9 config.compiler-nix-name then [
            {
              src = inputs.inline-r;
              subdirs = [ "inline-r" ];
            }
          ] else [{
            src = inputs.Shrinker;
            subdirs = [ "." ];
          }]);
        })
      ];
      projectForGhc = ghcName: flagDevelopment: system:
        let pkgs = nixpkgsFor system; in
        let pkgs' = nixpkgsFor' system; in
        let addSubDir = target: subdir: source:
          if source.src == target
          then source // { subdirs = source.subdirs ++ [ subdir ]; }
          else source; in
        let pkgSet = (nixpkgsFor system).haskell-nix.cabalProject' {
          src = ./.;
          compiler-nix-name = ghcName;
          modules = haskellModules ++ [
            {
              packages.plutarch-test.flags.development = flagDevelopment;
              packages.plutarch.flags.development = flagDevelopment;
            }
          ];
          shell = {
            withHoogle = true;

            exactDeps = true;

            # We use the ones from Nixpkgs, since they are cached reliably.
            # Eventually we will probably want to build these with haskell.nix.
            nativeBuildInputs = [
              pkgs'.cabal-install
              pkgs'.hlint
              pkgs'.haskellPackages.cabal-fmt
              pkgs'.nixpkgs-fmt
              pkgSet.hsPkgs.hspec-discover.components.exes.hspec-discover
            ];

            inherit tools;

            additional = ps: [
              ps.plutus-ledger-api

              ps.hspec
              ps.hspec-core
              ps.hspec-contrib
              ps.hspec-discover
              ps.hspec-hedgehog
              ps.hspec-golden
            ];
          };
        }; in
        pkgSet;

      projectFor = projectForGhc ghcVersion;
      projectFor810 = projectForGhc "ghc8107";

      formatCheckFor = system:
        let
          pkgs = nixpkgsFor system;
          pkgs' = nixpkgsFor' system;
          t = pkgs.haskell-nix.tools ghcVersion { inherit (tools) fourmolu haskell-language-server; };
        in
        pkgs.runCommand "format-check"
          {
            nativeBuildInputs = [ pkgs'.haskellPackages.cabal-fmt pkgs'.nixpkgs-fmt t.fourmolu ];
          } ''
          export LC_CTYPE=C.UTF-8
          export LC_ALL=C.UTF-8
          export LANG=C.UTF-8
          cd ${self}
          ./bin/format || (echo "    Please run ./bin/format" ; exit 1)
          mkdir $out
        ''
      ;

      haddock = system:
        let
          pkgs = nixpkgsFor system;
          sphinxcontrib-haddock =
            pkgs.callPackage plutus.inputs.sphinxcontrib-haddock { pythonPackages = pkgs.python3Packages; };
          haddock-combine = pkgs.callPackage "${inputs.plutus}/nix/lib/haddock-combine.nix" {
            ghc = pkgs.haskell-nix.compiler.${ghcVersion};
            inherit (sphinxcontrib-haddock) sphinxcontrib-haddock;
          };
          # If you use this, filter out pretty-show, it doesn't work if not.
          # hspkgs = builtins.map (x: x.components.library) (
          #   builtins.filter (x: x ? components && x.components ? library) (
          #     builtins.attrValues self.project.${system}.hsPkgs
          #   )
          # );
          hspkgs = builtins.map (x: self.haddockProject.${system}.hsPkgs.${x}.components.library) [
            "plutarch"
            "plutus-core"
            "plutus-tx"
            "plutus-ledger-api"
          ];
        in
        haddock-combine {
          inherit hspkgs;
          prologue = pkgs.writeTextFile {
            name = "prologue";
            text = ''
              = Combined documentation for Plutarch

              == Handy module entrypoints

                * "Plutarch.Prelude"
                * "Plutarch"
            '';
          };
        };
      plutarchWebsiteStatic = system:
        let
          pkgs = nixpkgsFor system;
          configFile = (pkgs.formats.yaml { }).generate "emanote-configFile" {
            template.baseUrl = "/"; # Use this when pushing to github.io: "/plutarch/";
          };
          configDir = pkgs.runCommand "emanote-configDir" { } ''
            mkdir -p $out
            cp ${configFile} $out/index.yaml
          '';
        in
        pkgs.runCommand "plutarch-docs-html" { }
          ''
            mkdir $out
            ${inputs.emanote.defaultPackage.${system}}/bin/emanote \
              --layers "${self}/docs;${configDir}" \
              gen $out
          '';
      plutarchWebsiteLive = system: path:
        rec {
          type = "app";
          # '' is required for escaping ${} in nix
          script = (nixpkgsFor system).writers.writeBash "emanoteLiveReload.sh" ''
            set -xe
            export PORT="''${EMANOTE_PORT:-7072}"
            ${inputs.emanote.defaultPackage.${system}}/bin/emanote --layers ${path} run --port "$PORT"
          '';
          program = builtins.toString script;
        };

      # Checks the shell script using ShellCheck
      checkedShellScript = system: name: text:
        ((nixpkgsFor system).writeShellApplication {
          inherit name text;
        }) + "/bin/${name}";

      # Create a flake app to run Plutarch tests, under the given project.
      plutarchTestApp = system: name: project:
        let
          flake = project.${system}.flake { };
        in
        {
          type = "app";
          program = checkedShellScript system "plutarch-test-${name}"
            ''
              cd ${self}/plutarch-test
              ${flake.packages."plutarch-test:exe:plutarch-test"}/bin/plutarch-test;
            '';
        };

      # Take a flake app (identified as the key in the 'apps' set), and return a
      # derivation that runs it in the compile phase.
      #
      # In effect, this allows us to run an 'app' as part of the build process (eg: in CI).
      flakeApp2Derivation = system: appName:
        (nixpkgsFor system).runCommand appName { } "${self.apps.${system}.${appName}.program} | tee $out";
    in
    {
      inherit haskellModules tools;

      # Build matrix. Plutarch is built against different GHC versions, and 'development' flag.
      projectMatrix = {
        ghc9 = {
          nodev = perSystem (system: (projectFor false system));
          dev = perSystem (system: (projectFor true system));
        };
        ghc810 = {
          nodev = perSystem (system: (projectFor810 false system));
          dev = perSystem (system: (projectFor810 true system));
        };
      };

      # Default build configuration.
      project = self.projectMatrix.ghc9.nodev;
      flake = perSystem (system: self.project.${system}.flake { });

      haddockProject = perSystem (projectFor false);

      packages = perSystem (system: self.flake.${system}.packages // {
        haddock = haddock system;
        website = plutarchWebsiteStatic system;
      });
      checks = perSystem
        (system:
          self.flake.${system}.checks
          // {
            formatCheck = formatCheckFor system;
            test-ghc9-nodev = flakeApp2Derivation system "test-ghc9-nodev";
            test-ghc9-dev = flakeApp2Derivation system "test-ghc9-dev";
            test-ghc810-nodev = flakeApp2Derivation system "test-ghc810-nodev";
            test-ghc810-dev = flakeApp2Derivation system "test-ghc810-dev";
            "ghc810-plutarch:lib:plutarch" = (self.projectMatrix.ghc810.nodev.${system}.flake { }).packages."plutarch:lib:plutarch";
            "ghc810-plutarch:lib:plutarch-test" = (self.projectMatrix.ghc810.nodev.${system}.flake { }).packages."plutarch-test:lib:plutarch-test";
            hls = checkedShellScript system "hls" "${self.project.${system}.pkgs.haskell-language-server}/bin/haskell-language-server";
          });
      # Because `nix flake check` does not work with haskell.nix (due to IFD),
      # we provide this attribute for running the checks locally, using:
      #   nix build .#check.x86_64-linux
      check = perSystem (system:
        (nixpkgsFor system).runCommand "combined-test"
          {
            checksss = builtins.attrValues self.checks.${system};
          } ''
          echo $checksss

          touch $out
        ''
      );
      apps = perSystem (system:
        self.flake.${system}.apps
        // {
          test-ghc9-nodev = plutarchTestApp system "ghc9-nodev" self.projectMatrix.ghc9.nodev;
          test-ghc9-dev = plutarchTestApp system "ghc9-dev" self.projectMatrix.ghc9.dev;
          test-ghc810-nodev = plutarchTestApp system "ghc810-nodev" self.projectMatrix.ghc810.nodev;
          test-ghc810-dev = plutarchTestApp system "ghc810-dev" self.projectMatrix.ghc810.dev;

          # `nix run .#docs` should be run from the Git repo.
          docs = plutarchWebsiteLive system "./docs";
          # `nix run github:Plutonomicon/plutarch#website` can be run from anywhere
          website = plutarchWebsiteLive system "${self}/docs";
        }
      );
      devShell = perSystem (system: self.flake.${system}.devShell);

      effects = { ref, ... }:
        let
          pkgs = nixpkgs.legacyPackages.x86_64-linux;
          hci-effects = hercules-ci-effects.lib.withPkgs pkgs;
        in
        {
          gh-pages = hci-effects.runIf (ref == "refs/heads/master") (
            hci-effects.mkEffect {
              src = self;
              buildInputs = with pkgs; [ openssh git ];
              secretsMap = {
                "ssh" = "ssh";
              };
              effectScript =
                let
                  githubHostKey = "github.com ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEAq2A7hRGmdnm9tUDbO9IDSwBK6TbQa+PXYPCPy6rbTrTtw7PHkccKrpp0yVhp5HdEIcKr6pLlVDBfOLX9QUsyCOV0wzfjIJNlGEYsdlLJizHhbn2mUjvSAHQqZETYP81eFzLQNnPHt4EVVUh7VfDESU84KezmD5QlWpXLmvU31/yMf+Se8xhHTvKSCZIFImWwoG6mbUoWf9nzpIoaSjB+weqqUUmpaaasXVal72J+UX2B+2RPW3RcT0eOzQgqlJL3RKrTJvdsjE3JEAvGq3lGHSZXy28G3skua2SmVi/w4yCE6gbODqnTWlg7+wC604ydGXA8VJiS5ap43JXiUFFAaQ==";
                in
                ''
                  writeSSHKey
                  echo ${githubHostKey} >> ~/.ssh/known_hosts
                  export GIT_AUTHOR_NAME="Hercules-CI Effects"
                  export GIT_COMMITTER_NAME="Hercules-CI Effects"
                  export EMAIL="github@croughan.sh"
                  cp -r --no-preserve=mode ${self.packages.x86_64-linux.haddock}/share/doc ./gh-pages && cd gh-pages
                  git init -b gh-pages
                  git remote add origin git@github.com:Plutonomicon/plutarch.git
                  git add .
                  git commit -m "Deploy to gh-pages"
                  git push -f origin gh-pages:gh-pages
                '';
            }
          );
        };
      herculesCI.ciSystems = [ "x86_64-linux" ];
    };
}
