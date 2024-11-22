{
  description = "advent-of-code";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = inputs.nixpkgs.legacyPackages.${system};
        haskell = pkgs.haskell.packages.ghc98;

        buildHaskell = { year, day, bin ? [ ] }:
          let
            ghcWithPackages = haskell.ghcWithPackages
              (p: [ p.hashable p.unordered-containers p.vector ]);
          in pkgs.stdenv.mkDerivation rec {
            name = "${year}-${day}";
            srcs = [
              (builtins.filterSource
                (name: type: builtins.baseNameOf name == "${day}.hs") ./${year})
              ./lib/hs
            ];
            sourceRoot = ".";
            nativeBuildInputs = [ pkgs.makeWrapper ];
            buildPhase = ''
              ${ghcWithPackages}/bin/ghc -Wall -ihs -O2 \
                -o ${name} ${year}/${day}.hs
            '';
            installPhase = ''
              mkdir $out
              cp ${name} $out
              wrapProgram $out/${name} \
                --set PATH "${pkgs.lib.makeBinPath bin}"
            '';
          };

        buildClojure = { year, day, suffix ? "" }:
          let clojure = "${pkgs.clojure}/bin/clojure";
          in pkgs.stdenv.mkDerivation rec {
            name = "${year}-${day}${suffix}";
            srcs = [ ./${year} ./lib/clj ];
            sourceRoot = ".";
            buildPhase = ''
              echo '#!/bin/sh' >${name}
              echo 'CLJCP="$(${clojure} -Spath)":"${./lib/clj}"' >>${name}
              echo 'exec ${clojure} -Scp "$CLJCP" -M ${
                ./${year}/${day}.clj
              }' >>${name}
              chmod +x ${name}
            '';
            installPhase = "mkdir $out; cp ${name} $out";
          };

        buildC = { year, day, suffix ? "" }:
          pkgs.stdenv.mkDerivation rec {
            name = "${year}-${day}${suffix}";
            src = ./${year};
            buildPhase = "${pkgs.gcc}/bin/gcc -Wall -O2 -o ${name} ${day}.c";
            installPhase = "mkdir $out; cp ${name} $out";
          };

        buildCpp = { year, day, suffix ? "" }:
          pkgs.stdenv.mkDerivation rec {
            name = "${year}-${day}${suffix}";
            src = ./${year};
            buildPhase = "${pkgs.gcc}/bin/g++ -Wall -O2 -o ${name} ${day}.cpp";
            installPhase = "mkdir $out; cp ${name} $out";
          };

        buildGo = { year, day, suffix ? "" }:
          pkgs.stdenv.mkDerivation rec {
            name = "${year}-${day}${suffix}";
            src = ./${year};
            buildPhase = ''
              export GOPATH="$PWD/.go"
              export GOCACHE="$PWD/.cache"
              ${pkgs.go}/bin/go build -o ${name} ${day}.go
            '';
            installPhase = "mkdir $out; cp ${name} $out";
          };

        buildPython = { year, day, suffix ? "", packages ? (p: [ ]) }:
          let python = pkgs.python3.withPackages packages;
          in pkgs.stdenv.mkDerivation rec {
            name = "${year}-${day}${suffix}";
            srcs = [ ./${year} ];
            sourceRoot = ".";
            buildPhase = ''
              echo '#!${python}/bin/python' >${name}
              cat ${./${year}/${day}.py} >>${name}
              chmod +x ${name}
            '';
            installPhase = "mkdir $out; cp ${name} $out";
          };

        buildOCaml = { year, day, suffix ? "" }:
          pkgs.stdenv.mkDerivation rec {
            name = "${year}-${day}${suffix}";
            src = ./${year};
            buildPhase = ''
              cp ${day}.ml main.ml  # Avoid bad source file name error
              ${pkgs.ocaml}/bin/ocamlopt -o ${name} main.ml
            '';
            installPhase = "mkdir $out; cp ${name} $out";
          };

        buildScheme = { year, day, suffix ? "" }:
          pkgs.stdenv.mkDerivation rec {
            name = "${year}-${day}${suffix}";
            srcs = [ ./${year} ./lib/scm ];
            nativeBuildInputs = [ pkgs.openssl ];
            sourceRoot = ".";
            buildPhase = ''
              ${pkgs.gambit}/bin/gsc -exe -o ${name} ${year}/${day}.scm
            '';
            installPhase = "mkdir $out; cp ${name} $out";
          };

        solutions = {
          y2015 = {
            d01 = buildClojure {year = "2015"; day = "01";};
            d02 = buildClojure {year = "2015"; day = "02";};
            d03 = buildClojure {year = "2015"; day = "03";};
            d04 = buildClojure {year = "2015"; day = "04";};
            d05 = buildClojure {year = "2015"; day = "05";};
            d06 = buildClojure {year = "2015"; day = "06";};
            d07 = buildClojure {year = "2015"; day = "07";};
            d08 = buildHaskell {year = "2015"; day = "08";};
            d09 = buildHaskell {year = "2015"; day = "09";};
            d10 = buildC       {year = "2015"; day = "10";};
            d11 = buildHaskell {year = "2015"; day = "11";};
            d12 = buildHaskell {year = "2015"; day = "12";};
            d13 = buildHaskell {year = "2015"; day = "13";};
            d14 = buildC       {year = "2015"; day = "14";};
            d15 = buildHaskell {year = "2015"; day = "15";};
            d16 = buildHaskell {year = "2015"; day = "16";};
            d17 = buildHaskell {year = "2015"; day = "17";};
            d18 = buildC       {year = "2015"; day = "18";};
            d19 = buildHaskell {year = "2015"; day = "19";};
            d20 = buildHaskell {year = "2015"; day = "20";};
            d21 = buildHaskell {year = "2015"; day = "21";};
            d22 = buildHaskell {year = "2015"; day = "22";};
            d23 = buildHaskell {year = "2015"; day = "23";};
            d24 = buildHaskell {year = "2015"; day = "24";};
            d25 = buildHaskell {year = "2015"; day = "25";};
          };
          y2017 = {
            d01       = buildHaskell {year = "2017"; day = "01";};
            d01-c     = buildC       {year = "2017"; day = "01"; suffix = "-c";};
            d02       = buildHaskell {year = "2017"; day = "02";};
            d02-c     = buildC       {year = "2017"; day = "02"; suffix = "-c";};
            d03       = buildHaskell {year = "2017"; day = "03";};
            d03-c     = buildC       {year = "2017"; day = "03"; suffix = "-c";};
            d04       = buildHaskell {year = "2017"; day = "04";};
            d04-ocaml = buildOCaml   {year = "2017"; day = "04"; suffix = "-ocaml";};
            d05       = buildC       {year = "2017"; day = "03";};
            d06       = buildOCaml   {year = "2017"; day = "06";};
            d07       = buildHaskell {year = "2017"; day = "07";};
            d08       = buildHaskell {year = "2017"; day = "08";};
            d09       = buildHaskell {year = "2017"; day = "09";};
            d10       = buildHaskell {year = "2017"; day = "10";};
            d11       = buildHaskell {year = "2017"; day = "11";};
            d12       = buildHaskell {year = "2017"; day = "12";};
            d13       = buildHaskell {year = "2017"; day = "13";};
            d14       = buildHaskell {year = "2017"; day = "14";};
            d15       = buildHaskell {year = "2017"; day = "15";};
            d15-c     = buildC       {year = "2017"; day = "15"; suffix = "-c";};
            d16       = buildHaskell {year = "2017"; day = "16";};
            d17       = buildHaskell {year = "2017"; day = "17";};
            d18       = buildHaskell {year = "2017"; day = "18";};
            d19       = buildHaskell {year = "2017"; day = "19";};
            d20       = buildHaskell {year = "2017"; day = "20";};
            d21       = buildHaskell {year = "2017"; day = "21";};
            d22       = buildHaskell {year = "2017"; day = "22";};
            d23       = buildHaskell {year = "2017"; day = "23";};
            d24       = buildHaskell {year = "2017"; day = "24";};
            d25       = buildHaskell {year = "2017"; day = "25";};
          };
          y2018 = {
            d01 = buildHaskell {year = "2018"; day = "01";};
            d02 = buildHaskell {year = "2018"; day = "02";};
            d03 = buildHaskell {year = "2018"; day = "03";};
            d04 = buildHaskell {year = "2018"; day = "04";};
            d05 = buildHaskell {year = "2018"; day = "05";};
            d06 = buildHaskell {year = "2018"; day = "06";};
            d07 = buildHaskell {year = "2018"; day = "07";};
            d08 = buildHaskell {year = "2018"; day = "08";};
            d09 = buildHaskell {year = "2018"; day = "09";};
            d10 = buildHaskell {year = "2018"; day = "10";};
            d11 = buildHaskell {year = "2018"; day = "11";};
            d12 = buildHaskell {year = "2018"; day = "12";};
            d13 = buildHaskell {year = "2018"; day = "13";};
            d14 = buildHaskell {year = "2018"; day = "14";};
            d15 = buildHaskell {year = "2018"; day = "15";};
            d16 = buildHaskell {year = "2018"; day = "16";};
            d17 = buildHaskell {year = "2018"; day = "17";};
            d18 = buildHaskell {year = "2018"; day = "18";};
            d20 = buildHaskell {year = "2018"; day = "20";};
            d25 = buildHaskell {year = "2018"; day = "25";};
          };
          y2019 = {
            d01        = buildHaskell {year = "2019"; day = "01";};
            d01-c      = buildC       {year = "2019"; day = "01"; suffix = "-c";};
            d01-scheme = buildScheme  {year = "2019"; day = "01"; suffix = "-scheme";};
            d02        = buildHaskell {year = "2019"; day = "02";};
            d02-c      = buildC       {year = "2019"; day = "02"; suffix = "-c";};
            d02-scheme = buildScheme  {year = "2019"; day = "02"; suffix = "-scheme";};
            d03        = buildHaskell {year = "2019"; day = "03";};
            d03-scheme = buildScheme  {year = "2019"; day = "03"; suffix = "-scheme";};
            d04        = buildHaskell {year = "2019"; day = "04";};
            d04-scheme = buildScheme  {year = "2019"; day = "04"; suffix = "-scheme";};
            d05        = buildHaskell {year = "2019"; day = "05";};
            d05-scheme = buildScheme  {year = "2019"; day = "05"; suffix = "-scheme";};
            d06        = buildHaskell {year = "2019"; day = "06";};
            d06-scheme = buildScheme  {year = "2019"; day = "06"; suffix = "-scheme";};
            d07        = buildHaskell {year = "2019"; day = "07";};
            d07-scheme = buildScheme  {year = "2019"; day = "07"; suffix = "-scheme";};
            d08        = buildHaskell {year = "2019"; day = "08";};
            d08-scheme = buildScheme  {year = "2019"; day = "08"; suffix = "-scheme";};
            d09        = buildHaskell {year = "2019"; day = "09";};
            d09-scheme = buildScheme  {year = "2019"; day = "09"; suffix = "-scheme";};
            d10        = buildHaskell {year = "2019"; day = "10";};
            d10-scheme = buildScheme  {year = "2019"; day = "10"; suffix = "-scheme";};
            d11        = buildHaskell {year = "2019"; day = "11";};
            d11-scheme = buildScheme  {year = "2019"; day = "11"; suffix = "-scheme";};
            d12        = buildHaskell {year = "2019"; day = "12";};
            d12-scheme = buildScheme  {year = "2019"; day = "12"; suffix = "-scheme";};
            d13        = buildHaskell {year = "2019"; day = "13";};
            d13-scheme = buildScheme  {year = "2019"; day = "13"; suffix = "-scheme";};
            d14        = buildHaskell {year = "2019"; day = "14";};
            d15        = buildHaskell {year = "2019"; day = "15";};
            d16        = buildHaskell {year = "2019"; day = "16";};
            d17        = buildHaskell {year = "2019"; day = "17";};
            d18        = buildHaskell {year = "2019"; day = "18";};
            d19        = buildHaskell {year = "2019"; day = "19";};
            d20        = buildHaskell {year = "2019"; day = "20";};
            d21        = buildHaskell {year = "2019"; day = "21";};
            d22        = buildHaskell {year = "2019"; day = "22";};
            d23        = buildHaskell {year = "2019"; day = "23";};
            d24        = buildHaskell {year = "2019"; day = "24";};
            d25        = buildHaskell {year = "2019"; day = "25";};
          };
          y2020 = {
            d01 = buildHaskell {year = "2020"; day = "01";};
            d02 = buildHaskell {year = "2020"; day = "02";};
            d03 = buildHaskell {year = "2020"; day = "03";};
            d04 = buildHaskell {year = "2020"; day = "04";};
            d05 = buildHaskell {year = "2020"; day = "05";};
            d06 = buildHaskell {year = "2020"; day = "06";};
            d07 = buildHaskell {year = "2020"; day = "07";};
            d08 = buildHaskell {year = "2020"; day = "08";};
            d09 = buildHaskell {year = "2020"; day = "09";};
            d10 = buildHaskell {year = "2020"; day = "10";};
            d11 = buildHaskell {year = "2020"; day = "11";};
            d12 = buildHaskell {year = "2020"; day = "12";};
            d13 = buildHaskell {year = "2020"; day = "13";};
            d14 = buildHaskell {year = "2020"; day = "14";};
            d15 = buildHaskell {year = "2020"; day = "15";};
            d16 = buildHaskell {year = "2020"; day = "16";};
            d17 = buildHaskell {year = "2020"; day = "17";};
            d18 = buildHaskell {year = "2020"; day = "18";};
            d19 = buildHaskell {year = "2020"; day = "19";};
            d20 = buildHaskell {year = "2020"; day = "20";};
            d21 = buildHaskell {year = "2020"; day = "21";};
            d22 = buildHaskell {year = "2020"; day = "22";};
            d23 = buildHaskell {year = "2020"; day = "23";};
            d24 = buildHaskell {year = "2020"; day = "24";};
            d25 = buildHaskell {year = "2020"; day = "25";};
          };
          y2021 = {
            d01        = buildHaskell {year = "2021"; day = "01";};
            d02        = buildHaskell {year = "2021"; day = "02";};
            d03        = buildHaskell {year = "2021"; day = "03";};
            d04        = buildHaskell {year = "2021"; day = "04";};
            d05        = buildHaskell {year = "2021"; day = "05";};
            d06        = buildHaskell {year = "2021"; day = "06";};
            d07        = buildHaskell {year = "2021"; day = "07";};
            d08        = buildHaskell {year = "2021"; day = "08";};
            d09        = buildHaskell {year = "2021"; day = "09";};
            d10        = buildHaskell {year = "2021"; day = "10";};
            d11        = buildHaskell {year = "2021"; day = "11";};
            d12        = buildHaskell {year = "2021"; day = "12";};
            d13        = buildHaskell {year = "2021"; day = "13";};
            d14        = buildHaskell {year = "2021"; day = "14";};
            d15        = buildHaskell {year = "2021"; day = "15";};
            d16        = buildHaskell {year = "2021"; day = "16";};
            d17        = buildHaskell {year = "2021"; day = "17";};
            d18        = buildHaskell {year = "2021"; day = "18";};
            d19        = buildHaskell {year = "2021"; day = "19";};
            d20        = buildHaskell {year = "2021"; day = "20";};
            d21        = buildHaskell {year = "2021"; day = "21";};
            d22        = buildHaskell {year = "2021"; day = "22";};
            d23        = buildHaskell {year = "2021"; day = "23";};
            d24-python = buildPython  {year = "2021"; day = "24"; packages = (p: [p.z3]); suffix = "-python";};
            d24        = buildHaskell {year = "2021"; day = "24"; bin = [pkgs.z3_4_8];};  # Note use of older Z3.
            d25        = buildHaskell {year = "2021"; day = "25";};
          };
          y2022 = {
            d01     = buildHaskell {year = "2022"; day = "01";};
            d02     = buildHaskell {year = "2022"; day = "02";};
            d03     = buildHaskell {year = "2022"; day = "03";};
            d04     = buildHaskell {year = "2022"; day = "04";};
            d05     = buildHaskell {year = "2022"; day = "05";};
            d06     = buildHaskell {year = "2022"; day = "06";};
            d07     = buildHaskell {year = "2022"; day = "07";};
            d07-go  = buildGo      {year = "2022"; day = "07"; suffix = "-go";};
            d08     = buildHaskell {year = "2022"; day = "08";};
            d09     = buildHaskell {year = "2022"; day = "09";};
            d09-cpp = buildCpp     {year = "2022"; day = "09"; suffix = "-cpp";};
            d10     = buildHaskell {year = "2022"; day = "10";};
            d11     = buildHaskell {year = "2022"; day = "11";};
            d12     = buildHaskell {year = "2022"; day = "12";};
            d13     = buildHaskell {year = "2022"; day = "13";};
            d14     = buildHaskell {year = "2022"; day = "14";};
            d15     = buildHaskell {year = "2022"; day = "15";};
            d16     = buildHaskell {year = "2022"; day = "16";};
            d17     = buildHaskell {year = "2022"; day = "17";};
            d18     = buildHaskell {year = "2022"; day = "18";};
            d19     = buildHaskell {year = "2022"; day = "19";};
            d20     = buildHaskell {year = "2022"; day = "20";};
            d21     = buildHaskell {year = "2022"; day = "21"; bin = [pkgs.z3_4_12];};
            d22     = buildHaskell {year = "2022"; day = "22";};
            d23     = buildHaskell {year = "2022"; day = "23";};
            d24     = buildHaskell {year = "2022"; day = "24";};
            d25     = buildHaskell {year = "2022"; day = "25";};
          };
          y2023 = {
            d01 = buildHaskell {year = "2023"; day = "01";};
            d02 = buildHaskell {year = "2023"; day = "02";};
            d03 = buildHaskell {year = "2023"; day = "03";};
            d04 = buildHaskell {year = "2023"; day = "04";};
            d05 = buildHaskell {year = "2023"; day = "05";};
            d06 = buildHaskell {year = "2023"; day = "06";};
            d07 = buildHaskell {year = "2023"; day = "07";};
            d08 = buildHaskell {year = "2023"; day = "08";};
            d09 = buildHaskell {year = "2023"; day = "09";};
            d10 = buildHaskell {year = "2023"; day = "10";};
            d11 = buildHaskell {year = "2023"; day = "11";};
            d12 = buildHaskell {year = "2023"; day = "12";};
            d13 = buildHaskell {year = "2023"; day = "13";};
            d14 = buildHaskell {year = "2023"; day = "14";};
            d15 = buildHaskell {year = "2023"; day = "15";};
            d16 = buildHaskell {year = "2023"; day = "16";};
            d17 = buildHaskell {year = "2023"; day = "17";};
            d18 = buildHaskell {year = "2023"; day = "18";};
            d19 = buildHaskell {year = "2023"; day = "19";};
            d20 = buildHaskell {year = "2023"; day = "20";};
            d21 = buildHaskell {year = "2023"; day = "21"; bin = [pkgs.z3_4_12];};
            d22 = buildHaskell {year = "2023"; day = "22";};
            d23 = buildHaskell {year = "2023"; day = "23";};
            d24 = buildHaskell {year = "2023"; day = "24"; bin = [pkgs.z3_4_12];};
            d25 = buildHaskell {year = "2023"; day = "25";};
          };
        };

        flat = builtins.concatMap
          (pkgs.lib.attrsets.mapAttrsToList (k: v: v))
          (pkgs.lib.attrsets.mapAttrsToList (k: v: v) solutions);
      in {
        devShells = {
          default = pkgs.mkShell {
            packages = [
              (haskell.ghc.withPackages
                (p: inputs.self.packages.${system}.default.buildInputs))
            ];
          };
        };
        packages = solutions // {
          default = pkgs.symlinkJoin {
            name = "advent-of-code";
            paths = flat;
          };
        };
      });
}
