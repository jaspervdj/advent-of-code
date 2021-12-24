{ pkgs ? import <nixpkgs> {} }:
let python = pkgs.python3.withPackages (p: with p; [ z3 ]); in
python.env
