# synthir_def_parser

Definition parser for CPU arch description used in Synthir experiment
that gets the file (check the one in defs/) and writes a trait
implementation in Rust.

## Building in Linux

```
nuget
mkdir packages/
mv FParsec.1.0.1/ packages/
xbuild
```

## Full Disclosure

This parser is mostly an experiment I made testing FParsec when all
the project was gonna be written in F# but finally built it with Rust.
