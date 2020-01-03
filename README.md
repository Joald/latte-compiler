# latte-compiler

## Compilation and dependencies

The compiler is built using stack. 
Parser generated with BNFC.
Packages used are listed in `package.yaml`.

## Extensions
- Implemented objects with virtual methods (extensions 2-4).

## Directory structure
- `app/Main.hs`: BNFC-generated `Main` module with minor tweaks.
- `src/Types/`: Frontend; static type checker
- `src/BNFC/`: BNFC parser files
- `src/CodeGen/`: Code generation, first to a linear representation and then to 32 bit x86 assembly.
- `src/Utils.hs`: General utilities.
