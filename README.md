# cabal-shatter

An experiment in faking declaration-level recompilation for Haskell.

GHC recompiles whole modules. `cabal-shatter` is meant to split a module on disk into one module per group of mutually dependent declarations, plus a facade with the original name and exports, so that GHC's ordinary recompilation check rebuilds only the group that was edited.

So far only the first stage exists: a report of the groups a module would split into.

## Usage

```
cabal run cabal-shatter -- --report FILE ...
```

Options:

- `--top N` lists the N largest groups (default 10); `--all` lists every group.
- `-XEXTENSION` passes an extension to the parser, for extensions the package turns on outside the file.
- `--with-ghc GHC` names the compiler to ask for the library directory. The default is the compiler the program was built with.

With the executable on the `PATH`, `cabal shatter --report FILE` works too.

## What the report says

```
module: Distribution.Client.ProjectPlanning
lines: 4747 (3840 in 147 declarations)
groups: 76
largest group: 1024 lines (26.7%)
expected recompile for a one-line edit: 369 lines (9.6%)
```

- **groups** is the number of modules the file would split into.
- **expected recompile** is the size of the group an edit lands in, averaged over every declaration line. It is the share of the module GHC would still have to recompile after a typical edit.

## How groups are found

The analysis is syntactic; nothing is renamed or typechecked.

- A declaration depends on another when it mentions a name the other binds. A local variable that shadows a top-level name counts as a mention, which can make groups larger but never wrong.
- Signatures, fixity declarations and pragmas join the declaration they name.
- An instance joins its class when the module defines it, and otherwise the first type in its head that the module defines. An instance with neither is reported as an orphan.
- Mutually dependent declarations form one group.

## Limits

- Modules that use CPP are refused.
- Top-level Template Haskell splices are counted and reported; the splitter will refuse them.
- The `ghc` library ties the build to one compiler version, GHC 10.0 at present.
