# cabal-shatter

An experiment in faking declaration-level recompilation for Haskell.

GHC recompiles whole modules. `cabal-shatter` is meant to split a module on disk into one module per group of mutually dependent declarations, plus a facade with the original name and exports, so that GHC's ordinary recompilation check rebuilds only the group that was edited.

Two stages exist: a report of the groups a module would split into, and the split itself. Nothing yet runs the split as part of a build.

## Usage

```
cabal run cabal-shatter -- --report FILE ...
```

Options:

- `--top N` lists the N largest groups (default 10); `--all` lists every group.
- `-XEXTENSION` passes an extension to the parser, for extensions the package turns on outside the file.
- `--with-ghc GHC` names the compiler to ask for the library directory. The default is the compiler the program was built with.

With the executable on the `PATH`, `cabal shatter --report FILE` works too.

To split a module, keep the original outside the package's source directory and write the split into it:

```
cabal run cabal-shatter -- --split --out SOURCE-DIR FILE ...
cabal run cabal-shatter -- --list-modules FILE ...
```

For module `Foo.Bar` this writes, under `SOURCE-DIR`:

- `Foo/Bar.hs`, a facade with the original header, export list and imports, importing every fragment.
- `Foo/Bar/S_<name>.hs`, one fragment per group, named after the least name the group binds. Each has the original pragmas, the imports it could need, and imports of the fragments it depends on. It exports everything.
- `Foo/Bar.shatter-map`, which lists for each fragment the lines copied from the original: fragment module, fragment line, original line, line count.

A file whose content hasn't changed is not rewritten, so after an edit only one fragment changes. `--list-modules` prints the fragment module names to add to the package's module list.

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
- An instance joins its class when the module defines it, and otherwise the first type in its head that the module defines.
- An instance with neither is an orphan. A module's orphans share one fragment, `S_orphans`, since they can use one another. Every other fragment of the module imports it, except the ones the orphans depend on.
- Mutually dependent declarations form one group.

## Which imports a fragment keeps

Without knowing what a module exports, only two kinds of import can be ruled out:

- a qualified import whose qualifier the fragment never writes;
- an import with a list of names, none of which the fragment writes.

An import that brings in everything, hides names, lists a type with `(..)`, or lists nothing (for its instances) is kept in every fragment. Adding or changing one of those rewrites every fragment, so GHC recompiles them all.

The module's header comment is not copied, so editing it changes only the facade.

## Packing

One fragment per group gives a module dozens of fragments, and every module costs GHC something whether or not it is recompiled. `--pack MS` packs groups into fragments that take at least MS milliseconds to compile:

```
cabal run cabal-shatter -- --split --pack 150 --costs costs.txt --out SOURCE-DIR FILE ...
cabal run cabal-shatter -- --report --pack 150 --costs costs.txt FILE ...
```

- Groups are packed by level, where a group's level is one more than the highest level it depends on. Groups on one level don't depend on each other, so any of them can share a fragment.
- A level that takes at least MS is packed on its own: a group that takes MS or more stays alone and the rest fill fragments in source order.
- Cheaper levels are joined to the levels above them until together they take MS, and become one fragment.
- A module that packs into one fragment is left as one module, with no facade.

Packing trades away some of the saving after an edit, and a fragment's name can change when a declaration is added, since the name comes from the least name in the fragment. `--report --pack MS` shows the result before anything is written.

### Where compile times come from

Line counts are a poor guide to compile time: a 50-line record with derived instances can cost more than a 1000-line function. So packing goes by time.

With `--costs FILE`, the times are measured ones. Make the file once from a build in which every group is its own fragment:

1. Split without `--pack`.
2. Build serially with `-ddump-timings -ddump-to-file -dumpdir DIR`. It has to be serial, with `-j1` for both cabal and GHC: GHC reports the whole process's CPU time, so under `-j` each phase is charged for the other threads' work.
3. `cabal run cabal-shatter -- --timings DIR > costs.txt`

That build is slow, but the file stays good until the code changes a lot, so it is made far less often than the package is built.

A group with no measurement, and every group when there is no file, gets an estimate from its size: 0.4 ms a line, or 1.35 ms a line for an instance or a declaration with a deriving clause. Those are averages over cabal-install at `-O0` and only a rough guide elsewhere.

## Importing other modules' fragments

By default a fragment imports other modules of the package as the original did, so it waits for the whole of each. With `--direct`, and every module of the package given in one run, a fragment imports the fragments that bind the names it uses:

```
cabal run cabal-shatter -- --split --direct --out SOURCE-DIR FILE ...
```

- The splitter works out what each module exports from its export list and imports, following re-exports inside the package.
- An import is kept as written when a name the fragment uses might come from outside the package, for example through a module that re-exports an external one.
- An import is also kept as written when it brings in a pattern synonym, or a type whose export attaches a pattern synonym to it, as in `T (P, field, ..)`. Importing `T (..)` from the fragment that binds `T` would leave those out.
- In a module left whole, an import that the export list passes on with `module X` is kept as written.
- A capitalised name the module binds only as a constructor can still be an imported type, as in `data ConfVar = OS OS`, so it is still looked for.
- A type is imported with everything that belongs to it, less whatever the original import hid or the module binds itself.
- Every newtype the original import brought in is imported, used or not, because coercing through a newtype needs its constructor in scope.
- Every fragment imports, with an empty list, the orphan fragments its module could reach, and any reachable module that couldn't be split and declares instances.

The run ends with how many imports were replaced and which modules' imports were kept most often.

## Limits

- Modules that use CPP are refused.
- Modules with top-level Template Haskell splices or an hs-boot file are reported but not split.
- With `--direct`, an orphan instance that reaches a fragment only through a module outside the package can be lost, and so can the constructor of an external newtype.
- A module that uses `RebindableSyntax` may lose an import it needs, because the names that syntax refers to are not written in the source.
- Whole-module warnings for unused imports and unused top-level bindings are turned off in the fragments.
- The `ghc` library ties the build to one compiler version, GHC 10.0 at present.
