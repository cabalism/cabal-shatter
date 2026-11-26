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

## Importing other modules' fragments

By default a fragment imports other modules of the package as the original did, so it waits for the whole of each. With `--direct`, and every module of the package given in one run, a fragment imports the fragments that bind the names it uses:

```
cabal run cabal-shatter -- --split --direct --out SOURCE-DIR FILE ...
```

- The splitter works out what each module exports from its export list and imports, following re-exports inside the package.
- An import is kept as written when a name the fragment uses might come from outside the package, for example through a module that re-exports an external one.
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
