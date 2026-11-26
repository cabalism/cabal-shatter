# cabal-shatter

An experiment in faking declaration-level recompilation for Haskell.

GHC recompiles whole modules. `cabal-shatter` is meant to split a module on disk into one module per group of mutually dependent declarations, plus a facade with the original name and exports, so that GHC's ordinary recompilation check rebuilds only the group that was edited.

Four things exist: a report of the groups a module would split into, the split itself, a shadow of a project in which the split libraries can be built with no change to the project, and advice on which modules to divide by hand.

## Advice on dividing modules

```
cabal-shatter --timings DUMP-DIR > times.txt
cabal-shatter --advise --costs times.txt [--flag NAME] PACKAGE-DIR ...
```

With enough cores a build takes as long as its longest chain of modules that each import the one before. `--advise` says which modules of each package's main library to divide by hand to shorten that chain, and into what. It changes nothing.

For each package it prints:

- The longest chain as the package stands, and the slowest modules on it.
- The modules to divide, most useful first. Each has the chain's length once it and those above it are divided, and its proposed pieces: their size, time, the first names each binds, and which other pieces each needs.
- The longest chain after all of them, and what is left on it. A piece marked as one group of declarations can't be divided: it is usually a large type with its instances.

With several packages, the last line is the longest chain across them, each package waiting for those it depends on.

How it works:

- A module's pieces are its groups of declarations packed by dependency level into about `--pieces N` (default 4) pieces.
- The list is found by starting with every module divided, dropping the divisions the chain doesn't need, then dropping the rest one at a time, least useful first. `--steps N` keeps the first N.
- Dividing a module assumes its importers are changed to import only the pieces they use. Which pieces those are is worked out as for `--direct`.
- Times come from `--costs`: one line per module, as `--timings` writes from the `-ddump-timings` dumps of a serial build (`-j1`) of the package as it stands. A piece's share of its module's time is estimated from its size. Without `--costs` every time is an estimate from size.
- The figures leave out what a build does besides compiling modules, and the fixed cost of each extra module, about 10 ms.

## Building, testing and loading a project split

```
cabal-shatter build TARGET ...
cabal-shatter test TARGET ...
cabal-shatter repl TARGET
```

or, with the executable on the `PATH`, `cabal shatter build TARGET ...`. The command and everything after it go to cabal as they are; `run`, `bench` and `haddock` work the same way.

Each run:

1. Brings a shadow of the project up to date in `dist-newstyle/shatter`, as `--shadow` does, reading only the modules that changed.
2. Runs the cabal command in the shadow, from the directory that matches the current one.
3. Rewrites the locations in the command's output to the project's files, as `--locate` does.

What it works out for itself:

- **The project** is the nearest directory at or above the current one that has a `cabal.project`.
- **The packages** are the local packages in the project's plan that have a library, with the flag settings the plan gives them. The plan is read from the shadow's build directory, then the project's; if neither has one, cabal is asked for it with a dry run.
- **CPP macros** come from the shadow's build directory, so on the first run some modules that use CPP are left whole. The second run splits them.

Options of this program go before the command: `--pack`, `--costs`, `--flag` to override the plan, and `--shadow DIR` for a shadow somewhere else.

A repl is run as it is, so that it keeps the terminal, and its output is not rewritten: errors there name the shadow's files. Its `:reload` and `:r` are redefined to bring the shadow up to date first, so an edit to the project's files is picked up.

## The shadow on its own


```
cabal-shatter --shadow OUT-DIR [--flag NAME] PACKAGE-DIR ...
cabal build --project-dir OUT-DIR
```

Run from the project's directory, or name it with `--root`. This makes `OUT-DIR` a mirror of the project and splits the main library of each package given.

- **The mirror:** everything is a link to the original, apart from the directories that lead to a split library's sources, its sources, and its `.cabal` file. Build directories are not mirrored, so the shadow has its own `dist-newstyle`.
- **The sources:** every module the library lists that is found as a `.hs` file is split, as with `--split --direct --pack 150`. A module that can't be split is copied. Other files in the source directories are links.
- **The `.cabal` file:** a copy with the fragments in a second `other-modules` field at the top of the `library` section. Nothing else changes, so the library's exposed modules are the same.
- **What is read from the package:** the module list, source directories, `default-language`, `default-extensions`, `cpp-options` and `include-dirs`. Conditionals are decided for this machine and compiler. A flag has the setting given with `--flag NAME` or `--flag -NAME`, or else the one in the project's plan, or else its default.
- **Which packages:** the directories named, or with none named every local package in the plan that has a library.
- **CPP:** the `cabal_macros.h` for each package is looked for in the shadow's `dist-newstyle`, then the project's, or in each `--builddir` given. Before the first build there is none, and modules that need its macros are copied whole. Run the command again after that build to split them.

After an edit to the original, run the command again and build. Only the files whose content changed are rewritten. A module deleted from the original leaves its files behind in the shadow.

To see errors and warnings against the original files, pass the build's output through `--locate`:

```
cabal build --project-dir OUT-DIR 2>&1 | cabal-shatter --locate OUT-DIR
```

- A location in a file of the shadow becomes the file and line in the project that the line was copied from. Line spans, the locations inside a message, the line numbers beside the source excerpt and `(line N)` hints are rewritten too. Columns don't change.
- Paths are shown relative to the current directory where they are beneath it.
- A fragment's module name becomes the name of the module it was split from.
- A location on a line the splitter wrote, such as a fragment's imports, is left as it is, since it has no place in the original.
- GHC's suggestions can differ from an unsplit build's: a misspelt name is not matched against names in other fragments that the fragment doesn't import.

A run after the first reads only what changed:

- Each library's last run is remembered in `OUT-DIR/.shatter-cache`: a hash of every module's source and what the rest of the package was told about the module (the names it binds and exports, its imports, and which fragment holds each name).
- A module whose source is unchanged is not read again.
- When a changed module still tells the package the same thing, as after an edit to a function's body, only that module is written again.
- When it tells the package something different, as after adding a binding, the modules that import it, and those that import them, are read and written again too.
- Everything is read again when the `.cabal` file, the macros header, the costs file, the options or this program changes.

## Usage

```
cabal run cabal-shatter -- --report FILE ...
```

Options:

- `--top N` lists the N largest groups (default 10); `--all` lists every group.
- `-XEXTENSION` passes an extension to the parser, for extensions the package turns on outside the file.
- `--with-ghc GHC` names the compiler to ask for the library directory and to preprocess modules that use CPP. The default is the compiler the program was built with.
- `--macros HEADER` names a header to include when preprocessing, such as the `cabal_macros.h` that cabal writes under `build/autogen`. `-DMACRO` and `-IDIR` pass the package's `cpp-options` and `include-dirs`.

With the executable on the `PATH`, `cabal shatter --report FILE` works too.

To split a module, keep the original outside the package's source directory and write the split into it:

```
cabal run cabal-shatter -- --split --out SOURCE-DIR FILE ...
cabal run cabal-shatter -- --list-modules FILE ...
```

For module `Foo.Bar` this writes, under `SOURCE-DIR`:

- `Foo/Bar.hs`, a facade with the original header, export list and imports, importing every fragment.
- `Foo/Bar/S_<name>.hs`, one fragment per group, named after the least name the group binds. Each has the original pragmas, the imports it could need, and imports of the fragments it depends on. It exports everything.
- `Foo/Bar.shatter-map`, which names the module on its first line and then lists the runs of lines copied from the original: the module they are in now (a fragment, or `Foo.Bar` itself), its line, the original's line, and the line count.

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

- A module that uses CPP is preprocessed with GHC (`ghc -E`) and it is the output that is split. Every line stays on the line it came from, so the map file still points at the original. The fragments, and the module itself if it stays whole, hold the text for one set of macros: re-split after a change to the package's flags, dependencies or compiler.
- Modules with top-level Template Haskell splices or an hs-boot file are reported but not split.
- With `--direct`, an orphan instance that reaches a fragment only through a module outside the package can be lost, and so can the constructor of an external newtype.
- A module that uses `RebindableSyntax` may lose an import it needs, because the names that syntax refers to are not written in the source.
- Whole-module warnings for unused imports and unused top-level bindings are turned off in the fragments.
- The shadow splits main libraries only. Sublibraries, executables and test suites are built from the original sources.
- The `ghc` library ties the build to one compiler version, GHC 10.0 at present.
