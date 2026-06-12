# Playground

Simple projects used to study, prototype, and experiment with programming, programming-language semantics, and quantum-computing tools.

This repository is intentionally exploratory: each folder contains a small project, prototype, or set of learning notes.

## Repository structure

```text
.
├── Haskell/
│   ├── happy/
│   ├── qProbConcHappy/
│   ├── qes/
│   └── random/
├── Python/
│   └── FamilyTree/
├── pennylane/
│   ├── algorithms/
│   ├── foundationsQC/
│   └── fundamentals/
└── qrisp/
    └── IQM_winter_school/
```

## Folders

### `Haskell/`

Haskell projects and experiments.

#### `Haskell/happy/`

A small Haskell project for experimenting with the **Happy** parser generator.

It contains work related to a simple imperative language with commands such as:

```text
skip
x := e
C ; C
if b then C else C
while b do C
```

The folder includes experiments with:

- defining a grammar;
- generating a parser automatically with Happy;
- building control-flow graphs;
- experimenting with different CFG representations;
- early work towards SSA form.

Main contents:

- `app/` — source files for the prototype;
- `testHappy.cabal` — Cabal project file;
- `CHANGELOG.md` and `LICENSE` — standard project metadata.

#### `Haskell/qProbConcHappy/`

A Haskell implementation of `qProbConc`, a prototype language/tool developed during the PhD work.

The project studies programs that combine:

- classical commands;
- probabilistic choice;
- quantum operations;
- parallel composition / concurrency;
- explicit scheduling of concurrent execution.

This version reformulates the tool using a Happy grammar and parser. It also includes examples and support for running programs through the operational semantics.

Main contents:

- `app/` — implementation of the language, parser, semantics, execution functions, and supporting modules;
- `examples/` — example programs written in the prototype language;
- `README.md` — detailed usage instructions;
- `qProbConcHappy.cabal` — Cabal project file;
- `CHANGELOG.md` and `LICENSE` — standard project metadata.

#### `Haskell/qes/`

A Haskell project related to experiments with event-structure-style models and control-flow graph generation.

Main contents:

- `app/` — source files for the prototype;
- `cfg.dot` — generated Graphviz/DOT representation of a control-flow graph;
- `cfg.png` — rendered image of the control-flow graph;
- `qes.cabal` — Cabal project file;
- `CHANGELOG.md` and `LICENSE` — standard project metadata.

#### `Haskell/random/`

Small standalone Haskell experiments.

Main contents:

- `ex.hs`
- `ex1.hs`

These files are useful as quick tests, sketches, or small examples that do not yet belong to a larger Cabal project.

---

### `Python/`

Python projects and experiments.

#### `Python/FamilyTree/`

A Python desktop application for creating, storing, editing, and visualising family-tree data.

The project supports:

- creating and loading families;
- adding people to family records;
- connecting family relationships;
- editing personal information;
- browsing family connections through a graphical interface;
- storing people and family data locally.

Main contents:

- `backend/` — backend logic and data-management code;
- `main.py` — application entry point;
- `README.md` — detailed project documentation;
- `pyproject.toml` and `uv.lock` — Python project/dependency files;
- `.python-version` — Python version configuration;
- `LICENSE` — project license.

---

### `pennylane/`

Learning material and experiments with **PennyLane**, following PennyLane learning-path/codebook material.

Main contents:

- `algorithms/` — notebooks or scripts related to quantum algorithms;
- `foundationsQC/` — material on foundations of quantum computing;
- `fundamentals/` — introductory PennyLane and quantum-computing exercises.

This folder is mainly for learning, practice, and testing PennyLane concepts.

---

### `qrisp/`

Learning material and exercises related to **Qrisp**.

#### `qrisp/IQM_winter_school/`

Material related to the IQM Winter School.

This folder is mainly for experimenting with Qrisp and following school/tutorial content.

## Notes

- Some folders are polished projects with their own `README.md`.
- Other folders are small experiments or work-in-progress prototypes.
- This repository is best understood as a playground for learning, research prototypes, and implementation experiments.
