# CLAUDE.md

This file provides guidance for AI assistants working with this Advent of Code solutions repository.

## Project Overview

This is a Clojure project containing solutions to [Advent of Code](https://adventofcode.com/) puzzles. The project uses Leiningen as its build tool and follows idiomatic Clojure conventions.

## Repository Structure

```
advent-of-code/
├── project.clj              # Leiningen project configuration
├── src/
│   └── advent_of_code/
│       ├── core.clj         # Main entry point (minimal)
│       └── 2022/            # Solutions organized by year
│           ├── day01.clj
│           ├── day02.clj
│           └── ...
├── inputs/
│   └── 2022/                # Puzzle inputs organized by year
│       ├── day01.txt
│       ├── day02.txt
│       └── ...
├── test/
│   └── advent_of_code/
│       └── core_test.clj    # Test placeholder
└── README.md
```

## Build & Run Commands

```bash
# Run a specific day's solution (from project root)
lein run -m advent-of-code.2022.day01

# Start a REPL
lein repl

# Run tests
lein test

# Build uberjar
lein uberjar
```

## Solution File Conventions

Each solution file follows this consistent structure:

### 1. Namespace Declaration
```clojure
(ns advent-of-code.2022.dayNN
  "Day N: Problem Title https://adventofcode.com/2022/day/N"
  (:require
   [clojure.string :as str]))
```
- Include docstring with link to the AoC problem page
- Common requires: `clojure.string`, `clojure.set`

### 2. Input Loading
```clojure
(def input-filename "inputs/2022/dayNN.txt")
```
- Use relative paths from project root
- Sample inputs may use `"inputs/2022/dayNN-sample.txt"`

### 3. Solution Structure
```clojure
;; Parsed/processed input data
(def parsed-data
  (->> (slurp input-filename)
       str/split-lines
       ...))

;; Part 1 answer
(def part-1-answer
  "Description of what this computes"
  (->> parsed-data
       ...))

;; Part 2 answer
(def part-2-answer
  "Description of what this computes"
  (->> parsed-data
       ...))
```

### 4. Main Function (optional)
```clojure
(defn -main []
  (println "part-1 answer:" part-1-answer)
  (println "part-2 answer:" part-2-answer))
```

### 5. REPL Comment Block
```clojure
(comment
  (-main)
  ;; Other REPL experiments
  )
```

## Coding Style & Patterns

### Threading Macros
Use `->>` (thread-last) extensively for data transformation pipelines:
```clojure
(->> (slurp input-filename)
     str/split-lines
     (map parse-line)
     (filter valid?)
     (apply +))
```

### Common Patterns
- `partition-by`, `group-by` for grouping data
- `reduce` with accumulator maps for stateful transformations
- `loop/recur` for iterative algorithms
- `for` with `:when` for filtered comprehensions
- Inline `time` for performance measurement

### Utility Functions
- Define helper functions at top of file with `;; UTILS start/end` markers
- Credit external sources in comments when adapting algorithms
- Use `assert` for inline sanity checks during development

### Answers
- Document answers as comments: `;; answer 1: 1234`
- Keep work-in-progress notes as comments

## Input Files

- Puzzle inputs are stored in `inputs/{year}/day{NN}.txt`
- Sample inputs use suffix: `inputs/{year}/day{NN}-sample.txt`
- Inputs are personal and should not be shared publicly per AoC rules

## Dependencies

- Clojure 1.10.3
- No external dependencies beyond clojure.core, clojure.string, clojure.set

## Adding New Solutions

1. Create solution file: `src/advent_of_code/{year}/day{NN}.clj`
2. Create input file: `inputs/{year}/day{NN}.txt`
3. Follow the namespace and structure conventions above
4. Test in REPL using the comment block

## Notes for AI Assistants

- Solutions prioritize clarity and correctness over performance optimization
- Answers are validated against AoC site, so preserve working solutions
- Use REPL-driven development workflow (comment blocks)
- When helping with a new day, create both the solution file and input file
- Keep solutions self-contained within each day's file
- Algorithms like Dijkstra's may be reimplemented per-file rather than shared
