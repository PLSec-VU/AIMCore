# The proofs

Everything here is checked at compile time by SymFC and Bitwuzla. The
statements are ordinary Haskell over the code in `src/`. `proof/` may import
`src/`, never the reverse: the ISA (`Core.ISA`) and ideal memory
(`Memory.Ideal`), which the proofs are checked against, live in `src/`.

## Layout

`proof/Proof/` mirrors `src/`:

```
Proof/
  Core/                     the core (src/Core): the pipeline behind ideal memory
    Functional/             link 1: the core behind ideal memory refines the ISA
    Leakage/                link 1: and leaks only what the ISA-level leakage says
    SMT/                    the register file and ALU shifts as SMT terms
  Memory/Cache4K/           the 4 KiB cache (src/Memory/Cache4K*)
    Server/Functional/      the cache on its own refines ideal memory, with delay;
                            Contract.hs: what the core and any memory promise each other
    Server/Leakage/         the cache leaks only its requests' observations
    CachedCore/Functional/  link 2: the core behind the cache refines the core
                            behind ideal memory
    CachedCore/Leakage/     link 2: and leaks only what that core leaks
    SMT/                    the cache's tag and data arrays as SMT arrays
  SMT/                      shared: memory as an SMT array, the base axioms,
                            Logged (marks each query in the build log, so a
                            timeout can be traced to its property), Sanity
                            (checks that the plugin runs)
```

Link 1 and link 2 are composed on paper, not in code. `test/CacheProofSpec.hs` checks the
composition on real programs.

## Inside a proof folder

Each `Functional/` or `Leakage/` folder splits a proof into modules of the same
kinds, using the ones it needs:

| Module        | Holds |
|---------------|-------|
| `Machine`     | the system being proved, where `src/` does not already define it: the core with memory, or the core behind the cache |
| `Model`       | (leakage) what the attacker observes |
| `Simulator`   | (leakage) the machine that predicts the observations from leakage alone |
| `Driver`      | how many cycles one step of the spec takes, and its operational reading |
| `Invariant`   | the relation between the implementation and its spec |
| `Obligation`  | the base case and one inductive step per hop length |
| `Solver`      | the properties as SymFC queries; an invalid one fails the build |
| `System`      | (core) how a system state is passed to the solver: SMT arrays for the register file and memory |

Only the `Solver` modules (and the `SMT` axioms they use) run the solver. They
are built only with the `smt-proof` flag, and a property that does not hold
fails the build. Load or build them one module at a time: a single module
needs about 2 GB.
