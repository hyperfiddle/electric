# Electric randomized tests

## Goal(s)

- increase confidence in electric correctness
- shift left on testing - find minimal repros faster

## Proposed solution

- randomized testing
  - generate random electric code
  - run through randomized network
  
### Input flows

- clocks
- client inputs (e.g. atoms)
- server inputs (e.g. atoms)

### Execution plan

- step on init
- loop N times
  - mutate 1+ inputs (client, server, or both)
  - step

### Bug searching

- return value derived from input state + source code interpreter
- generated source contains assertions derived from interpreter
- return value must equal interpreter value

### Codegen

- code in triple store
- starts as with a vector holding a random input
- loop N times
  - pick random mutation of source code - inserts node(s)

### Shrinking

- loop until fixpoint (default 3 loops at most)
  - travel depth-first
  - try to shrink node:
    - re-run test:
        - if still failing we shrunk
        - else we keep node
