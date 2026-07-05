# Implementation Plan: Time-Based Game Completion

## Goal
Transition the game state from `Typing` to `Finished` automatically once a predefined time limit has elapsed.

## Steps
- [ ] Define `gameDuration` constant in `test/Main.hs`.
- [ ] Implement a `checkTimer` function that:
    - Checks if `status` is `Typing`.
    - Checks if `startTime` is present.
    - Compares `now` with `startTime` against `gameDuration`.
    - Returns a new state with `status = Finished` and `endTime = Just now` if the limit is reached.
- [ ] Update `appLoop` in `test/Main.hs` to apply `checkTimer` at the start of each loop iteration.
- [ ] Ensure the updated state is used for both rendering and event handling in the loop.
