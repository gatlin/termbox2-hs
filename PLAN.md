# Plan: Typing Test Application Enhancement

## Project Goal
Transform the current `test/Main.hs` into a fully functional typing test that tracks performance and handles navigation (backspacing) correctly.

## Current Status
- [x] Infinite stream of target text generation.
- [x] Basic rendering of a single line with a cursor.
- [x] Color-coded feedback (Green = Correct, Red = Incorrect).
- [x] Basic character input handling.
- [x] Basic backspace logic (removes character from state).

---

## Phase 1: Backspace & Navigation Refinement (Short Term)
The goal is to ensure that when a user hits backspace, the cursor moves back and the screen scrolls left if the cursor moves past the current view boundary.

- [x] **Verify View Offset Logic**: Review the `nextOffset` calculation in `handleEvent` for backspaces. Ensure that if the cursor moves to `viewOffset - 1`, the `viewOffset` decrements by `lineWidth` to keep the cursor visible.
- [x] **Edge Case Testing**: Ensure backspacing at the very beginning of the stream (index 0) does not cause negative offsets or crashes.
- [x] **Input Validation**: Confirm that all common backspace codes (Special key, ASCII 8, ASCII 127) are captured across different terminal emulators.

## Phase 2: Metrics Tracking (Long Term)
Implement the logic to calculate typing speed and accuracy.

- [x] **Update GameState**:
    - Add `startTime :: Maybe UTCTime` to track when the user begins typing.
    - Add `mistakeCount :: Int` to track total errors.
- [x] **Implement Timer**:
    - Initialize `startTime` on the first printable character input.
- [x] **Track Mistakes**:
    - Modify `handleEvent` to increment `mistakeCount` whenever a character is typed that does not match the `targetText`.
- [x] **WPM Calculation**:
    - Implement a formula for WPM: `((Total Characters Typed / 5) - Mistakes) / Minutes Elapsed`.
- [x] **UI Update**:
    - Create a new rendering function to display the current WPM and Accuracy percentage in a corner of the screen (e.g., top-right).

## Phase 3: Polish & UX
- [x] **Start/Stop States**: Add a "Press any key to start" screen so the timer doesn't start immediately upon launch.
- [x] **Visual Feedback**: Add a subtle animation or flash when a mistake is made.
- [ ] **Completion Logic**: Define a "win condition" (e.g., typing a certain number of words) or a way to stop the test and see a final summary.

---

### Technical Notes for Implementation
- **Time Handling**: Will require importing `Data.Time.Clock`.
- **State Management**: Since `appLoop` is recursive, all new metrics must be passed through the `GameState` record.
- **Rendering**: The `renderTypingTest` function should remain focused on the text line; a separate `renderStats` function should be created for the WPM display.
