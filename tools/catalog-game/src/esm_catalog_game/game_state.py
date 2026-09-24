"""Pure scoring/streak bookkeeping for a capture round -- no UI dependency.

Kept free of Textual so the scoring rules are unit-testable on their own.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from pathlib import Path

BASE_POINTS = 10
STREAK_BONUS_PER = 2
WRONG_PENALTY = 5
TRAP_PENALTY = 3
"""Penalty for trying to capture a file that isn't part of the catalog at all."""


@dataclass
class GameState:
    """Tracks score, streak, and which catalog targets have been captured."""

    total_targets: int
    score: int = 0
    streak: int = 0
    best_streak: int = 0
    attempts: int = 0
    correct: int = 0
    captured: set = field(default_factory=set)

    @property
    def captured_count(self) -> int:
        return len(self.captured)

    @property
    def progress(self) -> float:
        return self.captured_count / self.total_targets if self.total_targets else 1.0

    @property
    def is_complete(self) -> bool:
        return self.total_targets > 0 and self.captured_count >= self.total_targets

    @property
    def accuracy(self) -> float:
        return self.correct / self.attempts if self.attempts else 0.0

    def register_correct(self, path: Path) -> int:
        """Record a correct classification of *path*. Returns points gained
        (0 if the file was already captured -- no double-dipping on retries)."""
        self.attempts += 1
        self.correct += 1
        if path in self.captured:
            return 0
        self.captured.add(path)
        self.streak += 1
        self.best_streak = max(self.best_streak, self.streak)
        gained = BASE_POINTS + STREAK_BONUS_PER * (self.streak - 1)
        self.score += gained
        return gained

    def register_wrong(self) -> int:
        """Record a wrong classification of a real catalog file. Returns the
        (positive) points lost; breaks the streak."""
        self.attempts += 1
        self.streak = 0
        lost = min(WRONG_PENALTY, self.score)
        self.score -= lost
        return lost

    def register_trap(self) -> int:
        """Record an attempted capture of a file that is NOT part of the
        catalog at all. Returns the (positive) points lost; breaks the streak."""
        self.streak = 0
        lost = min(TRAP_PENALTY, self.score)
        self.score -= lost
        return lost
