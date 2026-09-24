"""Ranger-style directory navigation model -- pure, no UI dependency.

:class:`Browser` holds the ``h/j/k/l`` state (current directory + cursor);
:class:`DirListing` is one directory's sorted entries plus a cursor into them.
The Textual app renders these; this module never imports Textual, so it is
directly unit-testable.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from pathlib import Path
from typing import Optional


@dataclass
class DirListing:
    """One directory's sorted entries (dirs first, then files, alphabetic
    within each) and a cursor into them."""

    path: Path
    entries: list[Path] = field(default_factory=list)
    cursor: int = 0

    @classmethod
    def load(cls, path: Path) -> "DirListing":
        try:
            entries = sorted(
                path.iterdir(), key=lambda p: (not p.is_dir(), p.name.lower())
            )
        except (PermissionError, FileNotFoundError, NotADirectoryError):
            entries = []
        return cls(path=path, entries=entries)

    @property
    def selected(self) -> Optional[Path]:
        if not self.entries:
            return None
        return self.entries[self.cursor]

    def move(self, delta: int) -> None:
        if not self.entries:
            return
        self.cursor = max(0, min(len(self.entries) - 1, self.cursor + delta))

    def index_of(self, path: Path) -> int:
        for index, entry in enumerate(self.entries):
            if entry == path:
                return index
        return 0


@dataclass
class Browser:
    """Three-pane ranger state: parent / current / preview(child) listings,
    confined to *root* -- ``h`` never ascends above it, keeping the player
    inside the experiment tree being catalogued.
    """

    root: Path
    current_dir: Path
    listing: DirListing = field(init=False)

    def __post_init__(self) -> None:
        self.listing = DirListing.load(self.current_dir)

    @property
    def parent_listing(self) -> Optional[DirListing]:
        if self.current_dir == self.root:
            return None
        parent = DirListing.load(self.current_dir.parent)
        parent.cursor = parent.index_of(self.current_dir)
        return parent

    @property
    def preview_listing(self) -> Optional[DirListing]:
        selected = self.listing.selected
        if selected is not None and selected.is_dir():
            return DirListing.load(selected)
        return None

    def move_cursor(self, delta: int) -> None:
        self.listing.move(delta)

    def enter(self) -> bool:
        """``l`` -- descend into the selected directory. Returns whether it moved."""
        selected = self.listing.selected
        if selected is not None and selected.is_dir():
            self.current_dir = selected
            self.listing = DirListing.load(self.current_dir)
            return True
        return False

    def up(self) -> bool:
        """``h`` -- ascend to the parent directory, never above *root*.
        Returns whether it moved."""
        if self.current_dir == self.root:
            return False
        child = self.current_dir
        self.current_dir = self.current_dir.parent
        self.listing = DirListing.load(self.current_dir)
        self.listing.cursor = self.listing.index_of(child)
        return True
