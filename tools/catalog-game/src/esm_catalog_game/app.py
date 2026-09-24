"""The Textual TUI: a ranger-style file browser gamifying esm_catalog scanning.

Screens
-------
SplashScreen
    Title card: experiment id, component count, target count. Any key starts.
GameScreen
    The three-pane browser (parent / current / preview), a scoreboard sidebar,
    a countdown timer, and a docked :class:`CaptureOverlay` bar. ``h/j/k/l``
    navigate; ``c`` captures the selected file.
GameOverScreen
    Final score, accuracy, best streak, elapsed time.

``CaptureOverlay`` is a focusable bar docked to the bottom of ``GameScreen``
(shown/hidden, never a separate pushed ``Screen``/``ModalScreen``): a pushed
modal screen here hit a rendering bug in Textual 8.x (the compositor's first
paint of the modal occasionally asked the Screen itself to render as a leaf
with no visual, raising ``AttributeError: 'NoneType' object has no attribute
'render_strips'``). Docking the prompt in-screen and moving focus to it gets
the same "keys go to the prompt until dismissed" behavior via normal DOM-chain
key bubbling, without pushing a screen at all.
"""

from __future__ import annotations

import random
from pathlib import Path
from typing import Callable, Optional

from rich.text import Text
from textual import events
from textual.app import App, ComposeResult
from textual.binding import Binding
from textual.containers import Horizontal, Vertical
from textual.screen import Screen
from textual.widgets import Footer, Static

from .browser import Browser, DirListing
from .catalog_data import ExperimentCatalog
from .game_state import GameState

DEFAULT_TIME_SECONDS = 90


def _format_time(seconds: int) -> str:
    seconds = max(0, seconds)
    return f"{seconds // 60:01d}:{seconds % 60:02d}"


def _render_progress_bar(fraction: float, width: int = 24) -> Text:
    """A hand-drawn block-character progress bar (plain Static, not Textual's
    ProgressBar widget -- keeps rendering fully synchronous with game state,
    no independent animation timer to reason about)."""
    filled = round(width * min(1.0, max(0.0, fraction)))
    text = Text()
    text.append("Catalog ", style="dim")
    text.append("█" * filled, style="bold green")
    text.append("░" * (width - filled), style="grey35")
    text.append(f" {fraction * 100:.0f}%", style="white")
    return text


class SplashScreen(Screen):
    """Title card, shown before play starts."""

    BINDINGS = [Binding("q", "quit_app", "Quit", show=False)]

    def compose(self) -> ComposeResult:
        catalog: ExperimentCatalog = self.app.catalog  # type: ignore[attr-defined]
        body = Text()
        body.append("ESM-CATALOG: FIELD SCAN\n\n", style="bold cyan")
        body.append("Experiment:  ", style="dim")
        body.append(f"{catalog.experiment_id}\n", style="bold white")
        body.append("Root:        ", style="dim")
        body.append(f"{catalog.root}\n", style="white")
        body.append("Components:  ", style="dim")
        body.append(f"{', '.join(catalog.components)}\n", style="white")
        body.append("Files to catalog: ", style="dim")
        body.append(f"{catalog.total}\n\n", style="bold yellow")
        body.append(
            "Navigate the real experiment tree with h/j/k/l, like ranger.\n"
            "Land on a file and press 'c' to capture it into the catalog --\n"
            "then pick its real (component / stream) from the choices shown.\n"
            "Correct calls build a streak and score bonus points; a wrong\n"
            "call breaks the streak, and capturing a file that was never\n"
            "part of the catalog costs you too.\n\n",
            style="white",
        )
        body.append("Press any key to begin.", style="bold green blink")
        yield Static(body, id="splash")

    def on_key(self, event: events.Key) -> None:
        if event.key == "q":
            return
        self.app.push_screen("game")


class CaptureOverlay(Vertical):
    """Docked bottom bar: multiple-choice for the file under the cursor.

    Hidden (``display: none``, via the ``hidden`` class) until
    :meth:`show` is called; grabs focus so ``j/k/enter/escape`` are consumed
    here rather than by :class:`GameScreen`'s own bindings, exactly like a
    modal would, without pushing one.
    """

    BINDINGS = [
        Binding("j,down", "move(1)", "Down", show=False),
        Binding("k,up", "move(-1)", "Up", show=False),
        Binding("enter,l,c", "select", "Confirm", show=True),
        Binding("escape,q", "cancel", "Cancel", show=True),
    ]

    def __init__(self, on_result: Callable[[Optional[str]], None]) -> None:
        super().__init__(id="capture-overlay", classes="hidden")
        self._on_result = on_result
        self.filename = ""
        self.options: list[str] = []
        self.cursor = 0
        # Only focusable while shown -- otherwise it's the sole focusable
        # widget on the screen, so Textual auto-focuses it on mount and
        # silently swallows every keystroke (including navigation) even
        # while hidden behind `display: none`.
        self.can_focus = False

    def compose(self) -> ComposeResult:
        yield Static("", id="prompt-title")
        yield Static("", id="prompt-options")
        yield Static(
            "j/k move  -  enter/c confirm  -  esc cancel", id="prompt-help"
        )

    def show(self, filename: str, options: list[str]) -> None:
        self.filename = filename
        self.options = options
        self.cursor = 0
        self.query_one("#prompt-title", Static).update(f"Capture: {filename}")
        self._render_options()
        self.remove_class("hidden")
        self.can_focus = True
        self.focus()

    def hide(self) -> None:
        self.add_class("hidden")
        self.can_focus = False

    def _render_options(self) -> None:
        text = Text()
        for index, option in enumerate(self.options):
            marker = ">" if index == self.cursor else " "
            style = "reverse bold yellow" if index == self.cursor else "white"
            text.append(f" {marker} {option}\n", style=style)
        self.query_one("#prompt-options", Static).update(text)

    def action_move(self, delta: int) -> None:
        if not self.options:
            return
        self.cursor = max(0, min(len(self.options) - 1, self.cursor + delta))
        self._render_options()

    def action_select(self) -> None:
        choice = self.options[self.cursor] if self.options else None
        self.hide()
        self._on_result(choice)

    def action_cancel(self) -> None:
        self.hide()
        self._on_result(None)


class GameOverScreen(Screen):
    """Final scoreboard."""

    BINDINGS = [Binding("q", "quit_app", "Quit", show=True)]

    def __init__(self, won: bool) -> None:
        super().__init__()
        self.won = won

    def compose(self) -> ComposeResult:
        state: GameState = self.app.state  # type: ignore[attr-defined]
        body = Text()
        headline = "CATALOG COMPLETE" if self.won else "TIME'S UP"
        body.append(f"{headline}\n\n", style="bold green" if self.won else "bold red")
        body.append("Score:         ", style="dim")
        body.append(f"{state.score}\n", style="bold yellow")
        body.append("Captured:      ", style="dim")
        body.append(f"{state.captured_count} / {state.total_targets}\n", style="white")
        body.append("Accuracy:      ", style="dim")
        body.append(f"{state.accuracy * 100:.0f}%\n", style="white")
        body.append("Best streak:   ", style="dim")
        body.append(f"{state.best_streak}\n\n", style="white")
        body.append("Press q to quit.", style="dim")
        yield Static(body, id="gameover")

    def action_quit_app(self) -> None:
        self.app.exit()


class GameScreen(Screen):
    """The three-pane ranger browser plus scoreboard sidebar."""

    BINDINGS = [
        Binding("j,down", "move_down", "Down"),
        Binding("k,up", "move_up", "Up"),
        Binding("h,left", "go_up", "Up dir"),
        Binding("l,right", "enter_dir", "Enter dir"),
        Binding("g", "go_top", "Top"),
        Binding("G", "go_bottom", "Bottom"),
        Binding("c,enter", "capture", "Capture"),
        Binding("q", "forfeit", "Quit"),
    ]

    def __init__(self) -> None:
        super().__init__()
        self.browser: Optional[Browser] = None
        self.flash: str = ""
        self.flash_style: str = "white"
        self.time_left: int = DEFAULT_TIME_SECONDS
        self._pending_target = None  # type: ignore[var-annotated]

    def compose(self) -> ComposeResult:
        with Horizontal(id="panes"):
            yield Static(id="pane-parent", classes="pane")
            yield Static(id="pane-current", classes="pane pane-current")
            yield Static(id="pane-preview", classes="pane")
            with Vertical(id="sidebar"):
                yield Static(id="sidebar-info")
                yield Static(id="progress")
                yield Static(id="sidebar-flash")
        yield CaptureOverlay(self._on_capture_result)
        yield Footer()

    def on_mount(self) -> None:
        catalog: ExperimentCatalog = self.app.catalog  # type: ignore[attr-defined]
        self.browser = Browser(root=catalog.root, current_dir=catalog.root)
        self.time_left = self.app.time_budget  # type: ignore[attr-defined]
        self.set_interval(1.0, self._tick)
        self._refresh()

    def _tick(self) -> None:
        self.time_left -= 1
        if self.time_left <= 0:
            self.time_left = 0
            self._end_game(won=False)
            return
        self._refresh()

    def _end_game(self, won: bool) -> None:
        self.app.push_screen(GameOverScreen(won=won))

    # -- rendering ---------------------------------------------------
    def _render_pane(
        self, listing: Optional[DirListing], highlight: bool
    ) -> Text:
        text = Text()
        if listing is None:
            return text
        catalog: ExperimentCatalog = self.app.catalog  # type: ignore[attr-defined]
        state: GameState = self.app.state  # type: ignore[attr-defined]
        if not listing.entries:
            text.append(" (empty)\n", style="dim italic")
            return text
        for index, entry in enumerate(listing.entries):
            selected = index == listing.cursor
            if entry.is_dir():
                marker = "*" if catalog.has_uncaptured_under(entry, state.captured) else " "
                label = f"{marker} {entry.name}/\n"
                style = "bold cyan"
            else:
                target = catalog.target_for(entry)
                if target is None:
                    label = f"  {entry.name}\n"
                    style = "grey50"
                elif entry in state.captured:
                    label = f"✓ {entry.name}\n"
                    style = "bold green"
                else:
                    label = f"● {entry.name}\n"
                    style = "bold yellow"
            if selected:
                style = f"reverse {style}" if highlight else f"{style} underline"
            text.append(label, style=style)
        return text

    def _refresh(self) -> None:
        assert self.browser is not None
        catalog: ExperimentCatalog = self.app.catalog  # type: ignore[attr-defined]
        state: GameState = self.app.state  # type: ignore[attr-defined]

        self.query_one("#pane-parent", Static).update(
            self._render_pane(self.browser.parent_listing, highlight=False)
        )
        self.query_one("#pane-current", Static).update(
            self._render_pane(self.browser.listing, highlight=True)
        )
        self.query_one("#pane-preview", Static).update(
            self._render_pane(self.browser.preview_listing, highlight=False)
        )

        info = Text()
        info.append(f"{catalog.experiment_id}\n", style="bold cyan")
        info.append(f"{self.browser.current_dir.relative_to(catalog.root) or '.'}\n\n", style="dim")
        info.append("Time   ", style="dim")
        info.append(f"{_format_time(self.time_left)}\n", style="bold" if self.time_left > 10 else "bold red")
        info.append("Score  ", style="dim")
        info.append(f"{state.score}\n", style="bold yellow")
        info.append("Streak ", style="dim")
        info.append(f"{state.streak}", style="bold magenta")
        if state.streak >= 3:
            info.append("  on fire", style="bold red")
        info.append("\n")
        info.append(f"{state.captured_count}/{state.total_targets} logged\n", style="white")
        self.query_one("#sidebar-info", Static).update(info)

        self.query_one("#progress", Static).update(_render_progress_bar(state.progress))
        self.query_one("#sidebar-flash", Static).update(
            Text(self.flash, style=self.flash_style)
        )

    def _set_flash(self, message: str, style: str) -> None:
        self.flash = message
        self.flash_style = style

    # -- navigation actions -------------------------------------------
    def action_move_down(self) -> None:
        assert self.browser is not None
        self.browser.move_cursor(1)
        self._refresh()

    def action_move_up(self) -> None:
        assert self.browser is not None
        self.browser.move_cursor(-1)
        self._refresh()

    def action_go_top(self) -> None:
        assert self.browser is not None
        self.browser.listing.cursor = 0
        self._refresh()

    def action_go_bottom(self) -> None:
        assert self.browser is not None
        listing = self.browser.listing
        listing.cursor = max(0, len(listing.entries) - 1)
        self._refresh()

    def action_go_up(self) -> None:
        assert self.browser is not None
        self.browser.up()
        self._set_flash("", "white")
        self._refresh()

    def action_enter_dir(self) -> None:
        assert self.browser is not None
        if not self.browser.enter():
            self._set_flash("Not a directory.", "dim")
        self._refresh()

    def action_forfeit(self) -> None:
        self._end_game(won=False)

    def action_capture(self) -> None:
        assert self.browser is not None
        selected = self.browser.listing.selected
        if selected is None:
            return
        if selected.is_dir():
            self._set_flash("That's a directory -- press l to enter it.", "dim")
            self._refresh()
            return

        catalog: ExperimentCatalog = self.app.catalog  # type: ignore[attr-defined]
        state: GameState = self.app.state  # type: ignore[attr-defined]
        target = catalog.target_for(selected)

        if target is None:
            lost = state.register_trap()
            self._set_flash(
                f"TRAP: {selected.name} isn't part of the catalog (-{lost})", "bold red"
            )
            self._refresh()
            return

        if selected in state.captured:
            self._set_flash(f"{selected.name} is already logged.", "dim")
            self._refresh()
            return

        distractors = catalog.distractor_labels(target, count=3)
        options = distractors + [target.label]
        random.shuffle(options)

        self._pending_target = (selected, target)
        self.query_one(CaptureOverlay).show(selected.name, options)

    def _on_capture_result(self, choice: Optional[str]) -> None:
        if self._pending_target is None:
            return
        selected, target = self._pending_target
        self._pending_target = None
        state: GameState = self.app.state  # type: ignore[attr-defined]

        if choice is None:
            self._refresh()
            return

        if choice == target.label:
            gained = state.register_correct(selected)
            self._set_flash(
                f"Correct! {target.label} (+{gained}, streak {state.streak})",
                "bold green",
            )
        else:
            lost = state.register_wrong()
            self._set_flash(f"Wrong -- it was {target.label} (-{lost})", "bold red")

        self._refresh()
        self.focus()
        if state.is_complete:
            self._end_game(won=True)


class CatalogGameApp(App):
    """Top-level app: owns the shared ExperimentCatalog / GameState and screens."""

    CSS = """
    Screen {
        background: $surface;
    }
    #splash, #gameover {
        padding: 2 4;
        width: 100%;
        height: 100%;
        content-align: center middle;
    }
    #panes {
        height: 1fr;
    }
    .pane {
        width: 1fr;
        border: round $primary-darken-2;
        padding: 0 1;
        overflow-y: auto;
    }
    .pane-current {
        border: round $accent;
    }
    #sidebar {
        width: 32;
        border: round $primary-darken-2;
        padding: 1 2;
    }
    #sidebar-flash {
        height: 3;
        margin-top: 1;
    }
    #capture-overlay {
        height: 7;
        border: thick $accent;
        background: $panel;
        padding: 0 2;
    }
    #capture-overlay.hidden {
        display: none;
    }
    #prompt-title {
        text-style: bold;
    }
    #prompt-help {
        color: $text-muted;
    }
    """

    SCREENS = {}

    def __init__(self, catalog: ExperimentCatalog, time_budget: int = DEFAULT_TIME_SECONDS):
        super().__init__()
        self.catalog = catalog
        self.state = GameState(total_targets=catalog.total)
        self.time_budget = time_budget

    def on_mount(self) -> None:
        self.install_screen(GameScreen(), name="game")
        self.push_screen(SplashScreen())


def run(exp_root: Path, time_budget: int = DEFAULT_TIME_SECONDS) -> None:
    from .catalog_data import load

    catalog = load(exp_root)
    if catalog.total == 0:
        raise SystemExit(
            f"esm_catalog's sourcing layer found 0 files to catalog under {exp_root} "
            "-- nothing to play. Point --exp-root at a completed ESM-Tools run."
        )
    app = CatalogGameApp(catalog, time_budget=time_budget)
    app.run()
