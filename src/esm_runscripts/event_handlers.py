import signal
import sys
import os


def signal_listener():
    """Listens to the signals (eg. CTRL-C press, ...) and calls the handler
    for that signal"""

    # 1) handler for the SIGINT (CTRL-C)
    signal.signal(signal.SIGINT, handle_sigint)

    # other signal handlers may go here, eg. SIGHUP, SIGSTOP, ...)
    # each signal is handled by a function handle_<signal>, eg. handle_sigint()


def handle_sigint(signum, stack_frame):
    """Handles the SIGINT signal (aka CTRL-C press on UNIX/LINUX)

    Parameters
    ----------
    signum : int
        signal number.
    stack_frame : Python frame object
        current stack frame

    Notes
    -----
        More info: https://docs.python.org/3/library/signal.html
    """
    print()  # print a new line after ^C character (CTRL-C)
    sys.stderr.write("ctrl c is pressed. Terminating\n")

    # exit with 2 according to https://en.wikipedia.org/wiki/Signal_(IPC)
    sys.exit(signal.SIGINT.value)


if __name__ == "__main__":
    pass
