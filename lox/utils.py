from contextlib import contextmanager
import sys
from typing import Any


SENTINEL = object()


def is_number(val: Any) -> bool:
    """Return True if a value is an int/float, false otherwise. Note that we return a python bool,
    not lox's TokenTypes.BOOL, and we expect a python variable input, not a Token or Token.lexeme.
    """
    # Need to handle bool case separately because python counts bools as ints.
    return isinstance(val, (int, float)) and not isinstance(val, bool)


def truthy(val: Any) -> bool:
    """Determine whether a value is truthy. In Lox, false and nil are considered falsy,
    everything else is considered truthy.

    Notice that this returns a python boolean which is useful for in-program logic. But if you are
    trying to print to stdout for codecrafters tests, bools must be represented as lowercase
    strings, not python bools.

    Parameters
    ----------
    val : Any
        A python object, not a literal or Token. E.g we expect False rather than "false" or
        ReservedTokenTypes.FALSE.
    """
    return val not in (False, None)


def get_interpreter() -> "Interpreter":
    """Pretty hacky workaround to grab the global INTERPRETER var defined in interpreter module.
    Otherwise would need a larger and kind of tricky refactor to avoid circular imports.
    """
    try:
        return sys.modules["lox.interpreter"].INTERPRETER
    except (KeyError, AttributeError):
        raise RuntimeError("Interpreter object is not available.")


@contextmanager
def _dummy_context_manager(*args, **kwargs):
    """Context manager that does nothing. Convenient when we want to enable/disable use of a
    real context manager depending on some arg.
    """
    try:
        yield
    finally:
        pass


def maybe_context_manager(context_manager, enable: bool, *args, **kwargs):
    """
    Examples
    --------
    def foo(track_time: bool):
        with maybe_context_manager(timer, enabled=track_time):
            do_the_thing()
    """
    resolved = context_manager if enable else _dummy_context_manager
    return resolved(*args, **kwargs)