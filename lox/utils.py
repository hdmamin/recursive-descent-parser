from contextlib import contextmanager, redirect_stdout
from functools import wraps
import inspect
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


class ConditionalRedirect:
    """Helper to be used with maybe_redirect. Allows us to write to stdout OR store writes as a list
    of strings which we can return with the `items` method. See `maybe_redirect` docs for sample
    usage.
    """

    def __init__(self, redirect: bool):
        self.redirect = redirect
        # Store this before redirect_stdout overwrites it.
        self.stdout = sys.stdout
        self._items = []

    def write(self, val: str):
        if self.redirect:
            self._items.append(val)
        else:
            self.stdout.write(val)

    def flush(self):
        if not self.redirect:
            return self.stdout.flush()

    def items(self):
        return self._items


def maybe_redirect(param: str, inverse: bool = False):
    """Decorator to optionally redirect stdout within a function to a list of strings depending
    on the value of an arg passed to that decorated function.
    Caution: notice that when we do redirect stdout, this will replace any output typically
    returned by the decorated function. So this is best used for functions that return nothing but
    print lots of stuff to stdout.

    Parameters
    ----------
    param : str
        The name of the param in the decorated function that determines whether we redirect stdout
        or not. We assume the corresponding value MUST be a boolean. Note that this MUST be a
        keyword arg, not positional.
    inverse : bool
        By default we assume param=True means redirect=True. If inverse=True, we instead assume
        param=True means redirect=False.

    Example
    -------
    Notice we use inverse=True here, so codecrafters_test=False means we will redirect stdout.

    >>> @maybe_redirect(codecrafters_test, inverse=True)
    ... def execute_lox(*, codecrafters_test: bool):
    ...     print("foo")
    ...     print("bar")

    ... res = execute_lox(*, codecrafters_test=True)
    ... res
    
    None

    >>> res = execute_lox(*, codecrafters_test=False)
    ... res

    ["foo", "bar"]
    """
    def decorator(func):
        signature = inspect.signature(func)
        @wraps(func)
        def wrapper(*args, **kwargs):
            bound_args = signature.bind(*args, **kwargs)
            bound_args.apply_defaults()
            do_redirect = bound_args.arguments[param] != inverse
            redirect = ConditionalRedirect(do_redirect)
            with redirect_stdout(redirect):
                res = func(*args, **kwargs)
            if do_redirect:
                return redirect.items()
            return res
        return wrapper
    return decorator