import inspect


def verbose(func):
    """Decorator that prints function name and resolved args when the decorated function
    executes.
    """
    sig = inspect.signature(func)
    def wrapper(*args, **kwargs):
        bound_args = sig.bind(*args, **kwargs)
        bound_args.apply_defaults()
        # Rather use-case-specific logic but that's ok for now, I'm using this with decorate_methods
        # and don't want to see self in every method call.
        bound_args.arguments.pop("self", None)
        print(f'==========\n>>> Calling func {func.__name__!r} with args: {bound_args.arguments}')
        res = func(*args, **kwargs)
        print(f"<<< Func {func.__name__!r} returned: {res!r}\n==========")
        return res
    return wrapper


def decorate_methods(decorator):
    """Class decorator to wrap every non-magic, non-internal method of a class with some decorator.
    
    Examples
    --------
    @decorate_methods(verbose)
    class Foo:
        def bar(self, a, b=3):
            return a
        def foobar(self, **kwargs):
            return kwargs
    """
    def wrapper(cls):
        # isfunction because seems like methods aren't recognized
        # as methods at this point.
        methods = [
            (name, val) for name, val in inspect.getmembers(cls)
            if inspect.isfunction(val)
            and not name.startswith("_")
        ]
        for name, val in methods:
            decorated = decorator(val)
            setattr(cls, name, decorated)
        return cls
    return wrapper