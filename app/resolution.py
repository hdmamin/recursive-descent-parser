from collections import deque
from contextlib import contextmanager
from typing import Any


class Resolver:

    def __init__(self):
        # Stack of dict[str, bool] containing (varname, has_been_defined)
        self.scopes = deque()
        # dict[str, int] containing (varname, depth in scopes stack)
        self.depths = {}

    @contextmanager
    def scope(self):
        try:
            self.scopes.append({})
            yield
        finally:
            self.scopes.pop()

    def resolve(self, obj: Any):
        with self.scope():
            obj.resolve()

    def declare(self, name: str):
        """
        Example:
        var a;
        """
        if self.scopes:
            self.scopes[-1][name] = False

    def define(self, name: str):
        """Mark a previously declared variable as ready for binding.

        Example:
        a = 6;
        """
        if self.scopes:
            self.scopes[-1][name] = True

    def resolve_local(self, name: str):
        i = len(self.scopes) - 1
        while i >= 0:
            declared = name in self.scopes[i]
            if declared:
                self.record_depth(name, i)
                return

    def resolve_function(self, func: "Function"):
        with self.scope():
            for param in func.params:
                self.declare(param.lexeme)
                self.define(param.lexeme)
            # TODO: translating from book here, not sure if need to call body resolve method or
            # resolver.resolve(func.body) here. Guessing we want to stay in the same scope (?) so
            # going with the former for now.
            func.body.resolve()

    def record_depth(self, name, depth: int):
        self.depths[name] = depth