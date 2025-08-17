from collections import deque
from contextlib import contextmanager
from typing import Any


class Resolver:

    def __init__(self):
        # Stack of dict[str, bool]
        self.scopes = deque()

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