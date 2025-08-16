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