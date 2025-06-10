from contextlib import contextmanager

from app.environment import GLOBAL_ENV, Environment


class Interpreter:

    def __init__(self):
        self.env = GLOBAL_ENV

    @contextmanager
    def new_env(self):
        prev_env = self.env
        try:
            self.env = Environment(parent=prev_env)
            yield self.env
        finally:
            self.env = prev_env

INTERPRETER = Interpreter()

