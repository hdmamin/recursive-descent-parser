"""Define an environment to track global variables. We need to keep this in its own module so we
can reference it in both parser.py and lexer.py without circular imports.
"""
from collections import defaultdict
from typing import Any, Union, Optional


class Environment:

    def __init__(self, parent: Optional["Environment"] = None):
        self.state = {}
        self.parent = parent

    def update_state(self, name: str, val: Any, is_declaration: bool) -> None:
        """Update the *current* state with a resolved python value (vs set/get, which work with
        unresolved VariableDeclarations).
        Just a convenience method to avoid making the user reference nested attrs.
        """
        if is_declaration:
            self.state[name] = val
            return

        env = self
        while True:
            if name in env.state:
                env.state[name] = val
                return
            elif env.parent:
                env = env.parent
            else:
                # TODO: is this the right error type?
                raise RuntimeError(
                    f"Cannot assign a value to var {name!r} because it does not exist."
                )

    def read_state(self, name: str) -> Any:
        """Read the *current* value of a variable given current environment state. This is a
        resolved python value (vs set/get, which work with unresolved VariableDeclarations).
        Just a convenience method to avoid making the user reference nested attrs.
        """
        try:
            return self.state[name]
        except KeyError as e:
            if self.parent:
                return self.parent.read_state(name)
            raise e

    def update_state_at(self, name: str, val: Any, depth: int):
        """Assign a value to a variable in a specific env.
        We do not expose is_declaration param from update_state because we don't want to try the
        parents, we just want to update the env at the specified depth.

        Parameters
        ----------
        name : str
            Name of variable to update.
        var : Any
            The value of the variable.
        depth : int
            Number of envs away from the current env to set the var in. E.g. depth=1 means
            the env's parent.
        """
        env = self.ancestor(depth)
        return env.update_state(name, val, is_declaration=True)

    def read_state_at(self, name: str, depth: int) -> Any:
        """
        Parameters
        ----------
        name : str
            Name of variable to retrieve.
        depth : int
            Number of envs away from the current env to retrieve the var from. E.g. depth=1 means
            the env's parent.
        """
        env = self.ancestor(depth)
        return env.read_state(name)

    def ancestor(self, depth: int) -> "Environment":
        """Grab the n'th parent. E.g. depth=1 returns the env's parent, depth=2 returns its
        parent's parent.
        """
        env = self
        for _ in range(depth):
            env = env.parent
        return env

    def contains(self, name: str) -> bool:
        return name in self.state


GLOBAL_ENV = Environment()