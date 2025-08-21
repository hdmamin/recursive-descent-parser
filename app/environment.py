"""Define an environment to track global variables. We need to keep this in its own module so we
can reference it in both parser.py and lexer.py without circular imports.
"""
from collections import defaultdict
from typing import Any, Union, Optional


class Environment:

    def __init__(self, parent: Optional["Environment"] = None):
        # TODO: am I still using this queue? doesn't seem like it but then how did I solve the issue
        # of later definitions clobbering earlier ones? I forget.
        # Maps name to queue of VariableDeclaration objects. We still need to evaluate each object
        # to get its value.
        self.variables = defaultdict(list)
        self.state = {}
        self.parent = parent

    # VariableDeclaration is defined in parser.py and importing it here would cause circular import.
    def set(self, var: Union["VariableDeclaration", "Assign"]) -> int:
        """Process a new variable declaration.
        
        Returns
        -------
        int
            Numeric index that can be used to retrieve the appropriate expression later.
            Zero-indexed like python lists.
        """
        self.variables[var.name].append(var)
        return len(self.variables[var.name]) - 1

    # TODO: do we still need this?
    def get(self, name: str) -> Union["VariableDeclaration", "Assign"]:
        """Retrieve a variable declaration statement.
        """
        if name not in self.variables:
            if self.parent:
                return self.parent.get(name)
            raise KeyError(f"Variable {name!r} not found.")
        # TODO: currently returning deque, need to grab single var. Need to think about when to
        # popleft vs index in with [0].
        # Seems like simply referencing a variable should not cause popping, we want to be able to
        # do that many times without changing the value. But if we instead pop when evaluating a
        # declaration, seems like we lose that value and subsequent references will not "see" it.
        # maybe need a separate data structure storing the latest evaluated value?
        return self.variables[name]

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
        for i in range(depth):
            env = env.parent
        return env

    # TODO: maybe need to update to use state attr now? Not sure if variables attr still being used.
    def contains(self, name: str) -> bool:
        return name in self.variables


GLOBAL_ENV = Environment()