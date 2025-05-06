"""Define an environment to track global variables. We need to keep this in its own module so we
can reference it in both parser.py and lexer.py without circular imports.
"""
from collections import defaultdict
from typing import Any


class Environment:

    # Maps name to queue of VariableDeclaration objects. We still need to evaluate each object to
    # get its value.
    variables = defaultdict(list)
    state = {}

    # VariableDeclaration is defined in parser.py and importing it here would cause circular import.
    @classmethod
    def set(cls, var: "VariableDeclaration") -> int:
        """Process a new variable declaration.
        
        Returns
        -------
        int
            Numeric index that can be used to retrieve the appropriate expression later.
            Zero-indexed like python lists.
        """
        cls.variables[var.name].append(var)
        return len(cls.variables[var.name]) - 1

    # TODO: do we still need this?
    @classmethod
    def get(cls, name: str) -> "VariableDeclaration":
        """Retrieve a variable declaration statement.
        """
        if name not in cls.variables:
            raise KeyError(f"Variable {name!r} not found.")
        # TODO: currently returning deque, need to grab single var. Need to think about when to
        # popleft vs index in with [0].
        # Seems like simply referencing a variable should not cause popping, we want to be able to
        # do that many times without changing the value. But if we instead pop when evaluating a
        # declaration, seems like we lose that value and subsequent references will not "see" it.
        # maybe need a separate data structure storing the latest evaluated value?
        return cls.variables[name]

    @classmethod
    def update_state(cls, name: str, val: Any) -> None:
        """Update the *current* state with a resolved python value (vs set/get, which work with
        unresolved VariableDeclarations).
        Just a convenience method to avoid making the user reference nested attrs.
        """
        cls.state[name] = val

    @classmethod
    def read_state(cls, name: str) -> None:
        """Read the *current* value of a variable given current environment state. This is a
        resolved python value (vs set/get, which work with unresolved VariableDeclarations).
        Just a convenience method to avoid making the user reference nested attrs.
        """
        return cls.state[name]

    def __contains__(self, name: str) -> bool:
        return name in self.variables
