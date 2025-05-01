"""Define an environment to track global variables. We need to keep this in its own module so we
can reference it in both parser.py and lexer.py without circular imports.
"""

class Environment:

    # Maps name to VariableDeclaration object. We still need to evaluate that object to get its
    # value.
    variables = {}

    # VariableDeclaration is defined in parser.py and importing it here would cause circular import.
    @classmethod
    def set(cls, var: "VariableDeclaration"):
        cls.variables[var.name] = var

    @classmethod
    def get(cls, name: str) -> "VariableDeclaration":
        return cls.variables[name]

