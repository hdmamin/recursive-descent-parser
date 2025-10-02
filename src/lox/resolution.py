from collections import deque
from contextlib import contextmanager
from typing import Any

from lox.exceptions import ResolutionError
from lox.lexer import Token


class FunctionType:
    FUNCTION = "FUNCTION"
    INITIALIZER = "INITIALIZER"
    METHOD = "METHOD"
    NONE = "NONE"


class ClassType:
    CLASS = "CLASS"
    SUBCLASS = "SUBCLASS"
    NONE = "NONE"


class Resolver:

    # Type hint is str to avoid circular import.
    def __init__(self, interpreter: "Interpreter"):
        self.interpreter = interpreter
        self.current_function = FunctionType.NONE
        self.current_class = ClassType.NONE
        # Stack of dict[str, bool] containing (varname, has_been_defined)
        self.scopes = deque()

    @contextmanager
    def scope(self):
        try:
            self.scopes.append({})
            yield self.scopes[-1]
        finally:
            self.scopes.pop()

    @contextmanager
    def inside_function(self, function_type: FunctionType):
        """Set current_function attr temporarily so other code can check if we're currently
        resolving a function.
        """
        prev = self.current_function
        try:
            self.current_function = function_type
            yield
        finally:
            self.current_function = prev

    @contextmanager
    def inside_class(self, class_type: ClassType):
        """Set current_class attr temporarily so other code can check if we're currently
        resolving a class.
        """
        prev = self.current_class
        try:
            self.current_class = class_type
            yield
        finally:
            self.current_class = prev

    def resolve(self, obj: Any):
        obj.resolve()

    def declare(self, name: Token):
        """
        Example:
        var a;
        """
        if not self.scopes:
            return

        scope = self.scopes[-1]
        if name.lexeme in scope:
            raise ResolutionError(
                f"[line {name.line}] Error at {name.lexeme}: Already a variable with this name in "
                "this scope."
            )
        scope[name.lexeme] = False

    def define(self, name: Token):
        """Mark a previously declared variable as ready for binding.

        Example:
        a = 6;
        """
        if self.scopes:
            self.scopes[-1][name.lexeme] = True

    def resolve_local(self, expr: "Expression", name: str):
        # If scopes is empty, that means the var is global and the while loop never executes.
        max_idx = i = len(self.scopes) - 1
        while i >= 0:
            declared = name in self.scopes[i]
            if declared:
                self.interpreter.resolve(expr, max_idx - i)
                return
            i -= 1

    def resolve_function(
            self,
            func: "Function",
            function_type: FunctionType = FunctionType.FUNCTION
        ):
        with self.scope():
            with self.inside_function(function_type):
                for param in func.params:
                    self.declare(param)
                    self.define(param)
                func.body.resolve(is_function_body=True)
