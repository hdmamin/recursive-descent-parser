from collections import deque
from contextlib import contextmanager
from typing import Any


class Resolver:

    # Type hint is str to avoid circular import.
    def __init__(self, interpreter: "Interpreter"):
        self.interpreter = interpreter
        # Stack of dict[str, bool] containing (varname, has_been_defined)
        self.scopes = deque()
        # TODO: hoping I can rm depths
        # dict[str, int] containing (varname, depth in scopes stack)
        # self.depths = {}

    @contextmanager
    def scope(self):
        try:
            self.scopes.append({})
            yield self.scopes[-1]
        finally:
            self.scopes.pop()

    def resolve(self, obj: Any):
        # with self.scope():
            # obj.resolve()

        # TODO testing
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

    # TODO: we only hit this method once, for f declaration, and it passes through the NOT declared
    # path. I think key may be that this is defined at global level and we need to handle that
    # separately?
    def resolve_local(self, expr: "Expression", name: str):
        # print(f'[resolve_local], {name}, {type(name)}, {hash(expr)}')
        # If scopes is empty, that means the var is global and the while loop never executes.
        max_idx = i = len(self.scopes) - 1
        # print('[resolve local]', name, self.scopes) # TODO
        while i >= 0:
            declared = name in self.scopes[i]
            if declared:
                # TODO testing obj key in interp.resolve
                # self.interpreter.resolve(name, max_idx - i)
                self.interpreter.resolve(expr, max_idx - i)
                # print(f'[resolve local] {name}: resolved', self.scopes, max_idx-i, self.interpreter.locals) # TODO
                return
            i -= 1

    def resolve_function(self, func: "Function"):
        # TODO slightly unclear how scopes are supposed to be handled here, I *think* we want params
        # in a new scope and then body in a nested scope (because Block always creates a new one)
        # but not sure of that.
        with self.scope():
            for param in func.params:
                self.declare(param.lexeme)
                self.define(param.lexeme)
            # TODO: translating from book here, not sure if need to call body resolve method or
            # resolver.resolve(func.body) here. Guessing we want to stay in the same scope (?) so
            # going with the former for now.
            func.body.resolve(is_function_body=True)

    # TODO trying to replace this with Interpreter.resolve
    # def record_depth(self, name, depth: int):
    #     # TODO: this is never getting called. Also sounds like I might want to rm this and have
    #     # resolve_local call INTERPRETER.resolve instead? Get the sense there is a separate bug
    #     # though, seems like nothing is ever getting declared?
    #     print('record_depth', name, depth) # TODO rm
    #     self.depths[name] = depth