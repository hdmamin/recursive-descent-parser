from contextlib import contextmanager
from datetime import datetime
from functools import wraps
from typing import Any, Optional, Union

from lox.environment import GLOBAL_ENV, Environment
from lox.exceptions import ParsingError, ResolutionError
from lox.lexer import TokenTypes, ReservedTokenTypes, Token
from lox.resolution import Resolver, FunctionType, ClassType
from lox.utils import truthy, is_number, SENTINEL, maybe_context_manager


class Return(Exception):
    """Exception raised when a return statement is evaluated."""

    def __init__(self, value: Any):
        self.value = value


class Expression:
    
    def __str__(self) -> str:
        return f"{type(self)}()"

    def resolve(self):
        """Used by Resolver to determine which scope a variable is referring to at parse time.
        """


class Literal(Expression):
    """
    Example
    'foo'
    """

    def __init__(self, val: Token):
        self.val = val

    def __str__(self) -> str:
        return self.val.non_null_literal

    def evaluate(self):
        """This returns the relevant python value, not the lox value. E.g. None rather than
        ReservedTokenTypes.NIL.
        """
        return self.val.evaluate()


class Variable(Expression):
    """
    Example
    foo
    """

    def __init__(self, identifier: Token):
        self.identifier = identifier

    def __str__(self) -> str:
        return self.identifier.non_null_literal

    def evaluate(self):
        """This returns the relevant python value, not the lox value. E.g. None rather than
        ReservedTokenTypes.NIL.
        """
        return self.identifier.evaluate(expr=self)

    def resolve(self):
        # Need to handle case like this:
        # ```
        # var a = "outer";
        # {
        #   var a = a;
        # }
        # ```
        if (
            INTERPRETER.resolver.scopes
            and not INTERPRETER.resolver.scopes[-1].get(self.identifier.lexeme, True)
        ):
            raise RuntimeError(
                f"[line {self.identifier.line}] Error at '{self.identifier.lexeme}': "
                "Can't read local variable in its own initializer."
            )
        INTERPRETER.resolver.resolve_local(self, self.identifier.lexeme)


class Unary(Expression):
    """
    Example
    !foo
    """

    def __init__(self, val: Token, right: Expression):
        self.val = val
        self.right = right

    def __str__(self) -> str:
        return "(" + self.val.non_null_literal + " " + str(self.right) + ")"

    def evaluate(self):
        """This returns the relevant python value, not the lox value. E.g. None rather than
        ReservedTokenTypes.NIL.
        """
        right = self.right.evaluate()
        if self.val.token_type == TokenTypes.BANG:
            return not truthy(right)
        if self.val.token_type == TokenTypes.MINUS:
            # Careful, python considers bools as ints. We operate on the evaluated right vs the
            # raw self.right because the latter is an expression, not a token, so has no token_type
            # attr we can reference.
            if is_number(right):
                return -right
            raise RuntimeError(f"Operand must be a number.\n[line {self.val.line}]")

        raise ValueError("Unexpected operator in Unary: {self.val.token_type}")

    def resolve(self):
        self.right.resolve()

class Binary(Expression):
    """
    Example
    3 / 4
    """

    def __init__(self, left: Expression, val: Token, right: Expression):
        self.left = left
        self.val = val
        self.right = right

    def __str__(self) -> str:
        # Book seems to want order (lexeme, left, right).
        return "(" + self.val.non_null_literal + " " + str(self.left) + " " + str(self.right) + ")"

    def evaluate(self):
        """This returns the relevant python value, not the lox value. E.g. None rather than
        ReservedTokenTypes.NIL.
        """
        left = self.left.evaluate()
        right = self.right.evaluate()

        try:
            if self.val.token_type == TokenTypes.SLASH:
                if not (is_number(left) and is_number(right)):
                    raise RuntimeError(f"Operands must be numbers.\n[line {self.val.line}]")
                res = left / right
                # If both operands and the output are all ints, we want to return an int as well.
                # Python division always returns a float so is_integer method should be available.
                # But left and right could be ints or floats.
                if isinstance(left, int) and isinstance(right, int) and res.is_integer():
                    return int(res)
                return res
            if self.val.token_type == TokenTypes.STAR:
                if not (is_number(left) and is_number(right)):
                    raise RuntimeError(f"Operands must be numbers.\n[line {self.val.line}]")
                return left * right
            if self.val.token_type == TokenTypes.MINUS:
                if not (
                    (isinstance(left, str) and isinstance(right, str))
                    or (is_number(left) and is_number(right))
                ):
                    raise RuntimeError(
                        f"Operands must be two numbers or two strings.\n[line {self.val.line}]"
                    )
                return left - right
            if self.val.token_type == TokenTypes.PLUS:
                if not (
                    (isinstance(left, str) and isinstance(right, str))
                    or (is_number(left) and is_number(right))
                ):
                    raise RuntimeError(
                        f"Operands must be two numbers or two strings.\n[line {self.val.line}]"
                    )
                return left + right
            # Note that these two cases rely on lox's definition of equality matching python's.
            # Based on the book definition this seems to be the case.
            if self.val.token_type == TokenTypes.BANG_EQUAL:
                return left != right
            if self.val.token_type == TokenTypes.EQUAL_EQUAL:
                return left == right
            
            # This condition applies to all 4 remaining operation types.
            if not (is_number(left) and is_number(right)):
                raise RuntimeError(f"Operands must be numbers.\n[line {self.val.line}]")
            if self.val.token_type == TokenTypes.GREATER:
                return left > right
            if self.val.token_type == TokenTypes.GREATER_EQUAL:
                return left >= right
            if self.val.token_type == TokenTypes.LESS:
                return left < right
            if self.val.token_type == TokenTypes.LESS_EQUAL:
                return left <= right
        except TypeError:
            raise ParsingError(f"Multiplication failed with operator: {self.val.token_type}")

        raise ParsingError(f"Unexpected operator in Binary: {self.val.token_type}")

    def resolve(self):
        self.left.resolve()
        self.right.resolve()


class Logical(Expression):

    def __init__(self, left: Expression, op: Token, right: Expression):
        self.left = left
        self.op = op
        self.right = right

    def evaluate(self):
        left = self.left.evaluate()
        left_is_truthy = truthy(left)
        if self.op.token_type == ReservedTokenTypes.AND:
            return self.right.evaluate() if left_is_truthy else left
        return self.right.evaluate() if not left_is_truthy else left

    def __str__(self) -> str:
        # TODO check if this is format book wants
        return f"({self.op.non_null_literal} {self.left} {self.right})"

    def resolve(self):
        self.left.resolve()
        self.right.resolve()


class Get(Expression):

    def __init__(self, obj: Expression, attr: Token):
        self.obj = obj
        self.attr = attr

    def __str__(self):
        return f"{type(self).__name__}(obj={self.obj}, attr={self.attr})"

    def evaluate(self):
        obj = self.obj.evaluate()
        if not isinstance(obj, LoxInstance):
            raise RuntimeError("Only instances have properties.")
        return obj.get(self.attr.lexeme)

    def resolve(self):
        self.obj.resolve()


class Set(Expression):

    def __init__(self, obj: Expression, attr: Token, val: Expression):
        """
        Parameters
        ----------
        obj : LoxInstance
            The instance the attribute should be attached to.
            # TODO: seems like this is actually This at leas in some cases? It's obj.evaluate() that
            should return a LoxInstance.
        """
        self.obj = obj
        self.attr = attr
        self.val = val

    def __str__(self):
        return f"{type(self).__name__}(obj={self.obj}, attr={self.attr}, val={self.val})"

    def evaluate(self, **kwargs):
        obj = self.obj.evaluate()
        if not isinstance(obj, LoxInstance):
            raise RuntimeError("Only instances have fields.")

        val = self.val.evaluate()
        obj.set(self.attr.lexeme, val)
        return val

    def resolve(self):
        self.obj.resolve()
        self.val.resolve()


class This(Expression):

    def __init__(self, this: Token):
        self.this = this

    def resolve(self):
        # TODO: isn't INITIALIZER also a valid class type here?
        if INTERPRETER.resolver.current_class != ClassType.CLASS:
            raise RuntimeError(
                f"[line {self.this.line}] Error at 'this': Can't use 'this' outside of a class."
            )

        INTERPRETER.resolver.resolve_local(self, "this")
    
    def evaluate(self):
        return self.this.evaluate(expr=self)


class Super(Expression):

    def __init__(self, super: Token, method: Token):
        self.super = super
        self.method = method

    def resolve(self):
        # TODO: isn't INITIALIZER also a valid class type here?
        if INTERPRETER.resolver.current_class != ClassType.CLASS:
            raise RuntimeError(
                f"[line {self.super.line}] Error at 'super': Can't use 'super' outside of a class."
            )

        INTERPRETER.resolver.resolve_local(self, "super")
    
    def evaluate(self):
        # super is a LoxClass and get_method will retrieve the relevant LoxFunction.
        # We don't want to execut the method yet so we don't call evaluate on LoxFunction.
        super = self.super.evaluate(expr=self)
        return super.get_method(self.method.lexeme)


def clock() -> int:
    """Return the current time in seconds since January 1, 1970 UTC."""
    return int(datetime.now().timestamp())


class DefinesInstanceAttrs(type):
    """Metaclass that forces instances of its target classes to define one or more attributes.
    Different from abstractmethod/property combo in that the attr value can be different for each
    instance. Subclasses can provide additional expected attrs but not remove parents' expected
    attrs:

    class Foo(metaclass=DefinesInstanceAttrs, expected=['a', 'b']):
        a = 1
        def __init__(self, b):
            self.b = b

    # Note that this must now contain attrs ['a', 'b', 'c'] (the union of parent and child
    # "expected" attrs).
    class Bar(Foo, expected=['c'])  
        c = 9
    """

    __attr_name__ = "__expected__"

    def __new__(mcls, name, bases, namespace, expected: Optional[list[str]] = None):
        new_cls = super().__new__(mcls, name, bases, namespace)
        expected = list(set(sum(
            [getattr(base, mcls.__attr_name__, []) for base in bases], expected or []
        )))
        new_cls.__expected__ = expected
        standard_init = new_cls.__init__

        @wraps(standard_init)
        def custom_init(self, *args, **kwargs):
            standard_init(self, *args, **kwargs)
            missing_attrs = [
                exp_name for exp_name in getattr(self, mcls.__attr_name__)
                if not hasattr(self, exp_name)
            ]
            if missing_attrs:
                raise AttributeError(f"class has metaclass={mcls.__name__} and is missing required attr(s): {missing_attrs}")

        new_cls.__init__ = custom_init
        return new_cls

class LoxCallable(Expression, metaclass=DefinesInstanceAttrs, expected=["arity"]):
    """LoxCallable().evaluate() returns a python object.

    Example
    foo()
    """
    # TODO: may need to flesh this out later, at this point not really clear what purpose it serves
    # beyond a function. Maybe will become clear once we implement user-defined funcs and/or support
    # arity method (len(args)).


class NativeClock(LoxCallable):
    arity = 0

    def evaluate(self, *args) -> float:
        """Returns a python object (current time in seconds)."""
        # Technically takes 0 args but maybe makes sense to include this for interface consistency.
        return clock()

    def __str__(self) -> str:
        # TODO: idk if this is book's desired format.
        return "<NativeClock>"


class Call(Expression):

    def __init__(self, callee: Expression, right_parens: Token, args: list[Expression]):
        self.callee = callee
        self.right_parens = right_parens
        self.args = args

    def __str__(self) -> str:
        # TODO check if this is format book wants
        return f"({self.callee} {self.args})"

    def evaluate(self, **kwargs):
        py_args = [arg.evaluate() for arg in self.args]
        lox_callable = self.callee.evaluate()
        if not isinstance(lox_callable, LoxCallable):
            raise RuntimeError(
                f"Can only call functions and classes.\n[line {self.right_parens.line}]"
            )
        if len(py_args) != lox_callable.arity:
            raise RuntimeError(
                f"Expected {lox_callable.arity} arguments but got {len(py_args)}.\n"
                f"[line {self.right_parens.line}]"
            )
        return lox_callable.evaluate(*py_args)

    def resolve(self):
        self.callee.resolve()
        for arg in self.args:
            arg.resolve()


class Grouping(Expression):
    """
    Example
    (foo)
    """

    def __init__(self, val: Expression):
        self.val = val

    def __str__(self) -> str:
        # Book-specified format.
        return "(group " + str(self.val) + ")"

    def evaluate(self):
        """This returns the relevant python value, not the lox value. E.g. None rather than
        ReservedTokenTypes.NIL.
        """
        return self.val.evaluate()

    def resolve(self):
        self.val.resolve()


class Assign(Expression):
    """Assign a value to an existing variable.

    Example:
    x = "bar"
    """

    def __init__(self, name: Token, expr: Expression):
        self.name = name
        self.expr = expr
        # This gets updated when evaluate() is called.
        self.val = SENTINEL

    def __str__(self) -> str:
        return f"({self.name.non_null_literal} = {self.expr})"

    def evaluate(self, env: Optional[Environment] = None):
        """Evaluates the value of the variable and returns the corresponding python object."""
        depth = INTERPRETER.locals.get(self.expr, None)
        self.val = self.expr.evaluate()

        if depth is None:
            INTERPRETER.global_env.update_state(self.name.lexeme, self.val, is_declaration=False)
        else:
            INTERPRETER.env.update_state_at(self.name.lexeme, self.val, depth)
        return self.val

    def resolve(self):
        self.expr.resolve()
        INTERPRETER.resolver.resolve_local(self.expr, self.name.lexeme)


class Statement:
    """Statements 'do things' that have side effects, but do not return a value.
    This is in contrast to expressions, which compute and return a value.

    TODO: maybe should move this to Parser or Interprete docstring at some point?
    Grammar:

    program        → declaration* EOF ;
    declaration    → classDecl
               | funDecl
               | varDecl
               | statement;
    funDecl        → "fun" function ;
    function       → IDENTIFIER "(" parameters? ")" block ;
    statement      → exprStmt
               | forStmt
               | ifStmt
               | printStmt
               | returnStmt
               | whileStmt
               | block ;

    returnStmt     → "return" expression? ";" ;
    block          → "{" declaration* "}" ;
    printStmt      → "print" expression ";" ;
    varDecl        → "var" IDENTIFIER ( "=" expression )? ";" ;
    """

    def resolve(self):
        """Used by Resolver to determine which scope a variable is referring to at parse time.
        """


class ExpressionStatement(Statement):
    
    def __init__(self, expr: Expression):
        self.expr = expr

    def __str__(self) -> str:
        return f"({self.expr};)"
    
    def evaluate(self, *arg, **kwargs) -> None:
        self.expr.evaluate()

    def resolve(self):
        self.expr.resolve()


class PrintStatement(Statement):
    
    def __init__(self, expr: Expression):
        self.expr = expr

    def __str__(self) -> str:
        return f"(print {self.expr})"

    def evaluate(self, *args, **kwargs) -> None:
        print(to_lox_dtype(self.expr.evaluate()))

    def resolve(self):
        self.expr.resolve()

    
class VariableDeclaration(Statement):
    """Creates a variable (global by default).
    """
    
    def __init__(self, name: Token, expr: Expression) -> None:
        self.name = name
        self.expr = expr
        # We will set this in evaluate. Don't use default=None because evaluate could return None.
        self.value = SENTINEL

    # TODO: getting displayed like "((a = foo);)", guessing that may not be correct.
    def __str__(self) -> str:
        return f"({self.name.lexeme} = {self.expr})"

    def evaluate(self, env: Optional[Environment] = None) -> None:
        env = env or INTERPRETER.env
        # Only want to evaluate once, not every time we reference a variable.
        self.value = self.expr.evaluate()
        env.update_state(self.name.lexeme, self.value, is_declaration=True)

    def resolve(self):
        INTERPRETER.resolver.declare(self.name)
        # TODO is this necessary? Book does this but I type hinted expr as non-optional in init.
        # Probably the type hint is just wrong bc I do remember declaring vars without a value works.
        if self.expr:
            self.expr.resolve()
        INTERPRETER.resolver.define(self.name)


class Block(Statement):
    """Section of code enclosed in curly braces that defines a new temporary scope.
    """
    
    def __init__(self, statements: list[Statement]) -> None:
        self.statements = statements

    # TODO not sure what desired format actually is
    def __str__(self) -> str:
        return f"({self.statements})"

    def evaluate(self, *args, _new_env: bool = True, **kwargs) -> None:
        # args, kwargs is needed because statement.evaluate is always passed an env arg.
        # But block always creates a new one anyway so doesn't need to use that.
        # TODO: confirm if above is still accurate, looks like this is the only place we call
        # "env=" in this project...
        context_manager = INTERPRETER.new_env if _new_env else INTERPRETER.existing_env
        with context_manager(**kwargs) as env:
            # print('[block] new env:', id(env), 'global env:', id(INTERPRETER.global_env), '\n', self.statements, '\n', _new_env) # TODO
            for statement in self.statements:
                try:
                    statement.evaluate(env=env)
                except Return as e:
                    raise e

    def resolve(self, is_function_body: bool = False):
        with maybe_context_manager(INTERPRETER.resolver.scope, enable=not is_function_body):
            for statement in self.statements:
                statement.resolve()


class Class(Statement):
    """Represents a class *declaration*.
    """

    def __init__(self, name: Token, methods: list["Function"], parent: Optional[Variable] = None):
        self.name = name
        self.methods = methods
        self.parent = parent

    def __str__(self) -> str:
        return f"{type(self).__name__}(name={self.name}, methods={self.methods}, " \
               f"parent={self.parent})"

    def evaluate(self, *args, **kwargs):
        methods = {
            method.name.lexeme:
            LoxFunction(method, INTERPRETER.env, is_init=method.name.lexeme == "init")
            for method in self.methods
        }
        parent_cls = None
        if self.parent:
            parent_cls = self.parent.evaluate()
            if not isinstance(parent_cls, LoxClass):
                raise RuntimeError(f"Superclass must be a class.\n[line {self.name.line}]")

        cls = LoxClass(self, methods, parent_cls=parent_cls)
        INTERPRETER.env.update_state(self.name.lexeme, cls, is_declaration=True)
        return cls

    def resolve(self):
        if self.name.lexeme == getattr(getattr(self.parent, "identifier", None), "lexeme", None):
            raise ResolutionError(f"[line {self.name.line}] Error at {self.name.lexeme!r}: "
                                  "A class can't inherit from itself.")

        with INTERPRETER.resolver.inside_class(ClassType.CLASS):
            INTERPRETER.resolver.declare(self.name)
            INTERPRETER.resolver.define(self.name)
            if self.parent:
                self.parent.resolve()
            with INTERPRETER.resolver.scope():
                # We don't actually use the line number, this is a dummy value.
                INTERPRETER.resolver.define(Token("this", -1, token_type=ReservedTokenTypes.THIS))
                INTERPRETER.resolver.define(Token("super", -1, token_type=ReservedTokenTypes.SUPER))
                for method in self.methods:
                    INTERPRETER.resolver.resolve_function(
                        method,
                        FunctionType.INITIALIZER if method.name.lexeme == "init"
                        else FunctionType.METHOD
                    )

    
class While(Statement):

    def __init__(self, condition: Expression, statement: Statement):
        self.condition = condition
        self.statement = statement

    def __str__(self) -> str:
        # TODO not sure if this is book's desired format
        return f"(while {self.condition} {self.statement}"

    def evaluate(self, *args, **kwargs) -> None:
        # TODO: do I need to be passing env to all these evaluate calls? Pretty sure no (and tests
        # pass without doing this) but good to confirm.
        while truthy(self.condition.evaluate()):
            self.statement.evaluate()

    def resolve(self):
        self.condition.resolve()
        self.statement.resolve()


class For(Statement):
    
    def __init__(
            self,
            initializer: Optional[VariableDeclaration],
            condition: ExpressionStatement,
            incrementer: Optional[Expression],
            statement: Statement,
        ):
        self.initializer = initializer
        self.condition = condition
        self.incrementer = incrementer
        self.statement = statement

    def __str__(self) -> str:
        # TODO check if this matches what book expects
        return f"(for {self.initializer} {self.condition} {self.incrementer})"

    def evaluate(self, *args, **kwargs) -> None:
        if self.initializer is not None:
            self.initializer.evaluate()
        # TODO book says condition can be null which seems weird. Could make that change here or
        # could use its desugaring route and use while loop. I think to do that we'd basically:
        # - create a Block with [statement, incrementer]
        # - pass (condition, block) to While constructor
        # - create another Block by passing in [initializer, while]
        # but can check book to confirm. ALTERNATIVELY, can leave this for later and proceed to next
        # stage, just depends on what mood you're in to do next.
        while truthy(self.condition.evaluate()):
            self.statement.evaluate()
            if self.incrementer is not None:
                self.incrementer.evaluate()

    def resolve(self):
        if self.initializer:
            self.initializer.resolve()
        self.condition.resolve()
        if self.incrementer:
            self.incrementer.resolve()
        self.statement.resolve()

class IfStatement(Statement):

    def __init__(self, condition: Expression, value: Statement,
                 other_value: Optional[Statement] = None):
        self.condition = condition
        self.value = value
        self.other_value = other_value

    def evaluate(self, *args, **kwargs):
        # TODO: do I need to be passing env to all these evaluate calls? Pretty sure no but good to
        # confirm.
        condition = self.condition.evaluate()
        if truthy(condition):
            return self.value.evaluate()
        elif self.other_value:
            return self.other_value.evaluate()

    def __str__(self) -> str:
        res = f"(if {self.condition} {self.value}"
        if self.other_value:
            res += f" {self.other_value}"
        return res + ")"

    def resolve(self):
        self.condition.resolve()
        self.value.resolve()
        if self.other_value:
            self.other_value.resolve()


class ReturnStatement(Statement):
    
    def __init__(self, line_num: int, expr: Optional[Expression] = None):
        self.line_num = line_num
        self.expr = expr

    def evaluate(self, *args, **kwargs):
        # Returns the python object resulting from evaluating a function.
        # TODO: forget what args/kwargs get passed into statement.evaluate, need to confirm whether
        # these should be passed to expr below.
        val = None
        if self.expr:
            val = self.expr.evaluate()
        raise Return(val)

    def __str__(self) -> str:
        # TODO prob need to change this format, check what book wants here if it says
        return f"Return({self.expr})"

    def resolve(self):
        prefix = f"[line {self.line_num}] Error at 'return':"
        if INTERPRETER.resolver.current_function == FunctionType.INITIALIZER:
            if self.expr is not None:
                raise RuntimeError(f"{prefix} Can't return a value from an initializer.")
        elif INTERPRETER.resolver.current_function not in {
            FunctionType.FUNCTION,
            FunctionType.METHOD
        }:
            raise RuntimeError(
                f"{prefix} Can't return from top-level code."
            )

        if self.expr:
            self.expr.resolve()


class LoxFunction(LoxCallable):
    """Stores the actual Lox code that the function will execute. Evaluating this will evaluate the
    function. (This is in contrast to Function (which this does wrap) which is used to parse
    function definitions rather than calls.)
    """

    def __init__(self, func: "Function", nonlocal_env: Optional[Environment] = None,
                 is_init: bool = False):
        # func typehint is in quotes because we haven't defined Function yet, and Function also uses
        # LoxFunction as a type hint so moving this class down wouldn't resolve the issue.
        self.func = func
        self.nonlocal_env = nonlocal_env
        self.is_init = is_init
        self.arity = len(self.func.params)

    def evaluate(self, *args, **kwargs):
        # TODO does lox suppport both positional
        # and named args tho? gpt says positional only, let's try that for now.
        with INTERPRETER.new_env(parent=self.nonlocal_env or self.func.definition_env) as env:
            py_kwargs = {param.lexeme: arg for param, arg in zip(self.func.params, args)}
            try:
                result = self.func.body.evaluate(**py_kwargs, _new_env=False)
                if self.is_init:
                    return INTERPRETER.env.read_state_at("this", 1)
                return result
            except Return as e:
                if self.is_init:
                    return INTERPRETER.env.read_state_at("this", 1)
                return e.value

    def bind(self, instance: "LoxInstance") -> "LoxFunction":
        """Attach a method to a class instance, making the instance available via `this`."""
        # TODO don't fully understand whether to pick definition_env or nonlocal_env here, need to
        # refresh my memory of what purpose each serves.
        # print(
        #     '[bind]',
        #      '\n\tnonlocal:', id(self.nonlocal_env), getattr(self.nonlocal_env, 'state', {}),
        #      '\n\tdefinition:', id(self.func.definition_env),  getattr(self.func.definition_env, 'state', {}),
        #      '\n\tglobal:', id(INTERPRETER.global_env), INTERPRETER.global_env.state
        # ) # TODO
        with INTERPRETER.new_env(parent=self.func.definition_env) as env:
            env.update_state("this", instance, is_declaration=True)
            env.update_state("super", instance.cls.parent_cls, is_declaration=True) # TODO testing
            return LoxFunction(func=self.func, nonlocal_env=env, is_init=self.is_init)

    def __str__(self) -> str:
        return f"<fn {self.func.name.lexeme}>"


class LoxClass(LoxCallable):
    """The class object we use at runtime. (This is in contrast to Class which is used to parse
    class *definitions*.)
    """

    def __init__(self, cls_declaration: "Class", methods: dict[str, LoxFunction],
                 parent_cls: Optional["LoxClass"] = None):
        self.cls_declaration = cls_declaration
        self.methods = methods
        self.parent_cls = parent_cls
        self.arity = self._set_arity()
        
    
    def _set_arity(self):
        init = self.get_method("init")
        if init:
            return init.arity
        return 0

    def evaluate(self, *args, **kwargs):
        instance = LoxInstance(self)
        init = self.get_method("init")
        if init:
            # Need to bind init manually here because it won't be invoked like other methods, i.e.
            # the user will not call MyClass.init().
            init.bind(instance).evaluate(*args)
        return instance

    def get_method(self, name: str) -> LoxFunction:
        if name in self.methods:
            return self.methods[name]
        if self.parent_cls:
            return self.parent_cls.get_method(name)

    def __str__(self) -> str:
        return self.cls_declaration.name.lexeme


class LoxInstance:

    def __init__(self, cls: LoxClass):
        self.cls = cls
        self.attrs = {}

    def __str__(self) -> str:
        return f"{self.cls.cls_declaration.name.lexeme} instance"

    def get(self, name: str) -> Any:
        if name in self.attrs:
            return self.attrs[name]

        method = self.cls.get_method(name)
        if method:
            return method.bind(self)

        raise RuntimeError(f"Undefined property {name!r}.")
    
    def set(self, name: str, val: Any):
        self.attrs[name] = val


class Function(Statement):
    """For parsing a user-defined function (FunctionDeclaration would also be a fitting name).
    In contrast, LoxFunction is used at runtime to execute such a function.
    """

    # TODO: statements are defined in parser.py. Could see if we can move them to a separate module
    # or leave type hint as str.
    def __init__(self, name: Token, params: list[Token], body: Block):
        self.name = name
        self.params = params
        self.body = body
        self.definition_env = None

    def evaluate(self, *args, **kwargs) -> LoxFunction:
        # Returns a LoxFunction object, NOT the result of calling the function.
        # TODO: note that gpt claims I should be using interp.env as fallback instead of None.
        # But seems like that would break my closure impelmentation even if I discarded Resolution
        # stuff? Idk, tried it and it did not change my current resolution error.
        # TODO: when testing closures, I saw saw instances where this was the global env even though
        # the func was defined inside a different func (need to confirm but I assume this creates a
        # new env, certainly creates a new scope). Also maybe this is unneeded entirely with
        # resolution?
        self.definition_env = INTERPRETER.env
        func = LoxFunction(self, kwargs.get("env", None), is_init=False)
        INTERPRETER.env.update_state(self.name.lexeme, func, is_declaration=True)
        return func

    def resolve(self):
        INTERPRETER.resolver.declare(self.name)
        INTERPRETER.resolver.define(self.name)
        INTERPRETER.resolver.resolve_function(self)

    def __str__(self) -> str:
        return f"Function(name={self.name})"


def boolean_lexeme(val: bool) -> str:
    """Map a python boolean to a string containing the appropriate lox lexeme. (In practice,
    this is currently equivalent to str(val).lower(), but that felt a little riskier in case we
    ever changed how lox represents bools.)
    """
    return [ReservedTokenTypes.FALSE.lexeme, ReservedTokenTypes.TRUE.lexeme][val]


def to_lox_dtype(val: Any) -> Union[str, int, float]:
    """Convert a python object to its corresponding lox datatype.
    Only a few types need to be converted.
    """
    # Have to be a little careful here - initially tried to define a dict mapping python vals to
    # lox vals but python treats bools as ints so it's easy to get unexpected results that way.
    if isinstance(val, bool):
        return boolean_lexeme(val)
    if val is None:
        return ReservedTokenTypes.NIL.lexeme
    return val


BUILTIN_FUNCTIONS = {
    "clock": NativeClock(),
}


class Interpreter:

    def __init__(self):
        self.env = self.global_env = GLOBAL_ENV
        self.locals = {}
        self.resolver = Resolver(interpreter=self)
        for name, func in BUILTIN_FUNCTIONS.items():
            self.env.update_state(name, func, is_declaration=True)

    @contextmanager
    def new_env(self, parent: Optional[Environment] = None, **kwargs):
        prev_env = self.env
        try:
            self.env = Environment(parent=parent or prev_env)
            for name, val in kwargs.items():
                self.env.update_state(name, val, is_declaration=True)
            yield self.env
        finally:
            self.env = prev_env

    @contextmanager
    def existing_env(self, **kwargs):
        try:
            for k, v in kwargs.items():
                self.env.update_state(k, v, is_declaration=True)
            yield self.env
        finally:
            pass

    def resolve(self, expr: Expression, depth: int):
        self.locals[expr] = depth

    def resolve_all(self, expressions: list[Expression]):
        for expr in expressions:
            self.resolver.resolve(expr)


INTERPRETER = Interpreter()
