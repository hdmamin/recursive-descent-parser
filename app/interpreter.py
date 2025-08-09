from contextlib import contextmanager
from datetime import datetime
from typing import Any, Optional, Union

from app.environment import GLOBAL_ENV, Environment
from app.exceptions import ParsingError
from app.lexer import TokenTypes, ReservedTokenTypes, Token
from app.utils import truthy, is_number, SENTINEL


class Return(Exception):
    """Exception raised when a return statement is evaluated."""

    def __init__(self, value: Any):
        self.value = value


class Expression:
    
    def __str__(self) -> str:
        return f"{type(self)}()"


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
        return self.identifier.evaluate()


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


def clock() -> int:
    """Return the current time in seconds since January 1, 1970 UTC."""
    return int(datetime.now().timestamp())


class LoxCallable(Expression):
    """LoxCallable().evaluate() returns a python object.

    Example
    foo()
    """
    # TODO: may need to flesh this out later, at this point not really clear what purpose it serves
    # beyond a function. Maybe will become clear once we implement user-defined funcs and/or support
    # arity method (len(args)).


class NativeClock(LoxCallable):

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

    def evaluate(self):
        py_args = [arg.evaluate() for arg in self.args]
        lox_callable = self.callee.evaluate()
        return lox_callable.evaluate(*py_args)


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
        env = env or INTERPRETER.env
        self.val = self.expr.evaluate()
        env.update_state(self.name.non_null_literal, self.val, is_declaration=False)
        return self.val


class Statement:
    """Statements 'do things' that have side effects, but do not return a value.
    This is in contrast to expressions, which compute and return a value.

    TODO: maybe should move this to Parser or Interprete docstring at some point?
    Grammar:

    program        → declaration* EOF ;
    declaration    → funDecl
                | varDecl
                | statement ;
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


class ExpressionStatement(Statement):
    
    def __init__(self, expr: Expression):
        self.expr = expr

    def __str__(self) -> str:
        return f"({self.expr};)"
    
    def evaluate(self, *arg, **kwargs) -> None:
        self.expr.evaluate()


class PrintStatement(Statement):
    
    def __init__(self, expr: Expression):
        self.expr = expr

    def __str__(self) -> str:
        return f"(print {self.expr})"

    def evaluate(self, *args, **kwargs) -> None:
        print(to_lox_dtype(self.expr.evaluate()))

    
class VariableDeclaration(Statement):
    """Creates a variable (global by default).
    """
    
    def __init__(self, name: str, expr: Expression) -> None:
        self.name = name
        self.expr = expr
        # We will set this in evaluate. Don't use default=None because evaluate could return None.
        self.value = SENTINEL

    # TODO: getting displayed like "((a = foo);)", guessing that may not be correct.
    def __str__(self) -> str:
        return f"({self.name} = {self.expr})"

    def evaluate(self, env: Optional[Environment] = None) -> None:
        env = env or INTERPRETER.env
        # Only want to evaluate once, not every time we reference a variable.
        self.value = self.expr.evaluate()
        env.update_state(self.name, self.value, is_declaration=True)


class Block(Statement):
    """Section of code enclosed in curly braces that defines a new temporary scope.
    """
    
    def __init__(self, statements: list[Statement]) -> None:
        self.statements = statements

    # TODO not sure what desired format actually is
    def __str__(self) -> str:
        return f"({self.statements})"

    def evaluate(self, *args, **kwargs) -> None:
        # args, kwargs is needed because statement.evaluate is always passed an env arg.
        # But block always creates a new one anyway so doesn't need to use that.
        # TODO: confirm if above is still accurate, looks like this is the only place we call
        # "env=" in this project...
        with INTERPRETER.new_env(**kwargs) as env:
            for statement in self.statements:
                try:
                    statement.evaluate(env=env)
                except Return as e:
                    raise e

    
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


class ReturnStatement(Statement):
    
    def __init__(self, expr: Optional[Expression] = None):
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


class LoxFunction(LoxCallable):
    """Stores the actual Lox code that the function will execute. Evaluating this will evaluate the
    function. (This is in contrast to Function (which this does wrap) which is used to parse
    function definitions rather than calls.)
    """

    def __init__(self, func: "Function", nonlocal_env: Optional[Environment] = None):
        self.func = func
        self.nonlocal_env = nonlocal_env

    def evaluate(self, *args, **kwargs):
        # TODO: currently py_kwargs line errors out bc it's trying to evaluate the param variable
        # in the scope that contains the function and we have not defined this as anything. BUT
        # *args is being passed the actual arg, forget how but it makes sense. And this appears to
        # be a python val, not a lox var. So I think we need to resolve args/kwargs with the
        # expected params rather than calling param.evaluate. Does lox suppport both positional
        # and named args tho? gpt says positional only, let's try that for now.
        with INTERPRETER.new_env(**getattr(self.nonlocal_env, "state", {})) as env:
            py_kwargs = {param.lexeme: arg for param, arg in zip(self.func.params, args)}
            try:
                return self.func.body.evaluate(**py_kwargs)
            except Return as e:
                return e.value

    def __str__(self) -> str:
        return f"<fn {self.func.name.lexeme}>"


class Function(Statement):
    """For parsing a user-defined function. In contrast, LoxFunction is used at runtime to execute
    such a function.
    """

    # TODO: statements are defined in parser.py. Could see if we can move them to a separate module
    # or leave type hint as str.
    def __init__(self, name: Token, params: list[Token], body: Block):
        self.name = name
        self.params = params
        self.body = body

    def evaluate(self, *args, **kwargs) -> LoxFunction:
        # Returns a LoxFunction object, NOT the result of calling the function.
        func = LoxFunction(self, kwargs.get("env", None))
        INTERPRETER.env.update_state(self.name.lexeme, func, is_declaration=True)
        return func


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
    "clock": NativeClock(), # TODO need to define a clock expression I think
}


class Interpreter:

    def __init__(self):
        self.env = GLOBAL_ENV
        for name, func in BUILTIN_FUNCTIONS.items():
            self.env.update_state(name, func, is_declaration=True)

    @contextmanager
    def new_env(self, **kwargs):
        # TODO: trying to allow passing in kwargs to allow LoxFunction to provide args in the new
        # env that its body (Block) will create. atm we assume kwargs map name (str) to val (python
        # obj).
        prev_env = self.env
        try:
            self.env = Environment(parent=prev_env)
            for name, val in kwargs.items():
                self.env.update_state(name, val, is_declaration=True)
            yield self.env
        finally:
            self.env = prev_env


INTERPRETER = Interpreter()
