from typing import Any, Optional, Union

from app.environment import Environment
from app.interpreter import INTERPRETER
from app.lexer import Token, TokenTypes, ReservedTokenTypes, TokenType


SENTINEL = object()


class Expression:
    
    def __str__(self) -> str:
        return f"{type(self)}()"


# TODO: seems like evaluate() needs to use token lexemes, not literals. Will need to strip
# quotes from strings.
# TODO: include docstring examples of each expr type, keep forgetting what each is.
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
            # Horribly hacky but we can't just return `not truthy(right)` because that will return a
            # python bool rather than the string codecrafters expects.
            # TODO: problem is that now Token.evaluate() returns a python dtype but this returns a
            # str bc tests require that. But Binary.evaluate() calls unary.evaluate() so it expects
            # evaluate() to return a python var, not a str.
            # gpt suggests making evaluate() ALWAYS return a python val and then handling formatting
            # totally separately. So I think we want to remove all the boolean_lexeme usages from
            # inside eval methods and handle that at the end. Could do one big general purpose
            # python_val_to_lox_val() func OR use __str__ or __repr__ or to_lox() method in each
            # expr class.
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
        # TODO: not sure why but book seems to want order (lexeme, left, right). Trying it out
        # but don't really understand why.
        return "(" + self.val.non_null_literal + " " + str(self.left) + " " + str(self.right) + ")"

    def evaluate(self):
        """This returns the relevant python value, not the lox value. E.g. None rather than
        ReservedTokenTypes.NIL.
        """
        left = self.left.evaluate()
        right = self.right.evaluate()

        # TODO: maybe can give Token/TokenType an optional "op" field so we can just call
        # self.val.op(left, right)?
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

class Grouping(Expression):
    """
    Example
    (foo)
    """

    def __init__(self, val: Expression):
        self.val = val

    def __str__(self) -> str:
        # TODO: don't really understand why book wants us to include word "group" here, format
        # doesn't really seem to match rest of expressions. But let's see how this looks.
        return "(group " + str(self.val) + ")"

    def evaluate(self):
        """This returns the relevant python value, not the lox value. E.g. None rather than
        ReservedTokenTypes.NIL.
        """
        return self.val.evaluate()


class Assign(Expression):
    """
    x = "bar"
    """

    # TODO: seems like I need to rm env from init, but if we don't call env.set there then
    # evaluate cannot use env.contains. Maybe can redefine contains to work differently?
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


    Grammar:

    program        → declaration* EOF ;

    declaration    → varDecl
                | statement ;

    statement      → exprStmt
                | ifStmt
                | printStmt 
                | block;
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
        with INTERPRETER.new_env() as env:
            for statement in self.statements:
                statement.evaluate(env=env)

    
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
            res += " " + self.other_value
        return res + ")"


class ParsingError(Exception):
    """Raise when the parser hits an invalid token given the current state."""


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



def is_number(val: Any) -> bool:
    """Return True if a value is an int/float, false otherwise. Note that we return a python bool,
    not lox's TokenTypes.BOOL, and we expect a python variable input, not a Token or Token.lexeme.
    """
    # Need to handle bool case separately because python counts bools as ints.
    return isinstance(val, (int, float)) and not isinstance(val, bool)


def truthy(val: Any) -> bool:
    """Determine whether a value is truthy. In Lox, false and nil are considered falsy,
    everything else is considered truthy.

    Notice that this returns a python boolean which is useful for in-program logic. But if you are
    trying to print to stdout for codecrafters tests, bools must be represented as lowercase
    strings, not python bools.

    Parameters
    ----------
    val : Any
        A python object, not a literal or Token. E.g we expect False rather than "false" or
        ReservedTokenTypes.FALSE.
    """
    return val not in (False, None)


# TODO: rm decorator once done debugging. For now leave it so can easily comment it on/off.
from app.debugging import decorate_methods, verbose
# @decorate_methods(verbose)
class Parser:
    """
    Each precedence level in our order of operations requires its own method.
    In the grammar below, "primary" has the "highest" precedence, meaning our parser calls it first
    and resolves it last.

    Grammar
    -------
    expression     → assignment ;
    assignment     → IDENTIFIER "=" assignment
                | logic_or ;
    logic_or       → logic_and ( "or" logic_and )* ;
    logic_and      → equality ( "and" equality )* ;
    equality       → comparison ( ( "!=" | "==" ) comparison )* ;
    comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    term           → factor ( ( "-" | "+" ) factor )* ;
    factor         → unary ( ( "/" | "*" ) unary )* ;
    unary          → ( "!" | "-" ) unary
                | primary ;
    primary        → "true" | "false" | "nil" | NUMBER | STRING
               | "(" expression ")"
               | IDENTIFIER ;
    """

    def __init__(self, tokens: list[Token]):
        # TODO: book doesn't really address what to do with spaces here so just skipping them for
        # now, otherwise our grammar breaks down.
        self.tokens = [token for token in tokens if token.token_type != TokenTypes.SPACE]
        self.max_idx = len(self.tokens) - 1
        self.curr_idx = 0

    def match(self, *token_types: TokenType) -> bool:
        """Check if the current token has one fo the expected token_types. If so, increment the
        index and return True. Otherwise return False without incrementing.
        """
        if self.curr_idx <= self.max_idx and self.tokens[self.curr_idx].token_type in token_types:
            self.curr_idx += 1
            return True
        return False

    def current_token(self) -> Token:
        if self.curr_idx <= self.max_idx:
            return self.tokens[self.curr_idx]
        raise ParsingError(
            f"Parsing error at line {self.previous_token().line}: "
            f"Invalid index {self.curr_idx}, max_idx is {self.max_idx}."
        )

    def previous_token(self) -> Token:
        if self.curr_idx - 1 <= self.max_idx:
            return self.tokens[self.curr_idx - 1]
        raise ParsingError(
            f"Parsing error: Invalid index {self.curr_idx - 1}, max_idx is {self.max_idx}."
        )

    def parse(self, mode: str = "run") -> dict:
        """Parse all tokens in the source code into expressions.

        Parameters
        ----------
        mode : str
            One of ("run", "evaluate", "parse") indicating what we're trying to do to the program.
            If "evaluate" or "parse",
            we construct a list of Expressions (do not need trailing semicolons). If "run", we
            construct a list of Statements (do require trailing semicolons).

        Returns
        -------
        TODO update docs
        dict
            expressions: list[Expression]
            statements: list[Statement]
            success: bool
            error: Optional[Exception]

            Note that we do NOT try to evalute the expressions yet. We may still encounter
            additional errors when we do.
        """
        method_name = {
            "parse": "expression",
            # TODO testing: previously was expression, tried switching to declaration 
            # but I think that broke our ability to handle expressions without trailing semicolons.
            # "evaluate": "declaration",
            "evaluate": "expression",
            "run": "declaration",
        }[mode]
        method = getattr(self, method_name)
        res = {
            f"{method_name}s": [],
            "success": True,
            "errors": [],
        }
        prev_idx = -1
        while self.curr_idx <= self.max_idx:
            # Avoid getting stuck in infinite loop on parsing errors that don't hit synchrnoize().
            if self.curr_idx == prev_idx:
                self.curr_idx += 1
                continue

            try:
                res[f"{method_name}s"].append(method())
            # TODO: may need to handle these differently, syntaxerrors are raised when statement
            # parsing fails while parsingerrors are raised when expression parsing fails.
            except (ParsingError, SyntaxError) as e:
                res["success"] = False
                res["errors"].append(e)
                # TODO: Previously had this working with break enabled, but was getting wrong number
                # of errors in latest stage.
                # Need to figure out some logic to skip ahead to the next valid expr/decl etc,
                # right now we keep trying to parse the next token and this results in ghost errors.
                # break

            prev_idx = self.curr_idx

        # TODO: will need to change this logic once we implement more complex statement types.
        # TODO: or update res key name to use mode if we end up needing to keep this.
        # res["expressions"] = [statement.expr for statement in res["statements"]]
        return res

    def expression(self) -> Expression:
        """
        Rule:
        expression → equality ;
        """
        # Call the highest precedence (last to be evaluated) operation.
        return self.assignment()

    def primary(self) -> Union[Literal, Grouping, Variable]:
        """
        Example:
        "foo"

        Rule:
        primary        → "true" | "false" | "nil" | NUMBER | STRING
               | "(" expression ")"
               | IDENTIFIER ;
        """
        token = self.current_token()
        reserved_types = (ReservedTokenTypes.FALSE, ReservedTokenTypes.TRUE, ReservedTokenTypes.NIL)
        other_types = (TokenTypes.NUMBER, TokenTypes.STRING)
        if self.match(*reserved_types, *other_types):
            return Literal(token)
        
        if self.match(TokenTypes.LEFT_PAREN):
            expr = self.expression()
            if not self.match(TokenTypes.RIGHT_PAREN):
                # TODO: this is getting raised by the third line in current test case open in vim.
                # Expects a ), getting an equal sign.
                # Clue: seems like we finish processing (a=false) but then we end up trying to
                # process the right parents again which is obviously invalid. Vaguely recall
                # decrementing an index somewhere, maybe that's to blame.
                raise TypeError(
                    f"Expected type {TokenTypes.RIGHT_PAREN}, found "
                    f"{self.current_token().token_type}."
                )
            return Grouping(expr)

        if self.match(TokenTypes.IDENTIFIER):
            return Variable(token)

        raise ParsingError(f"[line {token.line}] Error at {token.lexeme}.")

    def unary(self) -> Unary:
        """
        Example:
        !foo

        Rule:
        unary → ( "!" | "-" ) unary
               | primary ;
        """
        token = self.current_token()
        if self.match(TokenTypes.BANG, TokenTypes.MINUS):
            return Unary(token, self.unary())

        return self.primary()

    def factor(self) -> Binary:
        """
        Example:
        3 / 4        

        Rule:
        factor → unary ( ( "/" | "*" ) unary )* ;
        """
        left = self.unary()
        while self.match(TokenTypes.SLASH, TokenTypes.STAR):
            left = Binary(left, self.previous_token(), self.unary())
        return left

    def term(self) -> Binary:
        """
        Example:
        3 + 4        

        Rule:
        term → factor ( ( "-" | "+" ) factor )* ;
        """
        left = self.factor()
        while self.match(TokenTypes.MINUS, TokenTypes.PLUS):
            left = Binary(left, self.previous_token(), self.factor())
        return left

    def comparison(self) -> Binary:
        """
        Example:
        foo >= 3

        Rule:
        comparison → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
        """
        left = self.term()
        while self.match(
            TokenTypes.GREATER,
            TokenTypes.GREATER_EQUAL,
            TokenTypes.LESS,
            TokenTypes.LESS_EQUAL
        ):
            left = Binary(left, self.previous_token(), self.term()) 
        return left

    def equality(self) -> Binary:
        """
        Example:
        foo == bar

        Rule:
        equality → comparison ( ( "!=" | "==" ) comparison )* ;
        """
        left = self.comparison()
        while self.match(TokenTypes.EQUAL_EQUAL, TokenTypes.BANG_EQUAL):
            left = Binary(left, self.previous_token(), self.comparison())
        return left

    def assignment(self) -> Assign:
        """
        Example
        bar = "bar"

        Rule:
        assignment     → IDENTIFIER "=" assignment
               | logic_or ;
        """
        expr = self.logic_or()
        if self.match(TokenTypes.EQUAL):
            value = self.assignment()
            if isinstance(expr, Variable):
                return Assign(name=expr.identifier, expr=value)
            # TODO: add expr/value/etc into error msg to make more informative?
            # And is parsing error the right type?
            raise ParsingError("Invalid assignment target.")

        return expr

    def logic_or(self) -> Logical:
        """
        logic_or       → logic_and ( "or" logic_and )* ;
        """
        left = self.logic_and()
        while self.match(ReservedTokenTypes.OR):
            left = Logical(left, self.previous_token(), self.logic_and())
        return left

    def logic_and(self) -> Logical:
        """
        logic_and      → equality ( "and" equality )* ;
        """
        left = self.equality()
        while self.match(ReservedTokenTypes.AND):
            left = Logical(left, self.previous_token(), self.equality())
        return left

    def statement(self) -> Statement:
        """
        statement      → exprStmt
               | forStmt
               | ifStmt
               | printStmt
               | whileStmt
               | block ;

        Kind of analogous to `expression` method. However, this handles more of the delegation
        to different statement methods whereas expression fully offloads that to the methods it
        calls.
        """
        # TODO: print statement does not match order in docstring, maybe need to move down?
        if self.match(ReservedTokenTypes.PRINT):
            return self.print_statement()
        if self.match(TokenTypes.LEFT_BRACE):
            return Block(self.block())
        if self.match(ReservedTokenTypes.WHILE):
            return self.while_statement()
        if self.match(ReservedTokenTypes.IF):
            return self.if_statement()
        if self.match(ReservedTokenTypes.FOR):
            return self.for_statement()
        return self.expression_statement()
    
    def while_statement(self):
        """
        whileStmt      → "while" "(" expression ")" statement ;
        """
        if not self.match(TokenTypes.LEFT_PAREN):
            raise SyntaxError("Expect '(' after 'while'.")
        expr = self.expression()
        if not self.match(TokenTypes.RIGHT_PAREN):
            raise SyntaxError("Expect '(' after 'while'.")
        stmt = self.statement()
        return While(expr, stmt)

    def if_statement(self):
        if not self.match(TokenTypes.LEFT_PAREN):
            raise SyntaxError("Expect '(' after 'if'.")
        condition = self.expression()
        if not self.match(TokenTypes.RIGHT_PAREN):
            raise SyntaxError("Expect ')' after if condition.")
        val = self.statement()
        other = None
        if self.match(ReservedTokenTypes.ELSE):
            other = self.statement()
        return IfStatement(condition, val, other)

    def for_statement(self):
        """
        forStmt        → "for" "(" ( varDecl | exprStmt | ";" )
                 expression? ";"
                 expression? ")" statement ;
        """
        if not self.match(TokenTypes.LEFT_PAREN):
            raise SyntaxError("Expect '(' after 'for'.")

        # Parse the initializer
        if self.match(TokenTypes.SEMICOLON):
            initializer = None
        elif self.match(ReservedTokenTypes.VAR):
            initializer = self.variable_declaration()
        else:
            initializer = self.expression_statement()

        # Parse the condition
        if self.current_token().token_type != TokenTypes.SEMICOLON:
            condition = self.expression()
        else:
            condition = None
        if not self.match(TokenTypes.SEMICOLON):
            raise SyntaxError(f"Expect ';' after loop condition.")

        # Parse the incrementer
        if self.current_token().token_type != TokenTypes.RIGHT_PAREN:
            incrementer = self.expression()
        else:
            incrementer = None
        if not self.match(TokenTypes.RIGHT_PAREN):
            raise SyntaxError(f"Expect ')' after for clauses.")

        body = self.statement()
        # TODO: consider desugaring like in book -> while loop. Should be optional though.
        return For(initializer, condition, incrementer, body)

    def block(self) -> list[Statement]:
        statements = []
        while self.curr_idx < self.max_idx and \
                self.current_token().token_type != TokenTypes.RIGHT_BRACE:
            # TODO: seems like we're hitting a parsing error in here.
            statements.append(self.declaration())
        if not self.match(TokenTypes.RIGHT_BRACE):
            # TODO: when parsing back to back blocks, by the end of the first one idx seems to be
            # 1 too high and we hit this error (we've already moved past right_brace). Interestingly
            # this didn't happen when including just one block. Maybe some nuance around declaration()
            # incrementing 1 too many times in certain instances? Need to investigate more.
            raise ParsingError("Expect '}' after block.")
        return statements

    def expression_statement(self) -> ExpressionStatement:
        """
        Example:
        foo();
        """
        expr = self.expression()
        # At this point we know the statement needs a semicolon next to finish it.
        # TODO: error messages are looking better now except tests expect this to be raised in same
        # line as "Error at )" type errors. Need to figure out why/how (could be that the
        # original method that is raising the first part should be raising both?)
        # UPDATE: actually I think maybe we shouldn't be hitting this at all and the test case
        # output is slightly wrong re the post-colon part?
        # Can confirm that by running larger test suite, but first need to
        # understand why this is getting raised and why it shouldn't be.
        # If we use valid FOR syntax, we never hit this method, I guess we're processing in the
        # FOR method. But when the initializer is {}, I guess we can't parse the for statement and
        # instead end up parsing separate expressions? And so we call this method.
        if not self.match(TokenTypes.SEMICOLON):
            raise SyntaxError("Expect ';' after expression.")

        return ExpressionStatement(expr)

    def print_statement(self) -> PrintStatement:
        """
        Example:
        print foo;
        """
        expr = self.expression()
        # At this point we know the statement needs a semicolon next to finish it.
        if not self.match(TokenTypes.SEMICOLON):
            raise SyntaxError("Expect ';' after expression.")
        return PrintStatement(expr)
        
    def synchronize(self):
        """Error handling for when we hit a parsing error."""
        # TODO: raising an error for now bc tests do want this for parsing errors, e.g. for `print;`
        # but eventually may need to recover and keep going. Remember parse() has try/except error
        # handling, as does declaration(), but expression() does not - so need to figure out a
        # consistent solution.
        token = self.current_token()
        # TODO rm incr? Added this to avoid inf loop in else clause in declaration() but kind of
        # hazy now on why this is necessary. (Update: I assume bc this is called when we hit an
        # error and so if we don't incr, we will just keep hitting that same error forever?)
        self.curr_idx += 1
        # Can't use set because these aren't hashable.
        start_types = [
            ReservedTokenTypes.CLASS,
            ReservedTokenTypes.FUN,
            ReservedTokenTypes.VAR,
            ReservedTokenTypes.FOR,
            ReservedTokenTypes.IF,
            ReservedTokenTypes.WHILE,
            ReservedTokenTypes.PRINT,
            ReservedTokenTypes.RETURN,
        ]
        # We actually want to exit the loop with curr_idx GREATER than max_idx if we don't hit any
        # of the start_types. This will ensure the `parse` method doesn't just try to parse the
        # final token again.
        # TODO: sounds like the issue is once we encounter an error in the for loop, we want to
        # proceed to the next *statement* entirely, so maybe increment 1 statement at a time vs
        # 1 token? Book seems to do the latter though... It's possible we need to implement that
        # elsewhere rather than modifying this while logic though?
        # gpt really doesn't like this idea though, it doesn't really like modifying the forstatement
        # method either with additional error handling logic (and the book doesn't do this) but it
        # seems to like it more than modifying synchronize. I don't love it, seems like lots of
        # duplicate logic, but we could try it next.
        # So basically: modify for_statement method to raise error early if we fail to parse any
        # of the 3 components.
        # UPDATE: tried this but we're still left with the question of "how do we know when the 
        # (broken syntax) for loop def is done?" My idea was to try to parse expr and then rewind
        # idx when we found one that worked, but gpt/claude both recommended against this -
        # apparently kind of violates the recursive descent parser ethos and makes it very slow.
        # Fiddled around a bit in forStatement but that really doesn't seem like the way. One option
        # is to basically implement "balanced parentheses" leetcode q inside synchronize, but that
        # hardly seems ideal either.
        # Unrelated: also need to decide what to do about codecrafters now that trial ended, could
        # try to see how to use that public repo to run tests?
        while self.curr_idx <= self.max_idx:
            curr = self.current_token()
            if self.previous_token().token_type == TokenTypes.SEMICOLON:
                break
            if curr.token_type in start_types:
                break
            self.curr_idx += 1
        raise ParsingError(f"[line {token.line}] Error at {token.lexeme!r}: Expect expression.")

    def declaration(self):
        """Kind of analogous to `expression` and `statement` methods.
        """
        try:
            if self.match(ReservedTokenTypes.VAR):
                return self.variable_declaration()
            else:
                return self.statement()
        except ParsingError as e:
            self.synchronize()
        
    # TODO: looks like book creates a new `assignment` rule in our grammar (presumably need a new
    # method) and an Assign class (like Binary). Consider refactoring to match (or consider if it's
    # really necessary?)
    def variable_declaration(self) -> VariableDeclaration:
        """Called *after* we've already confirmed there was a preceding VAR token and current_token
        now points to the token after that.
        """
        name = self.current_token()
        # TODO: can probably refactor so all the else clauses/error handling is cleaner.
        if self.match(TokenTypes.IDENTIFIER):
            if self.match(TokenTypes.EQUAL):
                expr = self.expression()
                declaration = VariableDeclaration(name.lexeme, expr)
                if self.match(TokenTypes.SEMICOLON):
                    # TODO: this is where we return the declaration processing "var a = b = 1"
                    # which isn't working. I think what's happening is b=1 evaluates to None
                    # so a does as well.
                    return declaration
                else:
                    # TODO: current test case is hitting this error when defining "var a = b = 1"
                    raise SyntaxError("Expect ';' after variable declaration.")
            elif self.match(TokenTypes.SEMICOLON):
                # Assign default value of nil.
                return VariableDeclaration(
                    name.lexeme,
                    Literal(Token("nil", name.line, token_type=ReservedTokenTypes.NIL))
                )
            else:
                raise NotImplementedError("TODO: handle case where user does 'var x;' with no assigned value.")
        # TODO: not sure if should be parsing/syntax/runtime error.
        raise ParsingError(f"Invalid variable declaration at line {name.line}")