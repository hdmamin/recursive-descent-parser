from typing import Any, Optional, Union

from app.environment import Environment
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

    # TODO: not sure if this will work.
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
        # return "(" + str(self.left) + self.val.lexeme + str(self.right) + ")"
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

    # TODO: name arg currently expects a token, but we only have access to an expr when I create
    # Assign and that's what I'm passing in. Looks like current Assign functionality WOULD be fine
    # if we passed in a string, but everything else takes in tokens and expressions so a little
    # worried that breaking that pattern might be a problem at some point. Need to see if we can get
    # a token out of an expression, or maybe if we can pass in the full expression and extract the
    # name in Assign.
    def __init__(self, name: Token, expr: Expression, env: Optional[type] = None):
        self.name = name
        self.expr = expr
        self.env = env or Environment
        # This gets updated when evaluate() is called.
        self.val = SENTINEL

        # TODO: might not need this anymore I think?
        self.i = self.env.set(self)

    def __str__(self) -> str:
        return f"({self.name.non_null_literal} = {self.expr})"

    def evaluate(self):
        """Evaluates the value of the variable and returns the corresponding python object."""
        if self.val == SENTINEL:
            if not self.env.contains(self.name.non_null_literal):
                # TODO: is this the right error type? Also maybe raising it too early, this is at
                # parsing time IIRC?
                raise RuntimeError(
                    f"Cannot assign a value to var {self.name!r} because it does not exist."
                )

            self.val = self.expr.evaluate()
            self.env.update_state(self.name.non_null_literal, self.val)
        return self.val


class Statement:
    """Statements 'do things' that have side effects, but do not return a value.
    This is in contrast to expressions, which compute and return a value.


    Grammar:

    program        → declaration* EOF ;

    declaration    → varDecl
                | statement ;

    statement      → exprStmt
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
    
    def evaluate(self) -> None:
        self.expr.evaluate()


class PrintStatement(Statement):
    
    def __init__(self, expr: Expression):
        self.expr = expr

    def __str__(self) -> str:
        return f"(print {self.expr})"

    def evaluate(self) -> None:
        print(to_lox_dtype(self.expr.evaluate()))

    
class VariableDeclaration(Statement):
    """Creates a variable (global by default).
    """
    
    def __init__(self, name: str, expr: Expression, env: Optional[type] = None) -> None:
        self.name = name
        self.expr = expr
        # We will set this in evaluate. Don't use default=None because evaluate could return None.
        self.value = SENTINEL

        # Currently we register it as a global variable by default.
        self.env = env or Environment
        # The i'th declaration of this particular var name in the current environment.
        self.i = self.env.set(self)

    # TODO: getting displayed like "((a = foo);)", guessing that may not be correct.
    def __str__(self) -> str:
        return f"({self.name} = {self.expr})"

    def evaluate(self) -> None:
        # Only want to evaluate once, not every time we reference a variable.
        if self.value == SENTINEL:
            self.value = self.expr.evaluate()
            self.env.update_state(self.name, self.value)


class Block(Statement):
    """Section of code enclosed in curly braces that defines a new temporary scope.
    """
    
    def __init__(self, statements: list[Statement]) -> None:
        self.statements = statements

    # TODO not sure what desired format actually is
    def __str__(self) -> str:
        return f"({self.statements})"

    # TODO
    def evaluate(self) -> None:
        for statement in self.statements:
            statement.evaluate()


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
                | equality ;
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
            "run": "declaration",
            # TODO testing: previously was expression, tried switching to declaration 
            # but I think that broke our ability to handle expressions without trailing semicolons.
            # "evaluate": "declaration",
            "evaluate": "expression",
            "parse": "expression"
        }[mode]
        method = getattr(self, method_name)
        res = {
            f"{method_name}s": [],
            "success": True,
            "error": None,
        }
        while self.curr_idx <= self.max_idx:
            try:
                res[f"{method_name}s"].append(method())
            # TODO: may need to handle these differently, syntaxerrors are raised when statement
            # parsing fails while parsingerrors are raised when expression parsing fails.
            except (ParsingError, SyntaxError) as e:
                res["success"] = False
                res["error"] = e
                # TODO: may eventually want to keep parsing but for now we return early.
                break

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
                raise TypeError(
                    f"Expected type {TokenTypes.RIGHT_PAREN}, found "
                    f"{self.current_token().token_type}."
                )
            return Grouping(expr)

        if self.match(TokenTypes.IDENTIFIER):
            return Variable(token)

        # TODO: evaluate mode is hitting this condition when trying to define a var.
        raise ParsingError(f"Failed to parse token {token.lexeme} at line {token.line}.")

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
               | equality ;
        """
        expr = self.equality()
        if self.match(TokenTypes.EQUAL):
            value = self.assignment()
            if isinstance(expr, Variable):
                return Assign(name=expr.identifier, expr=value)
            # TODO: add expr/value/etc into error msg to make more informative?
            # TODO: is parsing error the right type?
            raise ParsingError("Invalid assignment target.")

        return expr

    def statement(self) -> Statement:
        # Kind of analogous to `expression` method. However, this handles more of the delegation
        # to different statement methods whereas expression fully offloads that to the methods it
        # calls.
        if self.match(ReservedTokenTypes.PRINT):
            return self.print_statement()
        if self.match(TokenTypes.LEFT_BRACE):
            return Block(self.block())
        return self.expression_statement()

    def block(self) -> list[Statement]:
        statements = []
        while self.curr_idx < self.max_idx and \
                self.current_token().token_type != TokenTypes.RIGHT_BRACE:
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
        if not self.match(TokenTypes.SEMICOLON):
            # TODO: both evaluate and run mode hit this error whe parsing exprs like '3+4'
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
        """TODO: error handling for when we hit a parsing error."""
        # TODO: raising an error for now bc tests do want this for parsing errors, e.g. for `print;`
        # but eventually may need to recover and keep going. Remember parse() has try/except error
        # handling, as does declaration(), but expression() does not - so need to figure out a
        # consistent solution.
        token = self.current_token()
        # TODO rm incr? Added this to avoid inf loop in else clause in declaration() but kind of
        # hazy now on why this is necessary.
        self.curr_idx += 1
        raise ParsingError(f"Failed to parse token {token.lexeme} at line {token.line}.")

    def declaration(self):
        """Kind of analogous to `expression` and `statement` methods.
        """
        try:
            if self.match(ReservedTokenTypes.VAR):
                return self.variable_declaration()
            else:
                return self.statement()
        except ParsingError:
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