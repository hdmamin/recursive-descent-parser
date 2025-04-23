from typing import Any, Union

from app.lexer import Token, TokenTypes, ReservedTokenTypes, TokenType


class Expression:
    
    def __str__(self) -> str:
        return f"{type(self)}()"


# TODO: seems like evaluate() needs to use token lexemes, not literals. Will need to strip
# quotes from strings.
# TODO: include docstring examples of each expr type, keep forgetting what each is.
class Literal(Expression):
    """
    Example
    foo
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


class Statement:
    """Statements 'do things' that have side effects, but do not return a value.
    This is in contrast to expressions, which compute and return a value.


    Grammar:

    program        → statement* EOF ;

    statement      → exprStmt
                     | printStmt ;

    exprStmt       → expression ";" ;
    printStmt      → "print" expression ";" ;
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
        print(self.expr.evaluate())


class ParsingError(Exception):
    """Raise when the parser hits an invalid token given the current state."""


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
# from app.debugging import decorate_methods, verbose
# @decorate_methods(verbose)
class Parser:
    """
    Each precedence level in our order of operations requires its own method.
    In the grammar below, "primary" has the "highest" precedence, meaning our parser calls it first
    and resolves it last.

    Grammar
    -------
    expression     → equality ;
    equality       → comparison ( ( "!=" | "==" ) comparison )* ;
    comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    term           → factor ( ( "-" | "+" ) factor )* ;
    factor         → unary ( ( "/" | "*" ) unary )* ;
    unary          → ( "!" | "-" ) unary
                | primary ;
    primary        → NUMBER | STRING | "true" | "false" | "nil"
                | "(" expression ")" ;
    """

    def __init__(self, tokens: list[Token]):
        # TODO: book doesn't really address what to do with spaces here so just skipping them for
        # now, otherwise our grammar breaks down.
        self.tokens = [token for token in tokens if token.token_type != TokenTypes.SPACE]
        self.max_idx = len(self.tokens) - 1
        self.curr_idx = 0

        self.expr_idx = 0
        # These will be populated by parse().
        self.expressions = []
        self.max_expr_idx = None

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

    def parse(self) -> dict:
        """Parse all tokens in the source code into expressions.

        Returns
        -------
        dict
            expressions: list[str]
            success: bool
            error: Optional[Exception]

            Note that we do NOT try to evalute the expressions yet. We may still encounter
            additional errors when we do.
        """
        res = {
            "expressions": [],
            "success": True,
            "error": None,
        }
        while self.curr_idx <= self.max_idx:
            try:
                res["expressions"].append(self.expression())
            except ParsingError as e:
                res["success"] = False
                res["error"] = e
                # TODO: may eventually want to keep parsing but for now we return early.
                break

        self.expressions = res["expressions"].copy()
        self.max_expr_idx = len(self.expressions)
        return res

    def expression(self) -> Expression:
        """
        Rule:
        expression → equality ;
        """
        # equality is the highest precedence (last to be evaluated) operation.
        return self.equality()

    def primary(self) -> Union[Literal, Grouping]:
        """
        Example:
        "foo"

        Rule:
        primary → NUMBER | STRING | "true" | "false" | "nil"
               | "(" expression ")" ;
        """
        token = self.current_token()
        # Reserved types
        reserved_types = (ReservedTokenTypes.FALSE, ReservedTokenTypes.TRUE, ReservedTokenTypes.NIL)
        other_types = (TokenTypes.NUMBER, TokenTypes.STRING)
        # TODO: book doesn't include IDENTIFIER here yet but codecraftesr tests seem to. Haven't
        # updated docstrings yet to reflect this, just testing.
        if self.match(*reserved_types, *other_types, TokenTypes.IDENTIFIER):
            return Literal(token)
        
        if self.match(TokenTypes.LEFT_PAREN):
            expr = self.expression()
            if not self.match(TokenTypes.RIGHT_PAREN):
                raise TypeError(
                    f"Expected type {TokenTypes.RIGHT_PAREN}, found "
                    f"{self.current_token().token_type}."
                )
            return Grouping(expr)

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

    def statement(self):
        # TODO: kinda analogous to parse? Or like an amalgamation of all our expr methods
        # (equality/binary/unary/etc)
        token = self.current_token()
        # TODO: this is wrong, I'm trying to compare an expr to a token type.
        if self.match(token, ReservedTokenTypes.PRINT):
            return self.print_statement()
        return self.expression_statement()

    def current_expression(self) -> Expression:
        if self.expr_idx <= self.max_expr_idx:
            return self.expressions[self.expr_idx]
        # TODO: not sure if this is the right error type. Also should probably improve error message
        # (currently geared more for debugging than for end user experience).
        raise SyntaxError(
            f"Syntax error. Invalid index {self.expr_idx}, max_idx is {self.max_expr_idx}."
        )

    def expression_statement(self):
        pass

    def print_statement(self):
        expr = self.current_expression()
        if self.current_expression() != TokenTypes.SEMICOLON:
            raise SyntaxError("TODO")
        # TODO: need to check if next statement is a semicolon.
        # Also unclear on why the match logic isn't implemented here - looks like book implements it
        # in the more generic statement() method, analogous to parse().
        return PrintStatement(expr)
        