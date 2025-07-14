import time
from typing import Any, Optional, Union

from app.environment import Environment
from app.exceptions import ParsingError
from app.interpreter import (
    INTERPRETER, Expression, Literal, Variable, Unary, Binary, Assign, Logical, Call, Grouping
)
from app.lexer import Token, TokenTypes, ReservedTokenTypes, TokenType
from app.utils import truthy, is_number, SENTINEL


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
        # Book doesn't really address what to do with spaces here so just skipping them for
        # now, otherwise our grammar breaks down.
        self.tokens = [token for token in tokens if token.token_type != TokenTypes.SPACE]
        self.max_idx = len(self.tokens) - 1
        self.curr_idx = 0
        self.mode = None

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
        self.mode = mode
        method_name = {
            "parse": "declaration",
            # TODO testing: previously was expression, tried switching to declaration 
            # but I think that broke our ability to handle expressions without trailing semicolons.
            # "evaluate": "declaration",
            "evaluate": "expression",
            "run": "declaration",
        }[mode]
        method = getattr(self, method_name)
        res = {
            "parsed": [],
            "success": True,
            "errors": [],
        }
        while self.curr_idx <= self.max_idx:
            prev_idx = self.curr_idx 

            try:
                res["parsed"].append(method())
            # TODO: may need to handle these differently, syntaxerrors are raised when statement
            # parsing fails while parsingerrors are raised when expression parsing fails.
            except (ParsingError, SyntaxError) as e:
                res["success"] = False
                res["errors"].append(e)
                # Avoid getting stuck in infinite loop on parsing errors that don't hit
                # synchronize().
                if self.curr_idx == prev_idx:
                    self.curr_idx += 1

                # TODO: Previously had this working with break enabled, but was getting wrong number
                # of errors in latest stage.
                # Need to figure out some logic to skip ahead to the next valid expr/decl etc,
                # right now we keep trying to parse the next token and this results in ghost errors.
                # break

        self.mode = None
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

        raise ParsingError(f"[line {token.line}] Error at {token.lexeme}.")

    def call(self) -> Call:
        """
        Example:
        foo()

        Rule:
        call → primary ( "(" arguments? ")" )* ;
        """
        expr = self.primary()
        while self.match(TokenTypes.LEFT_PAREN):
            args = self._get_call_args()
            expr = Call(expr, self.previous_token(), args)
        return expr

    def _get_call_args(self) -> list[Expression]:
        """Parse args from a callable expression.

        Example:
        foo(bar, baz) --> list containing two expressions (1 for bar, 1 for baz)
        """
        args = []
        if self.match(TokenTypes.RIGHT_PAREN):
            return args

        while True:
            args.append(self.expression())
            if not self.match(TokenTypes.COMMA):
                break

        if not self.match(TokenTypes.RIGHT_PAREN):
            # TODO: should this be syntax or parsing error? Guessing syntax?
            raise SyntaxError("Expect ')' after arguments.")
        return args


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

        return self.call()

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
            # TODO: add expr/value/etc into error msg to make more informative? Might break tests
            # since they check for specific wording?
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
            statements.append(self.declaration())
        if not self.match(TokenTypes.RIGHT_BRACE):
            raise ParsingError("Expect '}' after block.")
        return statements

    def expression_statement(self) -> Union[ExpressionStatement, Expression]:
        """
        Example:
        foo();

        Returns
        -------
        ExpressionStatement if mode is "run" or "evaluate"
        Expression if mode is "parse"
        """
        expr = self.expression()
        # In these modes, we don't want to enforce the trailing semicolon.
        if self.mode in ("parse", "evaluate"):
            return expr

        # At this point we know the statement needs a semicolon next to finish it.
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
        # Added this to avoid inf loop in else clause in declaration(), I think because
        # otherwise there's no way to increment curr_idx and we'd just keep hitting the same error
        # again and again.
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
        
    def variable_declaration(self) -> VariableDeclaration:
        """Called *after* we've already confirmed there was a preceding VAR token and current_token
        now points to the token after that.
        """
        name = self.current_token()
        if self.match(TokenTypes.IDENTIFIER):
            if self.match(TokenTypes.EQUAL):
                expr = self.expression()
                declaration = VariableDeclaration(name.lexeme, expr)
                if self.match(TokenTypes.SEMICOLON):
                    return declaration
                else:
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