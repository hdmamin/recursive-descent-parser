from typing import Union

from lox.exceptions import ParsingError
from lox.interpreter import (
    Expression, Literal, Variable, Unary, Binary, Assign, Logical, Call, Grouping,
    Statement, IfStatement, PrintStatement, ExpressionStatement, ReturnStatement,
    VariableDeclaration, Block, While, For, Function, Class, Get, Set, This, Super
)
from lox.lexer import Token, TokenTypes, ReservedTokenTypes, TokenType



# TODO: rm decorator once done debugging. For now leave it so can easily comment it on/off.
from lox.debugging import decorate_methods, verbose
# @decorate_methods(verbose)
class Parser:
    """
    Each precedence level in our order of operations requires its own method.
    In the grammar below, "primary" has the "highest" precedence, meaning our parser calls it first
    and resolves it last.

    Grammar
    -------
    expression     → assignment ;
    assignment     → ( call "." )? IDENTIFIER "=" assignment
               | logic_or ;
    logic_or       → logic_and ( "or" logic_and )* ;
    logic_and      → equality ( "and" equality )* ;
    equality       → comparison ( ( "!=" | "==" ) comparison )* ;
    comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    term           → factor ( ( "-" | "+" ) factor )* ;
    factor         → unary ( ( "/" | "*" ) unary )* ;
    unary          → ( "!" | "-" ) unary | call ;
    call           → primary ( "(" arguments? ")" | "." IDENTIFIER )* ;
    arguments      → expression ( "," expression )* ;
    primary        → "true" | "false" | "nil" | NUMBER | STRING
               | "(" expression ")"
               | IDENTIFIER ;
    """

    def __init__(self, tokens: list[Token]):
        # Book doesn't really address what to do with spaces here so just skipping them for
        # now, otherwise our grammar breaks down.
        self.tokens = [token for token in tokens
                       if token.token_type not in (TokenTypes.SPACE, TokenTypes.TAB)]
        self.max_idx = len(self.tokens) - 1
        self.curr_idx = 0
        self.mode = None
        self.synchronize_depth = 0

    def reset_index(self):
        """After parsing, we need to reset the index back to 0 before we run."""
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
            # print('\t parse loop:', self.curr_idx, self.current_token(), res) # TODO rm
            prev_idx = self.curr_idx 

            try:
                res["parsed"].append(method())
            # TODO: may need to handle these differently, syntaxerrors are raised when statement
            # parsing fails while parsingerrors are raised when expression parsing fails.
            except (ParsingError, SyntaxError) as e:
                # print('parse error:', self.curr_idx, e) # TODO rm
                self.synchronize_depth = 0
                res["success"] = False
                res["errors"].append(e)
            
            # TODO prev this was in except block but I guess technically it's always valid/useful.
            # Avoid getting stuck in infinite loop on parsing errors that don't hit
            # synchronize().
            if self.curr_idx == prev_idx:
                self.curr_idx += 1

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
        primary        → "true" | "false" | "nil" | "this"
               | NUMBER | STRING | IDENTIFIER | "(" expression ")"
               | "super" "." IDENTIFIER ;
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

        if self.match(ReservedTokenTypes.THIS):
            return This(token)

        if self.match(TokenTypes.IDENTIFIER):
            return Variable(token)

        if self.match(ReservedTokenTypes.SUPER):
            if not self.match(TokenTypes.DOT):
                # print("SUPER ERORR") # TODO rm
                raise SyntaxError("Expect '.' after 'super'.")
            if not self.match(TokenTypes.IDENTIFIER):
                raise SyntaxError("Expect superclass method name.")

            return Super(token, self.previous_token())

        # TODO: in parse mode, func call is hitting this error at the semicolon after the func call.
        # Think the primary call is triggered by self.call() and should be matching IDENTIFIER.
        # Looks like we are matching identifier, see below with idx-3, but for some reason we must
        # not be parsing the full expr? Or maybe we finished parsing that nd are trying to parse
        # seimcolon as its own expr? UPDATE: looks like it's the latter, we do parse a call() for
        # f() but then call primary again for some reason on just the semicolon.
        raise ParsingError(f"[line {token.line}] Error at {token.lexeme}.")

    def call(self) -> Call:
        """
        Example:
        foo()

        Rule:
        call → primary ( "(" arguments? ")" | "." IDENTIFIER )* ;
        """
        expr = self.primary()
        while True:
            if self.match(TokenTypes.LEFT_PAREN):
                args = self._get_call_args()
                expr = Call(expr, self.previous_token(), args)
            elif self.match(TokenTypes.DOT):
                expr = Get(expr, self.current_token())
                # Increment to avoid processing the attr name token again separately.
                self.curr_idx += 1
            else:
                break
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
        assignment     → ( call "." )? IDENTIFIER "=" assignment
               | logic_or ;
        """
        expr = self.logic_or()
        if self.match(TokenTypes.EQUAL):
            right = self.assignment()
            if isinstance(expr, Variable):
                return Assign(name=expr.identifier, expr=right)
            if isinstance(expr, Get):
                return Set(expr.obj, expr.attr, right)
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
        # TODO: presumably somewhere need to check if this is a valid place for return, i.e. are we
        # inside a class/function. May rm this if, could be unnecessary, we may actually want
        # expr_stmt.
        line_num = self.current_token().line
        if self.match(ReservedTokenTypes.RETURN):
            # TODO: we're hitting an error in the expression() call. Looks like the parsing logic
            # here is a little more complex, need to flesh it out. May also need to deal with case
            # specifically where there is no return statement, currently that would not get caught
            # by match(RETURN).
            expr = None if self.match(TokenTypes.SEMICOLON) else self.expression_statement()
            if isinstance(expr, ExpressionStatement):
                expr = expr.expr
            return ReturnStatement(line_num=line_num, expr=expr)
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
        error = None
        while self.curr_idx < self.max_idx and \
                self.current_token().token_type != TokenTypes.RIGHT_BRACE:
            try:
                declaration = self.declaration(custom_error=True)
            except (ParsingError, SyntaxError) as e:
                # TODO: if multiple errors inside block, this may incorrectly overwrite error with
                # the latest one whereas we want the first?
                error = e
                # break # TODO rm
            else:
                statements.append(declaration)

        # TODO: actually prob need to raise this error after the one we already caught if applicable?
        # Not sure how that should look.
        # Finish block parsing regardless of whether we encountered an error, otherwise we'll try
        # to parse the remainder as a new declaration in the next iteration of the `parse` while
        # loop.
        if not self.match(TokenTypes.RIGHT_BRACE):
            raise ParsingError("Expect '}' after block.")
        if error:
            # print('Block raising error:', error) # TODO rm
            raise error
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

        # Only 'run' mode enforces the semicolon but we should still check when in the other modes 
        # to increment the current index if there is a semicolon, so we don't try to parse it again.
        if not self.match(TokenTypes.SEMICOLON) and self.mode == "run":
            raise SyntaxError("Expect ';' after expression.")

        if self.mode in ("parse", "evaluate"):
            return expr

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
        
    def synchronize(self, error_suffix: str = "Expect expression.", start: int = None): # TODO rm start
        """Error handling for when we hit a parsing error."""
        # TODO
        if self.synchronize_depth > 0:
            # print("skip synchronize", start, self.synchronize_depth) # TODO
            return
        # print("ENTER SYNCHRONIZE", start, self.synchronize_depth) # TODO
        self.synchronize_depth += 1

        token = self.current_token()
        # Added this to avoid inf loop in else clause in declaration(), I think because
        # otherwise there's no way to increment curr_idx and we'd just keep hitting the same error
        # again and again.
        self.curr_idx += 1
        # print('incremented index to', self.curr_idx) # TODO rm
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
        # print('\t synchronize 2 (about to raise parsing err):', repr(error_suffix), token, token.line) # TODO rm
        # print("EXIT SYNCHRONIZE", start, self.synchronize_depth, 'curr idx:', self.curr_idx) # TODO
        raise ParsingError(f"[line {token.line}] Error at {token.lexeme!r}: {error_suffix}")

    def declaration(self, custom_error: bool = False):
        """Kind of analogous to `expression` and `statement` methods.
        """
        # TODO: trying to match book's expected error messages for function_declaration without
        # breaking prev stages. Maybe can go back after confirming this works and always use custom
        # here.
        import time; start = hash(time.time());
        # print("ENTER DECLARATION", start) # TODO
        # tmp = None
        try:
            if self.match(ReservedTokenTypes.FUN):
                custom_error = True
                return self.function_declaration(kind="function")
            elif self.match(ReservedTokenTypes.VAR):
                return self.variable_declaration()
            elif self.match(ReservedTokenTypes.CLASS):
                return self.class_declaration()
            else:
                return self.statement()
        except (ParsingError, SyntaxError) as e:
            kwargs = {"error_suffix": str(e)} if custom_error else {}
            # current_token = self.current_token()
            # error_suffix = str(e) if custom_error else "Expect expression."
            # print('declaration (after synchronize):', f"[line {current_token.line}] Error at {current_token.lexeme!r}: {error_suffix}") # TODO rm
            # tmp = e
            # print('pre sync', e, '\ttmp:', tmp)
            result = self.synchronize(**kwargs, start=start) # TODO rm start
            # print('post sync', e)
            # This means we return early due to being in panic mode and should re-raise the
            # original error.
            if result is None:
                # print(">>> RE RAISING ERROR", self.curr_idx, e)
                raise e
            # else:
            #     print(">>> RESULT ELSE:", result)
            # raise type(e)(f"[line {current_token.line}] Error at {current_token.lexeme!r}: {error_suffix}")
        # # TODO rm
        # else:
        #     print("DECLARATION SUCCESS", start, self.synchronize_depth)
        # finally:
        #     print("EXIT DECLARATION", start, self.synchronize_depth, tmp) # TODO
        
    def variable_declaration(self) -> VariableDeclaration:
        """Called *after* we've already confirmed there was a preceding VAR token and current_token
        now points to the token after that.
        """
        name = self.current_token()
        if self.match(TokenTypes.IDENTIFIER):
            if self.match(TokenTypes.EQUAL):
                expr = self.expression()
                declaration = VariableDeclaration(name, expr)
                if self.match(TokenTypes.SEMICOLON):
                    return declaration
                else:
                    raise SyntaxError("Expect ';' after variable declaration.")
            elif self.match(TokenTypes.SEMICOLON):
                # Assign default value of nil.
                return VariableDeclaration(
                    name,
                    Literal(Token("nil", name.line, token_type=ReservedTokenTypes.NIL))
                )
            else:
                raise NotImplementedError("TODO: handle case where user does 'var x;' with no assigned value.")
        # TODO: not sure if should be parsing/syntax/runtime error.
        raise ParsingError(f"Invalid variable declaration at line {name.line}")

    def function_declaration(self, kind: str) -> Function:
        """
        Parameters
        ----------
        kind : str
            "function" or "class"

        funDecl → "fun" function ;
        classDecl      → "class" IDENTIFIER ( "<" IDENTIFIER )?
                 "{" function* "}" ;

        Example
        fun foo(bar, baz) {
            print bar;
            print baz;
        }

        class Breakfast {
            cook() {
                print "cooking";
            }
        }
        """
        name = self.current_token()
        self.curr_idx += 1
        if not self.match(TokenTypes.LEFT_PAREN):
            # TODO syntax or parsing error?
            raise SyntaxError(f"Expect '(' after {kind} name.")
        
        params = []
        n_params = 0
        prev_token = None
        while True:
            param = self.current_token()
            if prev_token and prev_token.token_type == param.token_type:
                # We should alternate between identifier and comma. Curr_idx gets incremented
                # afterwards so we don't have to decrement here like in the other break case.
                break

            self.curr_idx += 1
            prev_token = param
            if param.token_type == TokenTypes.IDENTIFIER:
                params.append(param) 
                n_params += 1
            elif param.token_type == TokenTypes.COMMA:
                continue
            else:
                # Rewind one token so that the right_parens check that follows executes against the
                # correct token.
                self.curr_idx -= 1
                break
            if n_params > 255:
                # TODO or syntax error? or other?
                raise SyntaxError("Can't have more than 255 parameters.")

        if not self.match(TokenTypes.RIGHT_PAREN):
            # TODO syntax or parsing error?
            raise SyntaxError(f"Expect ')' after parameters.")
        if not self.match(TokenTypes.LEFT_BRACE):
            # TODO syntax or parsing error?
            raise SyntaxError(f"Expect '{{' before {kind} body.")
        body = Block(self.block())
        return Function(name, params, body)

    def class_declaration(self) -> Class:
        if not self.match(TokenTypes.IDENTIFIER):
            raise SyntaxError("Expect class name.")

        name = self.previous_token()
        parent = None
        if self.match(TokenTypes.LESS):
            parent = Variable(self.current_token())
            self.curr_idx += 1

        if not self.match(TokenTypes.LEFT_BRACE):
            raise SyntaxError("Expect '{' before class body.")

        methods = []
        error = None
        while self.curr_idx <= self.max_idx and not self.match(TokenTypes.RIGHT_BRACE):
            # print('while: idx', self.curr_idx)
            try:
                # print('try', self.curr_idx) # TODO rm
                method = self.function_declaration(kind="function")
            except (ParsingError, SyntaxError) as e:
                # print("class_decl while err:", e, self.curr_idx) # TODO rm
                error = e
            else:
                methods.append(method)
            # print('end while iter:', self.curr_idx) # TODO rm
        # The second condition (match) in `while` can send us past the last index.
        prev_token = self.previous_token()
        # self.curr_idx = min(self.curr_idx, self.max_idx)
        # print('>>> POST WHILE', self.curr_idx) # TODO rm
        
        # TODO: might be raising these 2 errors out of order now.
        # Check regardless of whether we hit an error otherwise `parse` will try to re-parse this
        # "}" on the next iteration.
        if prev_token.token_type != TokenTypes.RIGHT_BRACE:
            raise SyntaxError("Expect '}' after class body.")
        if error:
            # print('class decl end error', self.curr_idx, error, self.synchronize_depth) # TODO rm
            raise error
        return Class(name, methods, parent)