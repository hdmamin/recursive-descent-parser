from string import ascii_letters
from collections.abc import Iterable
from collections import deque
from dataclasses import dataclass
from functools import lru_cache
import logging
import sys
from typing import Union, Callable, Optional

from app.data_structures import Trie, ASTNode, AST


logger = logging.getLogger()
logging.basicConfig()


@dataclass
class TokenType:

    name: str
    # If a callable, this should be a function that takes one arg (the source token string) and
    # returns a string (the corresponding lexeme to display).
    lexeme: Union[str, Callable]
    # Most token types have literal="null" (the default), but we can also pass in a function that
    # accepts a single arg (a str consisting of the raw characters the token is comprised of).
    # e.g. for strings this is the identity
    # function, for numbers we do some additional processing to make sure ints have a trailing ".0"
    literal: Union[str, Callable] = "null"
    # False means this token should not show up in lexed output.
    lexable: bool = True
    # By default this will be set to the first char of the lexeme (str). But for cases
    # where that's a function, the user must provide it explicitly, either as a string if that's
    # known upfront (e.g. STRING starts with ") or tuple[str] if there are multiple valid
    # characters (e.g. NUMBER).
    start: Optional[Union[tuple, str]] = None
    # Function that accepts a str and returns the longest valid substring
    # starting from the first character that can produce a token of the current type.
    # TODO: in practice I think returning None should not be necessary because by the time we
    # select a tokentype cls we should know the first char at least is correct. But need to confirm.
    # If no valid substring is found, the returned value must be None (NOT an empty str).
    # It can also raise an UnterminatedLexeme error if appropriate (e.g. source code '"abc' opens
    # but does not close a str).
    # If a TokenType consists purely of a predefined str lexeme, e.g.
    # LEFT_PAREN or EQUAL_EQUAL, this does NOT need to be defined explicitly. It's only needed for
    # lexemes like STRING where we don't know upfront what characters the lexeme will consist of.
    longest_leading_substring: Optional[Callable] = None

    def __post_init__(self):
        if isinstance(self.lexeme, str):
            if self.start is None:
                self.start = self.lexeme[0]
            else:
                # TODO: prob remove this after initial dev is done. Allowing it because reserved
                # words do set a non-none value.
                logger.warning(
                    f"`start` should often not be specified when `lexeme` is a str. "
                    f"Got start={self.start}."
                )
        else:
            if self.start is None:
                raise ValueError(f"`start` cannot be None when `lexeme` is not a str.")

        if not self.longest_leading_substring:
            if not isinstance(self.lexeme, str):
                raise ValueError(
                    "`longest_leading_substring` must be provided when `lexeme` is not a string."
                )
            self.longest_leading_substring = self._default_longest_leading_substring

    def _default_longest_leading_substring(self, text: str) -> str:
        """The default way to get the longest leading substring for lexemes whose content is known
        upfront, e.g. '>=' or '!'.
        """
        if text.startswith(self.lexeme):
            return self.lexeme
        return None
            

class UnterminatedLexeme(Exception):
    """For when we encounter a lexeme that starts with a valid character but does not end with one.
    This lets us raise a single error for the whole sequence rather than a separate one for each
    character.
    """


def _string_longest_leading_substring(text: str) -> Optional[str]:
    """longest_leading_substring function for the STRING TokenType.
    """
    if not text[0] == '"':
        return None
    for i, char in enumerate(text[1:], 1):
        if char == '"':
            return text[1:i]
    raise UnterminatedLexeme("Unterminated string.")


def _number_longest_leading_substring(text: str) -> Optional[str]:
    """longest_leading_substring function for the NUMBER TokenType.

    123 -> 123
    123.4 -> 1234
    123.a -> 123
    123. -> 123
    a -> None
    . -> None
    """
    if not text[0].isdigit():
        return None

    # TODO: confirm if needs to end with space/newline or if valid to end with any non-digit
    # This could effect the if clause and/or the final return value (could possibly need to raise
    # an UnterminatedLexeme error there?)
    seen_dot = False
    max_idx = len(text) - 1
    for i, char in enumerate(text):
        is_dot = char == "."
        if (
            (is_dot and i == max_idx)  # lox numbers can't end with dot
            or (is_dot and not text[i+1].isdigit())  # non-digit after dot is invalid
            or not (char.isdigit() or is_dot)  # don't include non-digit non-dot characters
        ):
            return text[:i]
        seen_dot = seen_dot or is_dot
    return text


def _number_format_literal(text: str) -> str:
    """Format numbers (even ints) with at least one decimal place, per book conventions."""
    if "." in text:
        # Based on tests, it appears Lox won't let us end with multiple 0s after the decimal.
        text = text.rstrip("0")
        if text.endswith("."):
            text += "0"
        return text
    else:
        # Lox expects ints to be displayed with a trailing 0 decimal.
        return text + ".0"


def _identifier_longest_leading_substring(text: str) -> str:
    for i, char in enumerate(text):
        if not (char.isalnum() or char == "_"):
            return text[:i]
    return text


class TokenTypes:

    @classmethod
    @lru_cache()
    def lexemes2types(cls) -> dict:
        # Returns a mapping from each lexeme (e.g. "{") to its corresponding TokenType object.
        # TODO: this breaks down a bit for STRING type given current implementation where lexeme
        # is a lambda. Same with NUMBER type.
        return {
            v.lexeme: v for k, v in vars(cls).items()
            if k.isupper() and isinstance(v, TokenType)
        }

    @classmethod
    def lexeme2type(cls, lexeme: str) -> TokenType:
        """Maps a single lexeme, e.g. "}", to its coresponding TokenType."""
        return cls.lexemes2types()[lexeme]

    # Punctuators
    # (){};,+-*!===<=>=!=<>/.
    LEFT_PAREN = TokenType(name="LEFT_PAREN", lexeme="(")
    RIGHT_PAREN = TokenType(name="RIGHT_PAREN", lexeme=")")
    LEFT_BRACE = TokenType(name="LEFT_BRACE", lexeme="{")
    RIGHT_BRACE = TokenType(name="RIGHT_BRACE", lexeme="}")
    SEMICOLON = TokenType(name="SEMICOLON", lexeme=";")
    COMMA = TokenType(name="COMMA", lexeme=",")
    PLUS = TokenType(name="PLUS", lexeme="+")
    MINUS = TokenType(name="MINUS", lexeme="-")
    STAR = TokenType(name="STAR", lexeme="*")
    BANG_EQUAL = TokenType(name="BANG_EQUAL", lexeme="!=")
    EQUAL_EQUAL = TokenType(name="EQUAL_EQUAL", lexeme="==")
    LESS_EQUAL = TokenType(name="LESS_EQUAL", lexeme="<=")
    GREATER_EQUAL = TokenType(name="GREATER_EQUAL", lexeme=">=")
    LESS = TokenType(name="LESS", lexeme="<")
    GREATER = TokenType(name="GREATER", lexeme=">")
    SLASH = TokenType(name="SLASH", lexeme="/")
    DOT = TokenType(name="DOT", lexeme=".")

    # Assignment and equality operators
    EQUAL = TokenType(name="EQUAL", lexeme="=")
    EQUAL_EQUAL = TokenType(name="EQUAL_EQUAL", lexeme="==")

    # Negation and Inequality operators
    BANG = TokenType(name="BANG", lexeme="!")

    # Spaces
    SPACE = TokenType(name="SPACE", lexeme=" ", lexable=False)
    TAB = TokenType(name="TAB", lexeme="\t", lexable=False)
    NEWLINE = TokenType(name="TAB", lexeme="\n", lexable=False)

    # More complex types
    STRING = TokenType(
        name="STRING",
        lexeme=lambda x: '"' + x + '"',
        literal=lambda x: x,
        start='"',
        longest_leading_substring=_string_longest_leading_substring
    )
    NUMBER = TokenType(
        name="NUMBER",
        lexeme=lambda x: x,
        literal=_number_format_literal,
        # Confirmed it cannot start with a "."
        start=tuple(str(i) for i in range(10)),
        longest_leading_substring=_number_longest_leading_substring
    )
    # Varnames, function names, etc.
    IDENTIFIER = TokenType(
        name="IDENTIFIER",
        lexeme=lambda x: x,
        start=tuple(ascii_letters + "_"),
        longest_leading_substring=_identifier_longest_leading_substring
    )


class ReservedTokenTypes:

    # Reserved words
    # These are a bit different from other TokenTypes in that we don't place them in the trie
    # in order to avoid clashes with IDENTIFIER. We handle these separately in infer_token_type.
    # Note that we depart slightly from old `start` conventions and use the whole word as the
    # start char.
    # TODO: will have to see if this works with my trie scheme. Might need to adjust length
    # selection logic, forget if it counts number of edges/nodes or str length.
    # TODO: think default leading_substring func *should* work but check. If not maybe can write
    # one leading_substring func or partial and use for all of them?
    AND = TokenType(name="AND", lexeme="and")
    CLASS = TokenType(name="CLASS", lexeme="class")
    ELSE = TokenType(name="ELSE", lexeme="else")
    FALSE = TokenType(name="FALSE", lexeme="false") 
    FOR = TokenType(name="FOR", lexeme="for")
    FUN = TokenType(name="FUN", lexeme="fun")
    IF = TokenType(name="IF", lexeme="if")
    NIL = TokenType(name="NIL", lexeme="nil")
    OR = TokenType(name="OR", lexeme="or")
    PRINT = TokenType(name="PRINT", lexeme="print")
    RETURN = TokenType(name="RETURN", lexeme="return")
    SUPER = TokenType(name="SUPER", lexeme="super")
    THIS = TokenType(name="THIS", lexeme="this")
    TRUE = TokenType(name="TRUE", lexeme="true")
    VAR = TokenType(name="VAR", lexeme="var")
    WHILE = TokenType(name="WHILE", lexeme="while")

    @classmethod
    @lru_cache
    def lexemes2types(cls):
        # Duplicates logic from TokenTypes but I don't want to inherit from that because I don't
        # want to include non-keywords here. Maybe can change this to a single standalone function
        # later or find a more elegant solution.
        return {
            type_.lexeme: type_ for name, type_ in vars(cls).items()
            if name.isupper() and isinstance(type_, TokenType)
        }


# Hilariously over-engineered but 🤷‍♂️, just having fun.
# Each edge corresponds to a single character.
# If a TokenType has multiple valid start characters (like NUMBER), each of those gets its own
# outgoing edge. (TODO: guessing this logic will break down at some point though, we're already
# pushing it.)
# Sample usage: TYPES_TRIE.get("=")
# returns a dict with "node" (TrieNode) key where `value` is a TokenType instance or None.
# and "is_leaf" (bool). If is_leaf=True, `value` is not None and corresponds to a TokenType
# that fits the string you passed in.
def create_trie(token_types_cls: type) -> Trie:
    trie = Trie()
    for k, token_type in token_types_cls.lexemes2types().items():
        kwargs = {k: token_type}
        if not isinstance(k, str):
            if isinstance(token_type.start, str):
                kwargs = {token_type.start: token_type}
            elif isinstance(token_type.start, Iterable):
                kwargs = {val: token_type for val in token_type.start}
            else:
                raise TypeError(
                    f"Encountered unexpected start type {type(token_type.start)} for "
                    f"TokenType {token_type.name}."
                )
        trie.update(kwargs)
    return trie


TYPES_TRIE = create_trie(TokenTypes)
RESERVED_TYPES_TRIE = create_trie(ReservedTokenTypes)
    

def infer_token_type(text: str, trie: Optional[Trie] = None) -> Optional[type]:
    """Given a multi-character line of source code, find the appropriate next TokenType class.
    Preference is given to classes that match more characters, e.g. "== 3" would return
    TokenTypes.EQUAL_EQUAL rather than TokenTypes.EQUAL.

    Dev notes:
    - when we look at a new char, we use the trie to select candidate token types (this should find
    a node or None). We do this iteratively, stepping forward char by char and trying to extend
    each candidate token type.
    - eventually, each candidate should either reveal itself to be not a match OR hit a leaf node.
    We can then take the leaf node that uses the longest sequence.
    """
    trie = trie or TYPES_TRIE
    chars = deque(text)
    active_nodes = [trie.root]
    # length (int) -> TokenType (type)
    candidate_types = {}
    n_seen = 0
    while chars and active_nodes:
        char = chars.popleft()
        n_seen += 1
        new_nodes = []
        logger.debug(f"char={char}, n_active=({len(active_nodes)})")
        for node in active_nodes:
            try:
                new_nodes.append(node[char])
                if node[char].value:
                    candidate_types[n_seen] = node[char].value
            except KeyError:
                logger.debug(f'Node {node} has no edge for char {char!r}.')
        active_nodes = new_nodes
    if not candidate_types:
        return None
        
    _, cls = max(candidate_types.items(), key=lambda x: x[0])
    return cls


class Token:

    def __init__(self, value: str, line: int, token_type: Optional[type] = None):
        """
        Parameters
        ----------
        value : str
            The chunk of continuguous characters the token is comprised of.
        line : int
            The line number in the source code that the token belongs to. 1-indexed. This helps
            the parser report better error messages later.
        Strongly recommend specifying token_type explicitly.
        # TODO: consider removing implicit token_type option?
        """
        self.line = line
        self.token_type = token_type or TokenTypes.lexeme2type(value)
        self.value = value

    @property
    def name(self) -> str:
        """Returns the first value for our lexed display format:
        <token_type> <lexeme> <literal>
        """
        return self.token_type.name

    @property
    def lexeme(self) -> str:
        """Returns the second value for our lexed display format:
        <token_type> <lexeme> <literal>
        """
        if isinstance(self.token_type.lexeme, str):
            return self.token_type.lexeme
        return self.token_type.lexeme(self.value)

    @property
    def literal(self) -> str:
        """Returns the third value for our lexed display format:
        <token_type> <lexeme> <literal>
        """
        if isinstance(self.token_type.literal, str):
            return self.token_type.literal
        return self.token_type.literal(self.value)

    @property
    def non_null_literal(self) -> str:
        """TODO: still don't really get why this is necessary or if it actually is.
        Book sample parser outputs displays numbers like "3.0" (literals) but operations like 
        ">" (lexemes). So adding this helper here.
        """
        if self.literal != "null":
            return self.literal
        return self.lexeme

    def evaluate(self) -> Union[str, int, float]:
        """When evaluating expressions, we'll want to grab the actual value of the token, converted
        to a numeric type when appropriate. This will distinguish between int and float since this
        is what the book/tests require for evaluation; but note this will not when at the lexing
        and parsing stages, where they expect all numbers to have a trailing decimal place.
        """
        if self.token_type == TokenTypes.NUMBER:
            return to_numeric_if_necessary(self.value)
        
        # TODO: for now we do not convert bools or nils or operators to non-str types,
        # not sure if that's correct.
        # TODO: test seems to want us to convert False -> false. Not sure if this is a bool-specific
        # thing or if lox should be generally case insensitive?
        return self.value

    def lexed(self) -> str:
        """Contains lexed code corresponding to one token, consisting of
        <token_type> <lexeme> <literal>
        e.g. 'STRING "dog" dog'

        If token is not lexable, e.g. SPACE, we return an empty str.
        """
        if not self.token_type.lexable:
            return ""

        return f"{self.name} {self.lexeme} {self.literal}"

    def __repr__(self):
        return f"{type(self).__name__}(value={self.value!r})"

    def __len__(self):
        # Remember some, like STRING, have additional characters that are not present in self.value
        # (in the case of STRING these are leading/trailing " marks).
        return len(self.lexeme)

    @classmethod
    def from_longest_leading_substring(cls, text: str, line: int):
        token_type = infer_token_type(text, RESERVED_TYPES_TRIE) or infer_token_type(text)
        if not token_type:
            raise ValueError(f"Unexpected character: {text[0]}")
        # Note that the called method could still raise an error.
        substring = token_type.longest_leading_substring(text)
        if substring is None:
            raise AssertionError(
                f"Unexpected behavior: inferred token_type={token_type} but could not find a valid "
                f"leading substring from {text!r}."
            )
        return cls(substring, line=line, token_type=token_type)


def lex(source: str) -> dict:
    """Each str contains one line of lexed source code corresponding to a single token, e.g.
    'STRING "dog" dog'

    (Codecrafters wants this to execute when the script is called with the "tokenize" command
    but it does more than just tokenizing.)
    """
    res = []
    tokens = []
    success = True
    line_num = 0
    # TODO: might need to check if the newline is in a str once we support those? Maybe can
    # splitlines at the f.read() level and pass in list[str] to this func.
    lines = deque(source.splitlines())
    while lines:
        line = lines.popleft()
        line_num += 1
        i = 0
        max_idx = len(line) - 1

        # Iterate over characters in a line of source code.
        while i <= max_idx:
            lexed_item = ""

            # Possibly could handle this as a "token" but for now just hardcode it.
            if i < max_idx and line[i:i+2] == "//":
                # Skip to next line of source code.
                break

            token = None
            try:
                token = Token.from_longest_leading_substring(line[i:], line=line_num)
                lexed_item = token.lexed()
                i += len(token)
            except ValueError:
                lexed_item = f"[line {line_num}] Error: Unexpected character: {line[i]}"
                success = False
                i += 1
            except UnterminatedLexeme as e:
                lexed_item = f"[line {line_num}] Error: {e.args[0]}"
                success = False
                # TODO: might need to revisit this logic but at least for strings I think it's ok.
                # We don't want to break like we did for comments because we still need to add
                # our lexed_item to res down below.
                i = max_idx + 1

            if lexed_item:
                res.append(lexed_item)
            if token:
                tokens.append(token)
        
    res.append("EOF  null")
    return {
        "lexed": res,
        "tokenized": tokens,
        "success": success,
    }


# TODO: start of parsing section, move to a different module after finishing codecrafters or
# once I figure out how to more easily test locally.
def truthy(literal: str) -> bool:
    """Determine whether a value is truthy. In Lox, false and nil are considered falsy,
    everything else is considered truthy. Notice that we're passing in a string, NOT a Token.
    """
    return literal not in (ReservedTokenTypes.FALSE.lexeme, ReservedTokenTypes.NIL.lexeme)

        
def to_numeric_if_necessary(val: str) -> Union[int, float, str]:
    """Cast a str to int/float if applicable.
    """
    if val.isdigit():
        return int(val)
    if val.replace(".", "").isdigit():
        return float(val)
    return val


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
        # TODO: seems odd that this is same as __str__, not sure if correct?
        right = self.right.evaluate()
        if self.val.token_type == TokenTypes.BANG:
            return not truthy(right)
        if self.val.token_type == TokenTypes.MINUS:
            return -right

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
        left = self.left.evaluate()
        right = self.right.evaluate()

        # TODO: maybe can give Token/TokenType an optional "op" field so we can just call
        # self.val.op(left, right)?
        try:
            if self.val.token_type == TokenTypes.SLASH:
                return left / right
            if self.val.token_type == TokenTypes.STAR:
                return left * right
            if self.val.token_type == TokenTypes.MINUS:
                return left - right
            if self.val.token_type == TokenTypes.PLUS:
                return left + right
            if self.val.token_type == TokenTypes.GREATER:
                return left > right
            if self.val.token_type == TokenTypes.GREATER_EQUAL:
                return left >= right
            if self.val.token_type == TokenTypes.LESS:
                return left < right
            if self.val.token_type == TokenTypes.LESS_EQUAL:
                return left <= right
            # Note that these two cases rely on lox's definition of equality matching python's.
            # Based on the book definition this seems to be the case.
            if self.val.token_type == TokenTypes.BANG_EQUAL:
                return left != right
            if self.val.token_type == TokenTypes.EQUAL_EQUAL:
                return left == right
        except TypeError:
            raise ParsingError("Unexpected operator in Binary: {self.val.token_type}")

        raise ParsingError("Unexpected operator in Binary: {self.val.token_type}")


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
        return self.val.evaluate()


class ASTPrinter:

    @classmethod
    # TODO: looks like maybe don't need actual AST and ASTNode class? Just use expression classes?
    def _postorder(cls, root: Optional[ASTNode]) -> list[str]:
        if not root:
            return []
        return (
            cls._postorder(getattr(root, "left", None))
            + cls._postorder(getattr(root, "right", None))
            + [root.val]
        )

    @classmethod
    def pprint(cls, ast):
        nodes = cls._postorder(ast)
        print(nodes)


class ParsingError(Exception):
    """Raise when the parser hits an invalid token given the current state."""


# TODO: calling Parser().expression() on "2+3" correctly returns a Binary, but when there are spaces
# it fails. Check how/if book handles it, could always just ignore them for now.
# After that, need to see about loading this into an AST I think?
# TODO: rm decorator once done debugging.
from app.debugging import decorate_methods, verbose
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

    def expression(self) -> Expression:
        """
        Rule:
        expression → equality ;
        """
        # equality is the highest precedence (last to be evaluated) operation.
        return self.equality()

    def parse(self):
        """Parse all tokens in the source code into expressions.

        Returns
        -------
        dict
            expressions: list[str]
            success: bool
            error: Optional[Exception]
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

        return res

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


def main():
    if len(sys.argv) < 3:
        print("Usage: ./your_program.sh tokenize <filename>", file=sys.stderr)
        exit(1)

    command = sys.argv[1]
    filename = sys.argv[2]

    with open(filename) as file:
        file_contents = file.read()

    # You can use print statements as follows for debugging, they'll be visible when running tests.
    print("Logs from your program will appear here!", file=sys.stderr)
    lexed = lex(file_contents)
    if lexed["success"]:
        parser = Parser(lexed["tokenized"])
        parsed = parser.parse()

    # Print results for codecrafters.
    if command == "tokenize":
        for row in lexed["lexed"]:
            print(row, file=sys.stderr if row.startswith("[line") else sys.stdout)
        if not lexed["success"]:
            exit(65)
    elif command == "parse":
        # TODO: can probably find a cleaner solution here but my tokenizer is catching some errors
        # before we reach the parsing stage and Codecrafters wants us to exit in that case.
        # Think codecrafters will eventually check error messages but for now we just exit.
        if not lexed["success"]:
            exit(65)
            # for row in lexed["lexed"]:
            #     if row.lower().startswith("[line "):
                    # print(row)
                    # exit(65)

        # At this point we know `parsed` exists.
        if parsed["success"]:
            for expr in parsed["expressions"]:
                print(expr)
        else:
            print(parsed["error"], file=sys.stderr)
            exit(65)
    elif command == "evaluate":
        # TODO: again, would like to consolidate and raise this only once instead of in each
        # command, but codecrafters is picky about when/where errors are raised. Clean up later.
        if not lexed["success"]:
            exit(65)

        for expr in parsed["expressions"]:
            print(expr.evaluate())
        # TODO: left off here, maybe can move some parsing logic above the if/elif blocks? Need to
        # be careful IIRC so that the tokenization errors are raised at the right time if
        # command='parse'.
    else:
        print(f"Unknown command: {command}", file=sys.stderr)
        exit(1)

    # TODO for easier debugging
    return locals()


if __name__ == "__main__":
    # TODO: for easier debugging
    kwargs = main()
