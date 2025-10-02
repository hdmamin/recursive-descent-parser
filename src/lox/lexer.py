from collections import deque
from collections.abc import Iterable
from dataclasses import dataclass
from functools import lru_cache
from string import ascii_letters
from typing import Any, Union, Callable, Optional
import logging

from lox.data_structures import Trie
from lox.exceptions import UnterminatedLexeme
from lox.utils import get_interpreter


logger = logging.getLogger(__name__)
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
    # If no valid substring is found, the returned value must be None (NOT an empty str).
    # It can also raise an UnterminatedLexeme error if appropriate (e.g. source code '"abc' opens
    # but does not close a str).
    # If a TokenType consists purely of a predefined str lexeme, e.g.
    # LEFT_PAREN or EQUAL_EQUAL, this does NOT need to be defined explicitly. It's only needed for
    # lexemes like STRING where we don't know upfront what characters the lexeme will consist of.
    longest_leading_substring: Optional[Callable] = None
    reserved: bool = False

    def __post_init__(self):
        if isinstance(self.lexeme, str):
            if self.start is None:
                self.start = self.lexeme[0]
            else:
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

    def _default_longest_leading_substring(self, text: str) -> Optional[str]:
        """The default way to get the longest leading substring for lexemes whose content is known
        upfront, e.g. '>=' or '!'.
        """
        if text.startswith(self.lexeme):
            remaining = text[len(self.lexeme):]
            if self.reserved and remaining and remaining[0].isalnum():
                return None
            return self.lexeme
        return None


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
        # couldfix: seems like this would break down a bit for STRING type given current
        # implementation where lexeme is a lambda (same with NUMBER type).
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
    NEWLINE = TokenType(name="NEWLINE", lexeme="\n", lexable=False)

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


# Don't inherit from TokenTypes because it defines types as class attrs and we don't want all the
# unreserved types to show up here.
class ReservedTokenTypes:

    # Reserved words
    # These are a bit different from other TokenTypes in that we don't place them in the trie
    # in order to avoid clashes with IDENTIFIER. We handle these separately in infer_token_type.
    # Note that we depart slightly from old `start` conventions and use the whole word as the
    # start char.
    AND = TokenType(name="AND", lexeme="and", reserved=True)
    CLASS = TokenType(name="CLASS", lexeme="class", reserved=True)
    ELSE = TokenType(name="ELSE", lexeme="else", reserved=True)
    FALSE = TokenType(name="FALSE", lexeme="false", reserved=True) 
    FOR = TokenType(name="FOR", lexeme="for", reserved=True)
    FUN = TokenType(name="FUN", lexeme="fun", reserved=True)
    IF = TokenType(name="IF", lexeme="if", reserved=True)
    NIL = TokenType(name="NIL", lexeme="nil", reserved=True)
    OR = TokenType(name="OR", lexeme="or", reserved=True)
    PRINT = TokenType(name="PRINT", lexeme="print", reserved=True)
    RETURN = TokenType(name="RETURN", lexeme="return", reserved=True)
    SUPER = TokenType(name="SUPER", lexeme="super", reserved=True)
    THIS = TokenType(name="THIS", lexeme="this", reserved=True)
    TRUE = TokenType(name="TRUE", lexeme="true", reserved=True)
    VAR = TokenType(name="VAR", lexeme="var", reserved=True)
    WHILE = TokenType(name="WHILE", lexeme="while", reserved=True)
    PRINT = TokenType(name="PRINT", lexeme="print", reserved=True)

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


# Hilariously over-engineered but 🤷‍♂️, that's kind of in line with my vision for this project:
# coding for reasons other than producing useful software.
# Each edge corresponds to a single character.
# If a TokenType has multiple valid start characters (like NUMBER), each of those gets its own
# outgoing edge.
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


def to_numeric_if_necessary(val: str) -> Union[int, float, str]:
    """Cast a str to int/float if applicable.
    """
    if val.isdigit():
        return int(val)
    if val.replace(".", "").isdigit():
        return float(val)
    return val


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
        """Still don't really get why this is necessary or if it actually is.
        Book sample parser outputs displays numbers like "3.0" (literals) but operations like 
        ">" (lexemes). So adding this helper here.
        """
        if self.literal != "null":
            return self.literal
        return self.lexeme

    def evaluate(self, **kwargs) -> Any:
        """Return the associated python value represented by a Lox token.
        This is necessary when evaluating expressions: we need to grab the actual value of the
        token so we can perform various operations on it. We will have to cast back to a lox
        datatype before returning.

        Here we distinguish between int and float since this
        is what the book/tests require for evaluation; but note this cannot be used at the lexing
        and parsing stages, where the book expects all numbers to have a trailing decimal place.

        Parameters
        ----------
        kwargs : Any
            Used by Interpreter.Variable to pass in an Expression (key: "expr")
            so we can retrieve the right depth from Interpreter.locals.
        """
        if self.token_type == TokenTypes.STRING:
            return self.value

        if self.token_type == TokenTypes.NUMBER:
            return to_numeric_if_necessary(self.value)

        if self.token_type == ReservedTokenTypes.TRUE:
            return True

        if self.token_type == ReservedTokenTypes.FALSE:
            return False

        if self.token_type == ReservedTokenTypes.NIL:
            return None

        if self.token_type in (
            TokenTypes.IDENTIFIER, ReservedTokenTypes.THIS, ReservedTokenTypes.SUPER
        ):
            interpreter = get_interpreter()
            try:
                depth = interpreter.locals.get(kwargs["expr"], None)
                if depth is None:
                    value = interpreter.global_env.read_state(self.lexeme)
                else:
                    value = interpreter.env.read_state_at(self.lexeme, depth)
            except KeyError as e:
                raise RuntimeError(f"Undefined variable {self.lexeme!r}.\n[line {self.line}]")
            
            return value

        raise RuntimeError(f"Token.evaluate not implemented for {self.token_type}.")

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
        reserved_token_type = infer_token_type(text, RESERVED_TYPES_TRIE)
        token_type = infer_token_type(text)
        if not (token_type or reserved_token_type):
            raise ValueError(f"Unexpected character: {text[0]}")

        # Note that the called method could still raise an error.
        # Important to use None for now, we'll convert to empty str later. None means no leading
        # substring found, empty string could mean we matched a literal empty string in lox.
        substring = reserved_substring = None
        if token_type:
            substring = token_type.longest_leading_substring(text)
        if reserved_token_type:
            reserved_substring = reserved_token_type.longest_leading_substring(text)
        if substring is None and reserved_substring is None:
            raise AssertionError(
                f"Unexpected behavior: inferred token_type={token_type} but could not find a valid "
                f"leading substring from {text!r}."
            )
        
        substring = substring or ""
        reserved_substring = reserved_substring or ""
        if reserved_token_type and len(reserved_substring) >= len(substring):
            longest_substring = reserved_substring
            resolved_token_type = reserved_token_type
        else:
            longest_substring = substring
            resolved_token_type = token_type
        return cls(longest_substring, line=line, token_type=resolved_token_type)


def lex(source_lines: list[str]) -> dict:
    """Each str contains one line of lexed source code corresponding to a single token, e.g.
    'STRING "dog" dog'

    (Codecrafters wants this to execute when the script is called with the "tokenize" command
    but it does more than just tokenizing.)
    """
    res = []
    tokens = []
    success = True
    line_num = 0
    lines = deque(source_lines)
    while lines:
        # Currently process each line individually and view it as self contained. But if we
        # have open quotes or parentheses, a semantic line can actually continue onto the next
        # literal line.
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
            except ValueError:
                lexed_item = f"[line {line_num}] Error: Unexpected character: {line[i]}"
                success = False
                i += 1
            except UnterminatedLexeme as e:
                # Handle case where we're in a multi-line string.
                if line[i] == '"' and lines:
                    line_2 = lines.popleft()
                    line = line + "\n" + line_2
                    line_num += 1
                    max_idx += len(line_2) + 1
                    continue
                else:
                    lexed_item = f"[line {line_num}] Error: {e.args[0]}"
                    success = False
                    # We don't want to break like we did for comments because we still need to add
                    # our lexed_item to res down below.
                    i = max_idx + 1
            else:
                lexed_item = token.lexed()
                i += len(token)

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