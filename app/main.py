from collections import deque
from dataclasses import dataclass
from functools import lru_cache
import logging
import sys
from typing import Union, Callable

from data_structures import Trie


logger = logging.getLogger()
logging.basicConfig()


@dataclass
class TokenType:

    name: str
    # If a callable, this should be a function that takes one arg (the source token string) and
    # returns a string (the corresponding lexeme to display).
    lexeme: Union[str, Callable]
    # Most token types have literal=None, but for some (e.g. strings) it's the source token itself.
    has_literal: bool = False
    # False means this token should not show up in lexed output.
    lexable: bool = True


class TokenTypes:

    @classmethod
    @lru_cache()
    def _lexemes2types(cls) -> dict:
        # Returns a mapping from each lexeme (e.g. "{") to its corresponding TokenType object.
        # TODO: this breaks down for STRING type given current implementation where lexeme is a
        # lambda.
        return {
            v.lexeme: v for k, v in vars(cls).items()
            if k.isupper() and isinstance(v, TokenType)
        }

    @classmethod
    def lexeme2type(cls, lexeme: str) -> TokenType:
        """Maps a single lexeme, e.g. "}", to its coresponding TokenType."""
        return cls._lexemes2types()[lexeme]

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
    EOF = TokenType(name="EOF", lexeme=" ")

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
    STRING = TokenType(name="STRING", lexeme=lambda x: '"' + x + '"', has_literal=True)


class Token:

    def __init__(self, value: str):
        self.token_type = TokenTypes.lexeme2type(value)
        self.value = value

    def lexed(self) -> str:
        """Contains lexed code corresponding to one token, consisting of
        <token_type> <lexeme> <literal>
        e.g. 'STRING "dog" dog'

        If token is not lexable, e.g. SPACE, we return an empty str.
        """
        if not self.token_type.lexable:
            return ""

        res = f"{self.token_type.name} {self.token_type.lexeme} "
        if self.token_type.has_literal:
            res += self.value
        else:
            res += "null"
        return res


"""
- currently we hardcode: try 2 chars, then try 1 if that fails, then error if that fails
- but we want to be able to capture arbitrarily long tokens (e.g. string values)
- one thing we can do is let TokenType define a stopping condition. Then if we know the correct
type upfront, we can just try adding chars until we hit that condition.
    - one problem: currently we don't really have a good way to grab the correct token type upfront.
    We just try a 2 char token type, then a char token type.
    - in some cases there is ambiguity here. If we have an =, we could be in either "==" or "=".
    - kind of smelling a Trie here. Like we traverse the trie until we hit a leaf node? But I guess
    for some types like strings we have arbitrary/infinite valid sequences.
        - but I guess even for strs we know they start with ". So that can still help us identify
        the correct token type initially.
- plan:
    - construct a trie upfront of tokentypes
    - when we get a new char, use the trie to select candidate token types (this should return a
    node or raise error). We can step forward char by char and try to extend each candidate
    token type.
    - eventually, each candidate should either reveal itself to be not a match OR hit a leaf node.
    We can then take the leaf node that uses the longest sequence.
    - challenge: how to handle STRING type (the main one to benefit from this tbh)? Could place
    types in Trie based on `lexeme or start_sequence`, where most types use the lexeme itself but
    STRING and other dynamic lexemes can define this separately.
"""
def lex(source: str) -> dict:
    """Each str contains one line of lexed source code corresponding to a single token, e.g.
    'STRING "dog" dog'
    """
    res = []
    success = True
    line_num = 0
    # TODO: might need to check if the newline is in a str once we support those?
    lines = deque(source.splitlines())
    while lines:
        line = lines.popleft()
        line_num += 1
        i = 0
        max_idx = len(line) - 1

        # Iterate over characters in a line of source code.
        while i <= max_idx:
            lexed_item = ""
            token = None
            try:
                chunk = line[i:i+2]
                # TODO: this logic will prob need to change as we add support for longer lexemes and
                # multilexed_item code, but for current goal of supporting comments it should work.
                if chunk == "//":
                    # Skip to next line of source code.
                    break
                token = Token(chunk)
                # Usually 2, but as we near the end of the str it could be less.
                i += len(chunk)
            except KeyError:
                try:
                    token = Token(line[i])
                except KeyError:
                    lexed_item = f"[line {line_num}] Error: Unexpected character: {line[i]}"
                    success = False
                finally:
                    i += 1
            lexed_item = lexed_item or token.lexed()
            if lexed_item:
                res.append(lexed_item)
        
    res.append(TokenTypes.EOF("").lexed())
    return {
        "lexed": res,
        "success": success,
    }


def main():
    if len(sys.argv) < 3:
        print("Usage: ./your_program.sh tokenize <filename>", file=sys.stderr)
        exit(1)

    command = sys.argv[1]
    filename = sys.argv[2]

    if command != "tokenize":
        print(f"Unknown command: {command}", file=sys.stderr)
        exit(1)

    with open(filename) as file:
        file_contents = file.read()

    # You can use print statements as follows for debugging, they'll be visible when running tests.
    print("Logs from your program will appear here!", file=sys.stderr)

    # Uncomment this block to pass the first stage
    lexed = lex(file_contents)
    for row in lexed["lexed"]:
        print(row, file=sys.stderr if row.startswith("[line") else sys.stdout)
    if not lexed["success"]:
        exit(65)


if __name__ == "__main__":
    main()
