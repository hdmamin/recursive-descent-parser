from dataclasses import dataclass
from functools import lru_cache
import logging
import sys


logger = logging.getLogger()
logging.basicConfig()


@dataclass
class TokenType:

    name: str
    lexeme: str
    has_literal: bool = False
    # False means this token should not show up in lexed output.
    lexable: bool = True


class TokenTypes:

    @classmethod
    @lru_cache()
    def _lexemes2names(cls) -> dict:
        # Returns a mapping from each lexeme (e.g. "{") to its corresponding TokenType object.
        return {
            v.lexeme: v for k, v in vars(cls).items()
            if k.isupper() and isinstance(v, TokenType)
        }

    @classmethod
    def lexeme2name(cls, lexeme: str) -> TokenType:
        """Maps a single lexeme, e.g. "}", to its coresponding TokenType."""
        return cls._lexemes2names()[lexeme]

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


class Token:

    def __init__(self, value: str):
        self.token_type = TokenTypes.lexeme2name(value)
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


def lex(source: str) -> dict:
    """Each str contains one line of lexed source code corresponding to a single token, e.g.
    'STRING "dog" dog'
    """
    res = []
    success = True
    max_idx = len(source) - 1
    i = 0
    while i <= max_idx:
        line = None
        try:
            chunk = source[i:i+2]
            # TODO: this logic will prob need to change as we add support for longer lexemes and
            # multiline code, but for current goal of supporting comments it should work.
            if chunk == "//":
                break
            token = Token(chunk)
            # Usually 2, but as we near the end of the str it could be less.
            i += len(chunk)
        except KeyError:
            try:
                token = Token(source[i])
            except KeyError:
                # TODO: codecrafters wants us to hardcode i to 1 for now
                line = f"[line 1] Error: Unexpected character: {source[i]}"
                success = False
            finally:
                i += 1
        line = line or token.lexed()
        if line:
            res.append(line)

    res.append("EOF  null")
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
