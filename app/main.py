import logging
import sys
from typing import Any, Union, Optional

from app.data_structures import ASTNode
from app.lexer import (
    Token, TokenType, TokenTypes, ReservedTokenTypes, TYPES_TRIE, RESERVED_TYPES_TRIE, lex
)
from app.parser import Parser


logger = logging.getLogger()
logging.basicConfig()


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


# TODO: prob can delete?
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
            try:
                print(to_lox_dtype(expr.evaluate()))
            except RuntimeError as e:
                print(e, file=sys.stderr)
                exit(70)
        # TODO: left off here, maybe can move some parsing logic above the if/elif blocks? Need to
        # be careful IIRC so that the tokenization errors are raised at the right time if
        # command='parse'.
    elif command == "run":
        pass
        # TODO
    else:
        print(f"Unknown command: {command}", file=sys.stderr)
        exit(1)

    # TODO for easier debugging
    return locals()


if __name__ == "__main__":
    # TODO: for easier debugging
    kwargs = main()
