import logging
import sys
from typing import Optional

from app.data_structures import ASTNode
from app.interpreter import to_lox_dtype
from app.lexer import lex
from app.parser import Parser


logger = logging.getLogger()
logging.basicConfig()


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
    if lexed["success"] and command != "tokenize":
        parser = Parser(lexed["tokenized"])
        parsed = parser.parse(mode=command)
    else:
        # TODO: running into unterminated str errors when the closing quotes are on a different line
        # than the opening quotes.
        parsed = {"success": False}

    # Print results for codecrafters.
    if command == "tokenize":
        for row in lexed["lexed"]:
            print(row, file=sys.stderr if row.startswith("[line") else sys.stdout)
        if not lexed["success"]:
            exit(65)
    elif command == "parse":
        # When tokenizer catches some errors before we reach the parsing stage, Codecrafters
        # wants us to exit early.
        if not lexed["success"]:
            exit(65)

        # At this point we know `parsed` exists.
        if parsed["success"]:
            for expr in parsed["parsed"]:
                print(expr)
        else:
            for row in parsed["errors"]:
                print(row, file=sys.stderr)
            exit(65)
    elif command == "evaluate":
        # TODO: again, would like to consolidate and raise this only once instead of in each
        # command, but codecrafters is picky about when/where errors are raised. Clean up later.
        if not lexed["success"]:
            exit(65)

        # if parsed["success"]:
        # TODO: may need to change key back to declarations, depending
        for expr in parsed["parsed"]:
            try:
                # TODO rm one of these?
                print(to_lox_dtype(expr.evaluate()))
                # expr.evaluate()
            except RuntimeError as e:
                print(e, file=sys.stderr)
                exit(70)
    elif command == "run":
        # TODO: not sure if this is valid, just treating any error here like a syntax error.
        # Might need to modify parser to better distinguish between parsing and syntax errors.
        # The ParsingError at the end of primary() was causing problems in run mode when I checked
        # here specifically for syntaxerrors, but not sure if we rely on that for previous chapter's
        # tests to pass? Really should save all test cases from previous runs so I can run the full
        # past test suite on my own.
        if not parsed["success"]:
            for row in parsed["errors"]:
                print(row, file=sys.stderr)
            exit(65)

        for statement in parsed["parsed"]:
            try:
                statement.evaluate()
            except RuntimeError as e:
                print(e, file=sys.stderr)
                exit(70)
    else:
        print(f"Unknown command: {command}", file=sys.stderr)
        exit(1)

    # TODO for easier debugging
    return locals()


if __name__ == "__main__":
    # TODO: for easier debugging
    kwargs = main()
    from app.environment import Environment
    kwargs["env"] = Environment
    # print(kwargs['parsed'])