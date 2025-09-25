from contextlib import redirect_stdout
import logging
import sys
from typing import Optional

from lox.data_structures import ASTNode
from lox.interpreter import to_lox_dtype, INTERPRETER
from lox.lexer import lex
from lox.parser import Parser
from lox.utils import maybe_redirect


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


def raise_error(codecrafters_test: bool, errors: list, exit_code: int):
    """Codecrafters tests make us surface errors in a specific way which is not ideal for
    other contexts, e.g. streamlit (discards some info). In codecrafters_test mode we print to
    stderr and sys.exit. Otherwise, we raise an Exception with the same message.
    """
    if codecrafters_test:
        for error in errors:
            print(error, file=sys.stderr)
        exit(exit_code)
    raise Exception(*errors)


class ListWriter:

    def __init__(self):
        self._items = []

    def write(self, val: str):
        # Otherwise we get extra items from print default `end` arg.
        if val.rstrip("\n"):
            self._items.append(val)

    def flush(self):
        pass

    def items(self):
        return self.items


# TODO: revamp to better support streamlit? All these prints and exit codes don't play nicely with
# streamlit.
@maybe_redirect("codecrafters_test", inverse=True)
def main(*, codecrafters_test: bool = True, command=None, source_code: Optional[str] = None):
    if not (command and source_code) and len(sys.argv) < 3:
        raise_error(
            codecrafters_test,
            ["Usage: ./your_program.sh tokenize <filename>"],
            1
        )

    command = command or sys.argv[1]
    if command is None:
        raise ValueError("Command must not be None.")

    if source_code is None:
        filename = sys.argv[2]
        with open(filename) as file:
            source_code = file.read()

    # You can use print statements as follows for debugging, they'll be visible when running tests.
    print("Logs from your program will appear here!", file=sys.stderr)
    lexed = lex(source_code)

    # Print results for codecrafters.
    if command == "tokenize":
        for row in lexed["lexed"]:
            print(row, file=sys.stderr if row.startswith("[line") else sys.stdout)
        if not lexed["success"]:
            raise_error(codecrafters_test, "", 65)

        return

    if lexed["success"]:
        parser = Parser(lexed["tokenized"])
        # TODO: may need to revisit this if more modes are added but for now, every remaining mode
        # (parse, evaluate, run) requires parsing to be run next.
        parsed = parser.parse(mode="parse")
    else:
        parsed = {"success": False}

    if command == "parse":
        # When tokenizer catches some errors before we reach the parsing stage, Codecrafters
        # wants us to exit early.
        if not lexed["success"]:
            raise_error(codecrafters_test, [], 65)

        # At this point we know `parsed` exists.
        if parsed["success"]:
            for expr in parsed["parsed"]:
                print(expr)
        else:
            raise_error(codecrafters_test, parsed["errors"], 65)

        return

    # TODO maybe only resolve if parser succeeded? Need to be a little careful to raise the right
    # error at the right point depending on what codecrafters expects.
    try:
        INTERPRETER.resolve_all(parsed["parsed"])
    except Exception as e:
        raise_error(codecrafters_test, [e], 65)
    parser.reset_index()
    # TODO testing (noticed parse and run use same mode, eval will prob break but will get to that)
    # res = parser.parse(mode=command)
    res = parsed
    if command == "evaluate":
        # TODO: again, would like to consolidate and raise this only once instead of in each
        # command, but codecrafters is picky about when/where errors are raised. Clean up later.
        if not lexed["success"]:
            raise_error(codecrafters_test, [], 65)

        # if parsed["success"]:
        # TODO: may need to change key back to declarations, depending
        for expr in res["parsed"]:
            try:
                print(to_lox_dtype(expr.evaluate()))
            except RuntimeError as e:
                raise_error(codecrafters_test, [e], 70)
    elif command == "run":
        # TODO: not sure if this is valid, just treating any error here like a syntax error.
        # Might need to modify parser to better distinguish between parsing and syntax errors.
        # The ParsingError at the end of primary() was causing problems in run mode when I checked
        # here specifically for syntaxerrors, but not sure if we rely on that for previous chapter's
        # tests to pass? Really should save all test cases from previous runs so I can run the full
        # past test suite on my own.
        if not res["success"]:
            print('main:', res) # TODO rm
            raise_error(codecrafters_test, res["errors"], 65)

        for statement in res["parsed"]:
            try:
                statement.evaluate()
            except RuntimeError as e:
                raise_error(codecrafters_test, [e], 70)
    else:
        raise_error(codecrafters_test, f"Unknown command: {command}", 1)

    return res # TODO testing streamlit, think some obj in locals must be breaking streamlit
    # TODO for easier debugging
    # return locals()


if __name__ == "__main__":
    # TODO: for easier debugging
    kwargs = main() or {}
    from lox.environment import Environment
    kwargs["env"] = Environment
    # print(kwargs['parsed'])