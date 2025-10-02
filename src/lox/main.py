from contextlib import redirect_stdout
import logging
import sys
from typing import Optional

from lox.interpreter import to_lox_dtype, INTERPRETER
from lox.lexer import lex
from lox.parser import Parser
from lox.utils import maybe_redirect


logger = logging.getLogger()
logging.basicConfig()


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


@maybe_redirect("codecrafters_test", inverse=True)
def main(*, codecrafters_test: bool = True, command=None, source_lines: Optional[list[str]] = None):
    """Process a lox program.

    Parameters
    ----------
    codecrafters_test : bool
        Codecrafters is very particular about how they want the code to run (e.g. how to raise
        errors) and it's not always ideal for other contexts. E.g. when calling from streamlit,
        set to False.
    command : str
        Determines what we do with the lox program. Options are: 
            tokenize
            parse
            evaluate
            run
    source_lines : list[str] or None
        When running codecrafters tests, should be None (we will grab the lox program filename
        from sys.argv[2]). Otherwise, pass in list[str] containing lox code to execute.
        OR pass in a filename as a string.
    """
    if not (command and source_lines) and len(sys.argv) < 3:
        raise_error(
            codecrafters_test,
            ["Usage: ./your_program.sh tokenize <filename>"],
            1
        )

    command = command or sys.argv[1]
    if command is None:
        raise ValueError("Command must not be None.")

    # If None, we assume user is running from command line. If str, we assume this is a path.
    if not isinstance(source_lines, list):
        filename = source_lines or sys.argv[2]
        with open(filename) as file:
            source_lines = [line.removesuffix("\n") for line in file.readlines()]

    # You can use print statements as follows for debugging, they'll be visible when running tests.
    print("Logs from your program will appear here!", file=sys.stderr)
    lexed = lex(source_lines)

    # Print results for codecrafters.
    if command == "tokenize":
        for row in lexed["lexed"]:
            print(row, file=sys.stderr if row.startswith("[line") else sys.stdout)
        if not lexed["success"]:
            raise_error(codecrafters_test, "", 65)

        return

    if lexed["success"]:
        parser = Parser(lexed["tokenized"])
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

    try:
        INTERPRETER.resolve_all(parsed["parsed"])
    except Exception as e:
        raise_error(codecrafters_test, [e], 65)
    parser.reset_index()
    if command == "evaluate":
        if not lexed["success"]:
            raise_error(codecrafters_test, [], 65)

        for expr in parsed["parsed"]:
            try:
                print(to_lox_dtype(expr.evaluate()))
            except RuntimeError as e:
                raise_error(codecrafters_test, [e], 70)
    elif command == "run":
        if not parsed["success"]:
            raise_error(codecrafters_test, parsed["errors"], 65)

        for statement in parsed["parsed"]:
            try:
                statement.evaluate()
            except RuntimeError as e:
                raise_error(codecrafters_test, [e], 70)
    else:
        raise_error(codecrafters_test, f"Unknown command: {command}", 1)

    return parsed


if __name__ == "__main__":
    main()