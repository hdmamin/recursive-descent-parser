class ParsingError(Exception):
    """Raise when the parser hits an invalid token given the current state."""


class ResolutionError(Exception):
    """For errors that occur during the resolution stage. Post-parsing, pre-runtime."""


class UnterminatedLexeme(Exception):
    """For when we encounter a lexeme that starts with a valid character but does not end with one.
    This lets us raise a single error for the whole sequence rather than a separate one for each
    character.
    """