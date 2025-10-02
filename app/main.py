import streamlit as st

from pathlib import Path
import tempfile

from lox.main import main


st.markdown(
    """
    # Lox Interpreter
    An implementation of the Lox programming language defined in
    [Crafting Interpreters](https://craftinginterpreters.com/).
    """
)
with st.expander("Syntax Cheatsheet"):
    st.markdown(
        """
        ```lox
        // Comments
        print "Hello"; 
        var x = 42; x = x + 1;

        // Types: number, string "s", true/false, nil

        // Control
        if (x > 10) print "big"; else print "small";
        while (x < 5) { print x; x = x + 1; }
        for (var i = 0; i < 3; i = i + 1) print i;

        // Functions
        fun greet(name) { print "Hi " + name; }
        greet("Ada");

        // Classes
        class A { speak() { print "noise"; } }
        class B < A { speak() { print "woof"; } }
        var d = B(); d.speak();

        // Operators: == != ! and or < <= > >=
        """
    )
source = st.text_area("Write a lox program below", placeholder='print "hello word";', height=300)
if source:
    try:
        # hack: turns out lexing is slightly harder if we don't have readlines() to rely on in
        # lox.main.main so we write source code to a temporary file.
        with tempfile.NamedTemporaryFile(mode="w") as file:
            file.write(source)
            file.seek(0)
            result = main(codecrafters_test=False, command="run", source_lines=file.name)
        for line in result:
            st.text(line)
    except Exception as e:
        for error in e.args:
            st.error(str(error))
