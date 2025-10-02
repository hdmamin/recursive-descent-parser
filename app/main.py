import streamlit as st

from pathlib import Path
import tempfile

from lox.main import main


source = st.text_area("Lox Program", height=300)
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