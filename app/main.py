import streamlit as st

# TODO: tmp hack to import lox code, should switch to editable install or something
from pathlib import Path
import sys
project_path = str(Path(__file__).parent.parent)
if project_path not in sys.path:
    sys.path.insert(0, project_path)

from lox.main import main


source = st.text_area("Lox Program")
if source:
    print('source:', source)
    # TODO: main is never returning (also, raelized it only prints stuff, doesn't return, per
    # codecrafters requirments. But seems like we need to first figure out why it's not even
    # finishing running.)
    try:
        result = main(codecrafters_test=False, command="run", source_code=source)
        for line in result:
            st.text(line)
    except Exception as e:
        for error in e.args:
            st.error(str(error))