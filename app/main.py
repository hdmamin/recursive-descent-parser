from __future__ import annotations
import streamlit as st

# TODO: tmp hack to import lox code, should switch to editable install or something
from pathlib import Path
import sys
project_path = str(Path(__file__).parent.parent)
if project_path not in sys.path:
    sys.path.insert(0, project_path)
print(sys.path)

from lox.main import main


source = st.text_area("Lox Program")
result = main(source)
st.text(result)