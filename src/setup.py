from setuptools import setup, find_packages

setup(
    name="lox",
    version="0.1.0",
    packages=find_packages(include=["lox", "lox.*"]),
)
