from pathlib import Path
from setuptools import *

with open(Path(__file__ ).parent / 'README.md', 'r', encoding='utf-8') as fh:
    long_description = fh.read()

setup(
    name="cptextras",
    version="1.2.0",
    author="Kyle Hall",
    author_email="pycpt-help@iri.columbia.edu",
    description=(
        "CPT-EXTRAS"),
    license="MIT",
    keywords="CPT PyCPT ",
    url="https://github.com/iri-pycpt/CPT-EXTRAS",
    packages=[
        'cptextras',
    ],
    package_dir={
        '': 'src',
    },
    python_requires=">=3.4",
    long_description=long_description,
    long_description_content_type='text/markdown',
    classifiers=[
        "Development Status :: 3 - Alpha",
        "Topic :: Utilities",
        "License :: OSI Approved :: MIT License",
        "Operating System :: OS Independent",
    ],
)
