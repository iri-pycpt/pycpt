import os
from pathlib import Path
from setuptools import *

readme_path = Path(__file__).parent / 'README.md'
long_description = readme_path.read_text(encoding='utf-8')

setup(
    name = "cptio",
    version = "1.0.4",
    author = "Kyle Hall",
    author_email = "pycpt-help@iri.columbia.edu",
    description = ("Tools bridging Xarray and CPT"),
    license = "MIT",
    keywords = ["climate", 'predictability', 'prediction', 'precipitation', 'temperature', 'data', 'IRI'],
    url = "https://iri.columbia.edu/our-expertise/climate/tools/",
    packages=[
        'cptio', 
        'cptio.fileio', 
        'cptio.utilities',
    ],
    package_dir={
        '': 'src', 
    },
    python_requires=">=3.0",
    long_description=long_description,
    long_description_content_type='text/markdown',
    classifiers=[
        "Development Status :: 3 - Alpha",
        "Topic :: Utilities",
        "License :: OSI Approved :: MIT License",
		"Operating System :: OS Independent",
    ],
)
