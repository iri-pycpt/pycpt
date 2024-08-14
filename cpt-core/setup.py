from pathlib import Path
from setuptools import *
import os

readme_path = Path(__file__).parent / 'README.md'
long_description = readme_path.read_text(encoding='utf-8')

setup(
    name = "cptcore",
    version = "2.2.1",
    author = "IRI",
    author_email = "pycpt-help@iri.columbia.edu",
    description = ("Python Interface to the International Research Institute for Climate & Society's Climate Predictability Tool "),
    license = "MIT",
    keywords = ["climate", 'predictability', 'prediction', 'precipitation', 'temperature', 'data', 'IRI'],
    url = "https://iri.columbia.edu/our-expertise/climate/tools/",
    packages=[  'cptcore', 'cptcore.functional', 'cptcore.tests' ],
    package_data={ 
        'cptcore.tests': [
            'data/seasonal/*',
            'data/subseasonal/*',
        ]
    },
    package_dir={ 
        '': 'src'
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
