from setuptools import *
import os

with open('{}/../README.md'.format(os.getenv('RECIPE_DIR')), 'r', encoding='utf-8') as fh:
    long_description = fh.read()

setup(
    name="cptextras",
    version="1.0.3",
    author="Kyle Hall",
    author_email="kjhall@iri.columbia.edu",
    description=(
        "CPT-EXTRAS"),
    license="MIT",
    keywords="CPT PyCPT ",
    url="https://github.com/kjhall-iri/CPT-EXTRAS",
    packages=[
        'cptextras',
        ],
    package_data={
    },
    package_dir={
        'cptextras': '{}/../src'.format(os.getenv('RECIPE_DIR')),
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
