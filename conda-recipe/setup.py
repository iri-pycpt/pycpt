from setuptools import *
import os

with open('{}/../README.md'.format(os.getenv('RECIPE_DIR')), 'r', encoding='utf-8') as fh:
	long_description= fh.read()

setup(
    name = "cptcore",
    version = "1.0.6",
    author = "Kyle Hall",
    author_email = "kjhall@iri.columbia.edu",
    description = ("Python Interface to the International Research Institute for Climate & Society's Climate Predictability Tool "),
    license = "MIT",
    keywords = ["climate", 'predictability', 'prediction', 'precipitation', 'temperature', 'data', 'IRI'],
    url = "https://iri.columbia.edu/our-expertise/climate/tools/",
    packages=[  'cptcore', 'cptcore.functional', 'cptcore.tests' ],
    package_data={ 
        'cptcore': ['{}/../src/fortran'.format(os.getenv('RECIPE_DIR')) + ''.join(['/*' for i in range(j) ]) for j in range(10) ],
        'cptcore.tests': ['{}/../src/tests/data/seasonal/*'.format(os.getenv('RECIPE_DIR')), '{}/../src/tests/data/subseasonal/*'.format(os.getenv('RECIPE_DIR'))]
    },
	package_dir={ 
        'cptcore': '{}/../src'.format(os.getenv('RECIPE_DIR')), 
        'cptcore.tests': '{}/../src/tests'.format(os.getenv('RECIPE_DIR')), 
        'cptcore.functional': '{}/../src/functional'.format(os.getenv('RECIPE_DIR')), 
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
