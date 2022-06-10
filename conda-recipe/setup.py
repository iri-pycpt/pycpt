from setuptools import *
import os

with open('{}/../README.md'.format(os.getenv('RECIPE_DIR')), 'r', encoding='utf-8') as fh:
	long_description= fh.read()


setup(
    name = "cptdl",
    version = "1.0.0",
    author = "Kyle Hall",
    author_email = "kjhall@iri.columbia.edu",
    description = ("Tools bridging Xarray and CPT"),
    license = "MIT",
    keywords = ["climate", 'predictability', 'prediction', 'precipitation', 'temperature', 'data', 'IRI'],
    url = "https://iri.columbia.edu/our-expertise/climate/tools/",
    packages=[  'cptdl', 
                'cptdl.', 
                'cptdl.utilities', ],
	package_dir={ 'cptdl': '{}/../src'.format(os.getenv('RECIPE_DIR')), 
                  'cptdl.': '{}/../src/fileio'.format(os.getenv('RECIPE_DIR')),
                  'cptdl.utilities': '{}/../src/utilities'.format(os.getenv('RECIPE_DIR')),
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