from pathlib import Path
from setuptools import setup

readme_path = Path(__file__).parent / 'README.md'
long_description = readme_path.read_text(encoding='utf-8')

setup(
    name = "pycpt",
    version = "2.9.4",
    author = "IRI",
    author_email = "pycpt-help@iri.columbia.edu",
    description = ("Python Interface to the International Research Institute for Climate & Society's Climate Predictability Tool "),
    license = "MIT",
    keywords = ["climate", 'predictability', 'prediction', 'precipitation', 'temperature', 'data', 'IRI'],
    url = "https://iri.columbia.edu/our-expertise/climate/tools/",
    packages=[
        'cptcore',
        'cptcore.functional',
        'cptdl',
        'cptextras',
        'pycpt',
    ],
    package_data={},
    package_dir={
        '': 'src',
    },
    python_requires=">=3.8",
    long_description=long_description,
    long_description_content_type='text/markdown',
    classifiers=[
        "Development Status :: 3 - Alpha",
        "Topic :: Utilities",
        "License :: OSI Approved :: MIT License",
	"Operating System :: OS Independent",
    ],
    entry_points = {
        'console_scripts': [
            # Warning: these must be specified redundantly in the
            # conda recipe. Otherwise they work on linux and macos but
            # not on windows.
            'generate-forecasts-from-config=pycpt.commands:generate_forecasts',
            'upload-forecasts-from-config=pycpt.commands:upload_forecasts',
        ],
    },
)
