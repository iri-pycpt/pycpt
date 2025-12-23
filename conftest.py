def pytest_addoption(parser):
    # Add the custom command-line flag
    parser.addoption(
        "--record", action="store_true", default=False,
        help="Make real network requests and record the responses"
    )
