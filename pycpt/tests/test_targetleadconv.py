import pytest

import cptdl.targetleadconv as t

day_cases = [
    ('Sep-Nov', 91),
    ('Jun', 30),
    ('Jan-Mar', 90.25), # Feb is counted as 28.25
    ('Nov-Jan', 92),
]

@pytest.mark.parametrize('season,length', day_cases)
def test_seasonal_target_length(season, length):
    assert t.seasonal_target_length(season) == length

month_cases = [
    ('Sep-Nov', 3),
    ('Jun', 1),
    ('Jan-Apr', 4),
    ('Dec-Feb', 3)
]

@pytest.mark.parametrize('season,length', month_cases)
def test_seasonal_target_length_monthly(season, length):
    assert t.seasonal_target_length_monthly(season) == length
