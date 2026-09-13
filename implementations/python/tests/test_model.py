import pytest
from ai_invocation import InvocationPlan, validate_invocation
V=InvocationPlan("Reach the cliff safely","rope and map","medium; loss boundary is stop","stay within the marked route")
def test_valid(): assert validate_invocation(V)
@pytest.mark.parametrize("field",["objective","resources","tolerance","constraints"])
def test_missing(field):
    with pytest.raises(ValueError): validate_invocation(InvocationPlan(**(V.__dict__|{field:""})))
def test_tolerance_boundary():
    with pytest.raises(ValueError): validate_invocation(InvocationPlan(V.objective,V.resources,"medium",V.constraints))
def test_tolerance_level():
    with pytest.raises(ValueError): validate_invocation(InvocationPlan(V.objective,V.resources,"careful; loss boundary is stop",V.constraints))
