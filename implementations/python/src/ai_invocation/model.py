from dataclasses import dataclass
@dataclass(frozen=True)
class InvocationPlan:
    objective: str
    resources: str
    tolerance: str
    constraints: str
    def validate(self):
        for name, minimum in (("objective",8),("resources",3),("tolerance",4),("constraints",8)):
            value=getattr(self,name)
            if not isinstance(value,str) or len(value.strip())<minimum: raise ValueError(f"{name} must be explicit")
        t=self.tolerance.lower()
        if not any(x in t for x in ("low","med","high")): raise ValueError("tolerance must include low, med, or high")
        if not any(x in t for x in ("loss","accept","stop","abort","boundary")): raise ValueError("tolerance must include an explicit loss boundary")
def validate_invocation(plan): plan.validate(); return True
