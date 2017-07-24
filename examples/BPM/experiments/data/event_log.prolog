activity(register).
activity(analyze_defect).

case(case137).
case(case178).

activityInstance(instance311).
activityInstance(instance309).
activityInstance(instance662).
activityInstance(instance656).

activityInstanceActivity(instance311,register).
activityInstanceActivity(instance309,analyze_defect).
activityInstanceActivity(instance662,register).
activityInstanceActivity(instance656,analyze_defect).

activityInstanceCase(instance311,case137).
activityInstanceCase(instance309,case137).
activityInstanceCase(instance662,case178).
activityInstanceCase(instance656,case178).

% I deleted the first argument in the predicate below since updateSDE has two arguments only
updateSDE(0, 327240000):-
% Case 137
assert(happensAtIE(complete(instance311, register, case137), 286860000)),
assert(happensAtIE(start(instance309, analyze_defect, case137), 286860000)),
assert(happensAtIE(complete(instance309, analyze_defect, case137), 287460000)),
% Case 178
assert(happensAtIE(complete(instance662, register, case178), 326640000)),
assert(happensAtIE(start(instance656, analyze_defect, case178), 326640000)),
assert(happensAtIE(complete(instance656, analyze_defect, case178), 327240000)).
