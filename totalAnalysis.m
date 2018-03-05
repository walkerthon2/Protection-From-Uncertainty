function output = totalAnalysis(cond, pStart, pEnd)

ProcessEyeData_Stage1(cond, pStart, pEnd);
ProcessEyeData_Stage2(cond, pStart, pEnd);

totalFixations_Stage1(cond, pEnd);
totalFixations_Stage2(cond, pEnd);

output = Unc2Analysis(cond, pEnd);
% output = Unc2Analysis_noSDtest(cond, pEnd);

end