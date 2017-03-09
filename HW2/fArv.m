% Defined in section 2 subsection C Sensor Placement in Sukhatme's paper
% Average reduction in variance
% X is list of all points
% Prior is the trace of K(X,X)
function val = fArv(A, X, prior)
    val = (1 / size(A,1)) * (prior - trace(covPost(A,X)));
end