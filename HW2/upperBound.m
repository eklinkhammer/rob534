function info = upperBound(map, point, goalNode, budget, currentPath, X)
    info = evaluatePath(currentPath, map);
    for i = 1:size(X,1)
        point_i = X(i,:);
        costToReach = L(point, point_i) + L(point_i, goalNode);
        if costToReach <= budget
           info = info + findInformation(point_i(1), point_i(2), map);
        end
    end
end

function val = L(X,Y)
    val = sum(abs(X - Y));
end