

function path = greedy(map,budget)
    current = [1 1];
    path = [current];
    for b = 0:(budget-1)
        current = nextStep(map, budget - b, current);
        path = [path; current];
    end
end



% Returns the point that maximizes the information gained that is 
% adjacent to the current point and from which it is still possible to
% reach the goal given the budget.
function next = nextStep(map, budget, current)
    goal = getGoal(map);
    neighbors = getNeighbors(current, goal);
    numNeighbors = size(neighbors,1);
    bestScore = 0;
    next = [];
    for p = 1:numNeighbors
        point = neighbors(p,:);
        if canReachPoint(point,goal,budget - 1)
           newScore = findInformation(point(1), point(2), map);
           if newScore > bestScore
              bestScore = newScore;
              next = point;
           end
        end
    end
end

% Returns the bottom-right most point of map
function goal = getGoal(map)
    goal = [map.sideSize map.sideSize];
end


