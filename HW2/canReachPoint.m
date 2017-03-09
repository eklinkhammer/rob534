% True if there exists sufficient budget to go straight to the goal from 
% current, and false otherwise. 
function canReach = canReachPoint(current, goal, budget)
    manhattan_distance = abs (goal(1) - current(1) + goal(2) - current(2));
    canReach = manhattan_distance <= budget;
end