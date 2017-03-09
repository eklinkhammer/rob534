function myPath = branchAndBound(map, budget)
    goalPoint = [map.sideSize map.sideSize];
    setMStar(0);
    tot = prod(goalPoint);
    [I,J] = ind2sub(goalPoint, [1:tot]);
    X = [I ; J].';
    startPoint = [1 1];
    ippBnBRunner(map, startPoint, goalPoint, budget, [startPoint], X);
    myPath = getPStar;
end

function ippBnBRunner(map, startNode, goalNode, budget, currentPath, X)
    isLeaf = true;
    % If the goal node is ever not the boundary, this will break
    neighborNodes = [startNode ; getNeighbors(startNode, goalNode)];
    for node = 1:size(neighborNodes,1)
        point = neighborNodes(node,:);
        newPath = [currentPath ; point];
        if canReachPoint(point, goalNode, budget - 1)
           isLeaf = false;
           newUpperBound = upperBound(map, point, goalNode, budget - 1, newPath, X);
           if newUpperBound > getMStar
               ippBnBRunner(map, point, goalNode, budget - 1, newPath, X);
           end
        end
    end
    
    if isLeaf
       m = evaluatePath(currentPath, map);
       if m > getMStar
          setMStar(m);
          setPStar(currentPath);
       end
    end
end

function r = getMStar
    global mStar;
    r = mStar;
end

function r = getPStar
    global pStar;
    r = pStar;
end

function setMStar(val)
    global mStar;
    mStar = val;
end

function setPStar(val)
    global pStar;
    pStar = val;
end