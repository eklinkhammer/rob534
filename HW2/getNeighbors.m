% Returns a matrix with each row being the coordinates of a valid
% adjacent step. A valid position is one within the rectangle defined by 
% bounds and the point (1,1)
function neighbors = getNeighbors(current, bounds)
    all_neighbors = [current];
    if current(1) + 1 <= bounds(1)
        left = [(current(1) + 1) current(2)];
        all_neighbors = [left ; all_neighbors];
    end
    if current(1) - 1 > 0
        right = [(current(1) - 1) current(2)];
        all_neighbors = [right ; all_neighbors];
    end
    if current(2) + 1 <= bounds(2)
        down = [current(1) (current(2) + 1)];
        all_neighbors = [down ; all_neighbors];
    end
    if current(2) - 1 > 0
        up = [current(1) (current(2) - 1)];
        all_neighbors = [up ; all_neighbors];
    end
    neighbors = all_neighbors;
end