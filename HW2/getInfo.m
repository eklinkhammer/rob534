z = zeros(10,10);

for i = 1:10
    for j = 1:10
        z(i,j) = findInformation(i,j,mapTestBig);
    end
end

z