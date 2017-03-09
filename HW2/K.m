% Produces a matrix K, where the element at (i,j) = kP(A_i, B_j)
function val = K(A,B)
    val = zeros(size(A,1), size(B,1));
    %tot = numel(val);
    %[I,J] = ind2sub(size(val), [1:tot]);
    for i = 1:size(A,1)
        for j = 1:size(B,1)
            val(i,j) = kP(A(i,:), B(j,:));
        end
    end
    %for x = 1:tot
    %   val(I(x),J(x)) = kP(A(I(x)), B(J(x))); 
    %end
end

function v = kP(i,j)
    v = exp( - norm(i - j) / 2);
end
