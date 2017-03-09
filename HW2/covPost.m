function sPost = covPost(A,X)
    sPost = K(X,X) - K(X,A) * inv(K(A,A)) * K(A,X);
end