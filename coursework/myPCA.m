% This function calculates 'num_principal_components' of the 'data'.
% It returns PC: the matrix of principal components
%             E: the vector of eigenvalues


function [PC, E] = myPCA(data, num_principal_components)

	dt = data;
	K = num_principal_components;
    
    % Before we start finding the principal components, we should first
    % standardize the data, i.e. make each column have mean 0 and standard
    % deviation 1, as suggested by the textbook, p.124 (3rd ed.).
    
    
	dt = [zscore(dt(:,1:end-1)), dt(:,end)];
    
    
    % Now, we calculate the variance-covariance matrix of the standardized
    % data, and calculate the eigenvalues and eigenvectors of it.
    
    [V, D] = eig(cov(dt(:,1:end-1)));
    
    % Then, we sort the eigenvalues in descending order.
    
    [D, ind] = sort(diag(D), 'descend');
    
    % Here, we sort the matrix so that the column associated with the
    % biggest eigenvalue will be the first column, and so on.
    
    for(i = 1:length(D))
        V2(:,i) = V(:,ind(i));
    end
    
    % Now, we have the full eigenvector matrix. However, we are only
    % interested in K of them. So we take the first K columns of
    % eigenvectors.
    
	PC = V2(:,1:K);
    
    % E is the vector of biggest K eigenvalues, sorted in descending order.
	E = D(1:K);
    
end

	