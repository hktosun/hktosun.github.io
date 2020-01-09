% This function implements Linear Discriminant Analysis. 
% The inputs are:
%                  data: the data we want to reduce the dimension of.
%                  num_principal_components : number of principal
%                  components returned.
% The outputs are:
%                   W: the projection matrix
%                   D: the vector of eigenvalues

function [W, D] = myLDA(data, num_principal_components)

% Rename the inputs to work easier.
dt = data;
L = num_principal_components;

% The number of inputs
d = size(dt, 2) - 1;


m = zeros(d,1);
S_w = zeros(d, d);
S_b = zeros(d, d);
n = zeros(10,1);

% For each class, we calculate the mean m_i and covariance S_i.
% Then we sum them up to get m and S_w. We later divide m by K (the number
% of classes) to get the correct m.

for(i = 0:9)
	
    dt_i = dt(dt(:,d+1)== i, 1:d);

    n_i = size(dt_i,1);
		
    m_i = mean(dt_i(:,1:d))';
	
    m = m + m_i;
    S_i = zeros(d,d);

	for(t = 1:n_i)
		x_t = (dt_i(t, 1:d))';
		S_i = S_i + (x_t-m_i)*(x_t-m_i)';
    end
	S_w = S_w + S_i;
end

m = (1/10).*m;

% We get S_b by using the following formula.
for(i = 0:9)
    dt_i = dt(dt(:,d+1)== i, 1:d);
    n_i = size(dt_i,1);	
    m_i = mean(dt_i(:,1:d))';
	S_b = S_b + n_i.*((m_i-m)*(m_i-m)');
end


% Finally, we calculate the eigenvectors and eigenvalues of the
% (S_w)^{-1}S_b. 

[V, D] = eig(inv(S_w)*S_b);

% Then, we sort the eigenvalues in descending order.

[D, ind] = sort(diag(D), 'descend');
% Here, we sort the matrix so that the column associated with the
% biggest eigenvalue will be the first column, and so on.
    
for(i = 1:length(D))
    V2(:,i) = V(:,ind(i));
end

% Now, we have the full eigenvector matrix. However, we are only
% interested in L of them. So we take the first L columns of
% eigenvectors, and first L eigenvalues.
W = V2(:,1:L);

D = D(1:L);

end