% This function implements K-nearest-neighbor algorithm.
% The inputs are 
%                   training_data: the data we use to train our learning
%                                  algoritm
%                   test_data    : the data we use to predict its outcome
%                   k            : number of nearest neighbors that we take
%                                  into account

% The function returns the predictions for the test data.
% We assume the output variable is a categorical variable. 
% We use the Euclidian distance between the two points as our distance
% measure.
% For Euclidian distance to be meaningful, we standardize the data, i.e. 
% make each input column have mean 0 and variance 1.
% In case there is a tie between output values, we randomize among the
% candidate output values.

function pred = myKNN(training_data, test_data, k)
    
    % Rename the input to use it easily.
	tr = training_data;
	te = test_data;
    
    
    % The number of input vectors.
    d = size(tr, 2) - 1;
    
    
    % Here, we standardize the input so that the Euclidian distance between
    % the two points truely reflects how much the points are away from
    % each other.
    
    tr = [zscore(tr(:,1:end-1)), tr(:,end)];
    te = zscore(te(:,1:d));    
    
    
    % The number of observations in each data set.
	N1 = size(tr, 1);
	N2 = size(te, 1);
    
	D = zeros(N1, N2);
    
    % Possible values for the output variable.
    classes = unique(tr(:,end));
    numclass = length(classes);
    
    % nn keeps the classes of nearest neighbors of each observation in the
    % test data.
    % Each column of nn corresponds to an observation in the test
    % data. For each column, the observations in the rows are sorted in a
    % way that the first row has the class of the closest observation in
    % the training data.
    
	nn = zeros(numclass, N2);
    
    % pred stands for the prediction. It keeps the predicted class for each
    % observation in the test data. Prediction will be done by the KNN
    % algorithm.
	pred = zeros(N2,1);
	
    
    
	for(j = 1:N2)
        % Take each observation in the test data one by one.
		x = te(j, 1:d);
        % Calculate its distance to each observation in the test data.
		for(i = 1:N1)
			xt = tr(i, 1:d);
            % D(i,j) is the distance of i'th observation in the training
            % data to the j'th observation in the test data.
			D(i, j) = norm(x - xt);
        end
        
        %  We sort each column of D matrix, so that the top row corresponds
        %  to the closest observation to j. The vector r is a vector of
        %  indices of the training data. The first item in r corresponds to
        %  the index of the closest observation in the training data to the
        %  jth observation of the test data.
        
		[~, r] = sort(D(:,j));

        % Now, we count the occurences of each class in the closest k neighbors.
        % nn(h,j) corresponds to the number of nearest neighbors of
        % observation j that has an output value h.
		for(k1 = 1:k)
			for(h = 1:numclass)
				nn(h,j) = nn(h,j) + (tr(r(k1), d+1) == h-1);
			end
        end
        
        % Now, we find the most observed output value among the nearest 
        % neighbors for each observation.

		m1 = max(nn(:,j));
        
        % Tie-breaking rule: We may have more than one output value that
        % has the same maximum number of occurences. To account for this
        % case, we randomize between those output values.
        
        pr = find(nn(:,j) == m1);

		pred(j) = classes(pr(ceil(rand()*length(pr))));
    end
end
