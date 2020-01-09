function [h, m, Q] = EM(flag, image_bmp, k)

[img cmap] = imread(image_bmp);
img_rgb = ind2rgb(img, cmap);
img_double = im2double(img_rgb);

nrows = size(img_double, 1);
ncols = size(img_double, 2);

X = reshape(img_double,nrows*ncols,3);

N = size(X, 1);
D = size(X, 2);



%% Start EM algorithm

[idx, mu] = kmeans(X, k, 'MaxIter', 3);

b = zeros(N,k);

for t = 1:N
	for i = 1:k
		if idx(t) == i
			b(t, i) = 1;
		end
	end
end


Qs = [];

if(flag == 1)
	lambda = 1;
	p = zeros(k, 1);
	S = zeros(D, D, k);
	h = zeros(N, k);

	% Initialize parameters with k-means

	for i = 1:k
	    p(i) = sum(b(:,i))/N;
	end


	for i = 1:k
	    temp = lambda * eye(D, D);
	    m_i = mu(i,:)';
	    for t = 1:N
	        x_t = X(t,:)';
	        temp = temp + b(t,i)* ((x_t-m_i)*(x_t-m_i)');
        end
	    S(:,:,i) = temp ./ sum(b(:,i));
	end


	ll = 100;
	ll_new = 0;
	it = 0;

	while(abs(ll_new - ll)/ll > 0.001 && it < 100)
	    
		ll = ll_new;

	%% E-step

	    for i = 1:k
	        p_i = p(i);
	        m_i = mu(i,:)';
	        S_i = S(:,:,i);
	        for t = 1:N
	            x_t = X(t, :)';
	            temp = 0;
	            for j = 1:k
	                p_j = p(j);
	                m_j = mu(j,:)';
	                S_j = S(:,:,j);
	                temp = temp + p_j * mvnpdf(x_t, m_j, S_j);
	            end
	            h(t,i) = p_i * mvnpdf(x_t, m_i, S_i) / temp;
	        end
	    end

	% Calculate the expected complete likelihood after E-step.
	    temp = 0;
	    for t = 1:N
	        x_t = X(t,:)';
	        for i = 1:k
	            p_i = p(i);
	            m_i = mu(i,:)';
	            S_i = S(:,:,i);
	            if(h(t,i)>1e-10)
	            	temp = temp + h(t,i) * (log(p_i) + log(mvnpdf(x_t, m_i, S_i)));
	            end
	        end
	    end
	    Q_e = temp;
	    
	    Qs = [Qs;Q_e];
	    


	%% M-step

	    for i = 1:k
	        p(i) = sum(h(:,i))/N;
	    end

	    for i = 1:k
	    	mu(i, :) = (h(:,i)' * X)./sum(h(:,i));
	    end

	    for i = 1:k
	        temp = lambda * eye(D, D);
	        m_i = mu(i,:)';
	        for t = 1:N
	            x_t = X(t,:)';
	            temp = temp + h(t,i)* ((x_t-m_i)*(x_t-m_i)');
	        end
	        S(:,:,i) = temp ./ sum(h(:,i));
	    end



	% Calculate the expected complete likelihood after M-step.

	    temp = 0;
	    for t = 1:N
	        x_t = X(t,:)';
	        for i = 1:k
	            p_i = p(i);
	            m_i = mu(i,:)';
	            S_i = S(:,:,i);
	            if(h(t,i)>1e-10)
	            	temp = temp + h(t,i) * (log(p_i) + log(mvnpdf(x_t, m_i, S_i)));
	            end
	        end
	    end

	    Q_m = temp;
	    Qs = [Qs; Q_m];



	% Calculate the likelihood as in Bishop chapter (to check for convergence).

	    temp = 0;
	    for t = 1:N
	        temp2 = 0;
	        x_t = X(t,:)';
	        for i = 1:k
	            p_i = p(i);
	            m_i = mu(i,:)';
	            S_i = S(:,:,i);
	            temp2 = temp2 + p_i * mvnpdf(x_t, m_i, S_i);
	        end
	        temp = temp + log(temp2);
	    end
	    
	    ll_new = temp;

	    it = it + 1;

	end


end

if flag == 0
	p = zeros(k, 1);
	S = zeros(D, D, k);
	h = zeros(N, k);

	% Initialize parameters with k-means

	for i = 1:k
	    p(i) = sum(b(:,i))/N;
	end


	for i = 1:k
	    temp = zeros(D, D);
	    m_i = mu(i,:)';
	    for t = 1:N
	        x_t = X(t,:)';
	        temp = temp + b(t,i)* ((x_t-m_i)*(x_t-m_i)');
	    end
	    S(:,:,i) = temp ./ sum(b(:,i));
	end


	% Check if S_i is positive definite.

	for i = 1:k
	    S_i = S(:,:,i);
	    [~, ch] = chol(S_i);
	    if(ch~=0)		
	        fprintf('The variance-covariance matrix is singular. Start the algorithm again.')
	        return
	    end
	end




	ll = 100;
	ll_new = 0;
	it = 0;

	while(abs(ll_new - ll)/ll > 0.0005 && it < 100)
	    
	    ll = ll_new;

	%% E-step

	    for i = 1:k
	        p_i = p(i);
	        m_i = mu(i,:)';
	        S_i = S(:,:,i);
	        for t = 1:N
	            x_t = X(t, :)';
	            temp = 0;
	            for j = 1:k
	                p_j = p(j);
	                m_j = mu(j,:)';
	                S_j = S(:,:,j);
	                temp = temp + p_j * mvnpdf(x_t, m_j, S_j);
	            end
	            h(t,i) = p_i * mvnpdf(x_t, m_i, S_i) / temp;
	        end
	    end

	% Calculate the expected complete likelihood after E-step.
	    temp = 0;
	    for t = 1:N
	        x_t = X(t,:)';
	        for i = 1:k
	            p_i = p(i);
	            m_i = mu(i,:)';
	            S_i = S(:,:,i);
	            if(h(t,i)>1e-10)
	            	temp = temp + h(t,i) * (log(p_i) + log(mvnpdf(x_t, m_i, S_i)));
	            end
	        end
	    end
	    Q_e = temp;
	    
	    Qs = [Qs;Q_e];
	    


	%% M-step

	    for i = 1:k
	        p(i) = sum(h(:,i))/N;
	    end

	    for i = 1:k
	    	mu(i, :) = (h(:,i)' * X)./sum(h(:,i));
	    end


	    for i = 1:k
	        temp = zeros(D, D);
	        m_i = mu(i,:)';
	        for t = 1:N
	            x_t = X(t,:)';
	            temp = temp + h(t,i)* ((x_t-m_i)*(x_t-m_i)');
	        end
	        S(:,:,i) = temp ./ sum(h(:,i));
	    end

	    % Check if S_i is positive definite.

	    for i = 1:k
	        S_i = S(:,:,i);
	        [~, ch] = chol(S_i);
	        if(ch~=0)		
	            fprintf('The variance-covariance matrix is singular. Start the algorithm again.')
	            return
	        end
	    end


	% Calculate the expected complete likelihood after M-step.

	    temp = 0;
	    for t = 1:N
	        x_t = X(t,:)';
	        for i = 1:k
	            p_i = p(i);
	            m_i = mu(i,:)';
	            S_i = S(:,:,i);
	            if(h(t,i)>1e-10)
	            	temp = temp + h(t,i) * (log(p_i) + log(mvnpdf(x_t, m_i, S_i)));
	            end
	        end
	    end

	    Q_m = temp;

	    Qs = [Qs; Q_m];



	% Calculate the likelihood as in Bishop chapter (to check for convergence).

	    temp = 0;
	    for t = 1:N
	        temp2 = 0;
	        x_t = X(t,:)';
	        for i = 1:k
	            p_i = p(i);
	            m_i = mu(i,:)';
	            S_i = S(:,:,i);
	            temp2 = temp2 + p_i * mvnpdf(x_t, m_i, S_i);
	        end
	        temp = temp + log(temp2);
	    end
	    
	    ll_new = temp;

	    it = it + 1;

	end
end





Q = Qs;
m = mu;

figure
plot(1:it, Qs(1:2:end)) %first points in blue
hold on
plot(1:it, Qs(2:2:end), 'g') %green ones
hold off



X2 = zeros(N,D);
[~, ind] = max(h,[],2);
for t = 1:N
    for i = 1:k
        if(ind(t) == i)
            X2(t,:) = mu(i,:);
        end
    end     
end

new_X = reshape(X2, [nrows, ncols, 3]);

figure        
image(new_X)
filename = strcat('fig_k', num2str(k), '.png');
saveas(gcf, filename)

end
