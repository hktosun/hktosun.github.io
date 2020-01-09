function [alpha, b, error_rate] = kernPerc(train_data, train_label, p)

X = train_data;
y = train_label;
N = size(X, 1);
alpha = zeros(N, 1);
b = 0;
iter = 0;
converge = false;

while(converge == false)
    iter = iter + 1;
    wrong = 0;
	for t = 1:N
		temp = 0;
		x_t = X(t,:)';
		for i = 1:N
			K(i) = (X(i,:) * x_t + 1)^p;
			temp = temp + alpha(i)*y(i)*K(i) ;
		end
		if ((temp +b) * y(t) <= 0)
			wrong = wrong + 1;
			alpha(t) = alpha(t) + 1;
			b = b + y(t);
		end
	end
	if(wrong / N < 0.005)
		converge = true;
	end
	
	if(iter > 500)
		converge = true;
	end    
end

error_rate = wrong/N;


end