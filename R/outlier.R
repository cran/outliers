"outlier" <-
function (x, opposite = FALSE, logical = FALSE) 
{
    if (is.matrix(x)) 
        apply(x, 2, outlier, opposite = opposite, logical = logical)
    else if (is.data.frame(x)) 
        sapply(x, outlier, opposite = opposite, logical = logical)
    else {

	if (xor(((x[length(x)] - mean(x)) < (mean(x) - x[1])),opposite)) 
		{
			if (!logical) min(x)
			else x == min(x)
		}
		else 
		{
			if (!logical) max(x)
			else x == max(x)
		}
	} 
}

