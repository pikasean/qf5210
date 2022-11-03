manual_forecast <- function(m, n, s) {
  ar_order = m@fit$series$order[1]
  ma_order = m@fit$series$order[2]
  res = residuals(m, standardize = F) # epsilons
  res_std = residuals(m, standardize = T) # z_t's
  #res_for_se = residuals(m, standardize = F) # epsilons
  res_h = c(tail(res, n = 1)/tail(res_std, n = 1))^2
  forecast_mean = rep(0,n)
  forecast_se = rep(0,n)
  
  mu = m@fit$params$params[1]
  ar_coefs = m@fit$params$params[2:(1+ar_order)] # ar1, ar2, ...
  ma_coefs = m@fit$params$params[(2+ar_order):(1+ar_order+ma_order)] # ma1, ma2, ...
  omega = m@fit$params$params[2+ar_order+ma_order]
  alpha1 = m@fit$params$params[3+ar_order+ma_order]
  beta1 = m@fit$params$params[5+ar_order+ma_order]
  shape = m@fit$params$params[8+ar_order+ma_order]
  for (i in 1:n) {

    ar_part = ar_coefs %*% rev(tail(s, n = ar_order)) # get the previous series values for AR
    ma_part = ma_coefs %*% rev(tail(res, n = ma_order))
    forecast_mean[i] = mu + ar_part + ma_part
    
    #print(omega)
    #print(alpha1)
    #print(beta1)
    #print(tail(res_for_se, n = 1))
    #print(tail(res_std, n = 1))
    
    ht = omega + alpha1*(tail(res, n = 1)^2) + beta1*(tail(res_h, n = 1))
    se = sqrt(ht)*qt(0.975,shape)
    forecast_se[i] = se
    
    s <- c(s, mu + ar_part + ma_part)
    res <- c(res, 0)
    #res_for_se <- c(res_for_se, se)
    res_h <- c(res_h, ht)
    res_std <- c(res_std, qt(0.975,shape))
  }
  
  return(list(forecast_mean, forecast_se))
}