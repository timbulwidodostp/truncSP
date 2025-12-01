# Olah Data Semarang
# WhatsApp : +6285227746673
# IG : @olahdatasemarang_
# Estimators of semi-parametric truncated regression models Use qme and lt and stls (truncSP) With (In) R Software
install.packages("truncSP")
library("truncSP")
truncSP = read.csv("https://raw.githubusercontent.com/timbulwidodostp/truncSP/main/truncSP/truncSP.csv",sep = ";")
# Estimation Estimators of semi-parametric truncated regression models Use qme and lt and stls (truncSP) With (In) R Software
x1 <- truncSP$X1_IPM
x2 <- truncSP$X2_IDG
x3 <- truncSP$X3_LPE
y <- truncSP$Y_IDI
d <- data.frame(y = y,x1 = x1, x2 = x2, x3 = x3) 
##Use a truncated subsample
dtrunc <- subset(d, y>0)
# Use qme or lt to consistently estimate the slope parameters
qme <- qme(y ~ x1 + x2 + x3, dtrunc, point = 0, direction = "left", cval = "ols", const = 1, beta = "ols", covar = FALSE)
lt <- lt(y ~ x1 + x2 + x3, dtrunc, point = 0, direction = "left", clower = "ols", const = 1, cupper = 2, beta = "ols", covar = FALSE)
# Simulate a data.frame (symmetrically distributed errors)
x1 <- truncSP$X1_IPM
x2 <- truncSP$X2_IDG
x3 <- truncSP$X3_LPE
y <- truncSP$Y_IDI
d1 <- data.frame(y = y,x1 = x1, x2 = x2, x3 = x3)
# Use a truncated subsample
dtrunc <- subset(d1, y>0)
# Use stls to estimate the model
stls <- stls(y ~ x1 + x2 + x3, dtrunc, point = 0, direction = "left", beta = "ols", covar = FALSE)
qme
lt
stls
summary(qme)
summary(lt)
summary(stls)
# Estimators of semi-parametric truncated regression models Use qme and lt and stls (truncSP) With (In) R Software
# Olah Data Semarang
# WhatsApp : +6285227746673
# IG : @olahdatasemarang_
# Finished