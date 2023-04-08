c_dob = "2020-08-01"
age = 7

c_interview = ymd(c_dob) + days(round(age*30.5))
ymd(c_dob) + days(6*30.5)

c_gestation = ymd(c_dob) - days(38*7)

ymd("2020-09-24") - ymd(c_dob)
