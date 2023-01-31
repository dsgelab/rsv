
## This is the logistic regression model equation for the clinical prediction model for probability of RSV hospitalisation in infants younger than 1 year.


# To use the equation, use respective variable values in the place of names
# This equation gives the log odds of the predicted probability.
# To transform the log odds into a probability, for example the following function can be used:

# log_odds_to_prob <- function(log_odds) { exp(log_odds) / (1 + exp(log_odds)) }




### INTERCEPT
(-0.45411329 + 
               

### CONTINUOUS VARIABLES
## The variables are represented as splines in the model.

## The 'pmax' function chooses the larger of the two values. 
## Here, the interpretation is that if the knot value subtracted from the variable value is less than 0, 0 should be used in this term.
## The exponentiated term is always 0 or greater


# Months from birth to next estimated epidemic peak ("dist")
(0.15770323*dist) + 
(-0.0086047306*pmax(dist, 0)^3) +
(0.029222391*pmax(dist-4, 0)^3) +
(-0.027476176*pmax(dist-7, 0)^3) + 
(0.0068585149*pmax(dist-11, 0)^3) +


# Gestational age at birth ("gest_days")
(-0.0085261165*gest_days) +
(-2.6026789e-06*pmax(gest_days-203, 0)^3) + 
(3.6156146e-06*pmax(gest_days-238, 0)^3) + 
(1.4786345e-05*pmax(gest_days-257, 0)^3) +
(-5.6586779e-05*pmax(gest_days-276, 0)^3) + 
(4.6504911e-05*pmax(gest_days-283, 0)^3) +
(-5.7174129e-06*pmax(gest_days-293, 0)^3) +


# birth weight in SD units ("weight_sd")
(-0.11353702*weight_sd) + 
(0.022105926*pmax(weight_sd+1.7785123, 0)^3) +
(-0.060928467*pmax(weight_sd+0.54131916, 0)^3) +
(0.040663683*pmax(weight_sd-0.22936323, 0)^3) +
(-0.0018411422*pmax(weight_sd-1.6255334, 0)^3) +

# Mother's age in years ("mother_age")
(-0.048627475*mother_age) +
(0.00028458*pmax(mother_age-21.467488, 0)^3) +
(-0.0009113168*pmax(mother_age-28.172485, 0)^3) + 
(0.00073102537*pmax(mother_age-32.366872, 0)^3) +
(-0.00010428857*pmax(mother_age-39.277207, 0)^3) + 


### BINARY VARIABLES

# Twin siblings ("twin")
0.26196394*twin + 

# Male gender ("male_gender")
0.21186408*male_gender + 

# Older siblings aged less than 4 years ("sib_0_4")
0.87154293*sib_0_4 + 

# Older siblings aged 4-7 years ("sib_4_7")
0.3217131*sib_4_7 + 

# Down's syndrome ("down")
0.84563921*down + 

# Sibling hospitalised for viral bronchiolitis or wheezing at age 0-4 years ("sib_resp_hosp")
0.62547002*sib_resp_hosp + 

# Maternal smoking during pregnancy ("smoking")
0.20650808*smoking + 

# Neonatal respiratory conditions in a term child (term_breathing)
0.33918452*term_breathing + 

# Esophagus malformations ("q39")
1.1382028*q39 + 

# Asthma in any first-degree relative ("any_family_asthma")
0.39925972*any_family_asthma + 

# Congenital heart defect requiring operation during the first year of life ("any_severe_chd")
1.0538093*any_severe_chd + 

# Lower complexity atrial or\nventricular septal defect ("asd_or_vsd_only")
0.34578448*asd_or_vsd_only)




