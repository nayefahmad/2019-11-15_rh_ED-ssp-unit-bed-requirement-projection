# Richmond Hospital: Using queuing theory to determine bed requirements

![queue-model](https://raw.githubusercontent.com/nayefahmad/Richmond-Hospital-SSP-unit-bed-requirement-projection/master/src/2019-11-15_rh_ED-ssp-unit-bed-requirement-projection_files/figure-html/unnamed-chunk-14-1.png)

## Key takeaways
1. Queue performance deteriorates in a higly nonlinear way as average occupancy increases.

2. Simple rules of thumb about target occupancy levels can be dangerously misleading, especially in small systems (e.g. units with small number of beds). In this example, note that avg occupancy in current state is “only” about 22%, so one might think there is no harm in reducing the number of beds from 4 to 3. However, this will lead to an almost 500% increase in wait time to get into a bed. Is it worth it? Maybe, but the decision-maker must know about the tradeoff involved.

3. There are two alternative decision paradigms: “Quality-focused” and “Cost-focused”. Quality-focused: choose number of beds so that average occupancy is the same or lower than current value. Cost-focused: choose number of beds so that average wait time to get into a bed is the same or lower than current value.

## Bed requirements 

Rule i) suggests that 6 inpatient SSP beds will be required by 2030.

Rule ii) suggests that 5 inpatient SSP beds will be required by 2030.