map_page_notes <- function(month_name,life_exp_thresh){
  paste0(
  "

This app attempts to display probable <em>current</em> cases per million in each country.

First, take a look at the number of active cases, based on the reported active cases over the last 7 days (Figure 1).
<br />
<h4>Method</h4>

Data is limited but around the world, 0.5% has been estimated as a plausible infection to fatality rate for COVID-19 in normal populations 
<a href='https://science.sciencemag.org/content/early/2020/04/24/science.abb3221'>(Li et al., 2020)</a>.

<br /><br />
1. We can estimate the true infections \\(i\\)  three weeks prior as the number of fatalities \\(f\\) this week, multiplied by 200, if we assume an 0.5% CFR.
<br /><br />
$$i_{estimated, three weeks prior} = \\frac{f_{observed, this week}}{0.005} $$

<br /><br />
2. We can <i>then</i> compare the estimated true infections \\(i\\) three weeks prior with the observed \\(i\\) cases at the same time, and calculate a hit rate \\(h\\) 
(proportion of infections detected as confirmed cases)  (Figure 2). We then assume that the hit rate is constant.
<br /><br />
$$h = \\frac{i_{observed, three weeks prior}}{i_{estimated, three weeks prior}} $$

<br /><br />
3. Then we can estimate the true infections \\(i\\) today (Figure 3) based on detected cases today, and the estimated hit rate \\(h\\) .
$$i_{estimated, today} = \\frac{i_{observed, today}}{h}$$
<br /><br />
4. We can then divide that estimated number of true infections by population size \\(p\\) to get a per capita figure (Figure 4).
$$r = \\frac{i_{estimated, today}}{p} $$
<br /><br />

5. We can get the arrivals from last year based on country of usual residence (Figure 5). 
This isn't perfect data because it records country of usual residence rather than the country each person was last in. 
However, it is actually helpful to use this data because it excludes arrivals by New Zealand residents, who are already allowed back to NZ.

<br /><br />

6. We can then calculate the probability that we'll get one or more cases \\(P(c>0)\\) 
arriving from each source country in a month (Figure 6), 
using population size \\(p\\), 
the estimated number of active cases in that source country \\(i\\), 
and the number of residents of that country who arrived in NZ in "
  ,month_name,
  " 2019, \\(a\\) . If we assume cases are independent, we can use the following formula:

$$P(c>0) = 1-(\\frac{p-i_{estimated, today}}{p})^a$$

<br /><br />

7. Finally we can calculate the <em>expected number</em> of cases from each country (Figure 7). 
This is simply the estimated infection rate multiplied by the number of arrivals.

<br /><br />

8. If we can assume that a certain rate of quarantined arrivals will get out into the community before they are confirmed negative,
we can calculate the probability that at least arrival, if quarantined, will get out into the community. 

A very cautious assumption is that 1 in 50 people will break quarantine, and 1 in 100 people will test negative and be released but actually be positive.

With those assumptions we can calculate the probability of getting an arrival who goes through quarantine and testing, but reaches the community, as (Figure 8) and the expected number of such people (Figure 9).

<br /><br />

An alternative method would make an adjustment based on the estimated current rate of growth. Although it's problematic, we assume that the rate of growth in confirmed cases
reflects the rate of growth in actual infections. This method is not used here.

<br /><br />

In countries where health systems are underdeveloped, it is unlikely that COVID-19 deaths will be accurately recorded. 

Because countries where health systems are underdeveloped may not accurately record deaths, 
the visualization only displays countries with a life expectancy of "
  ,as.character(life_exp_thresh),
  " or higher."
  )
}