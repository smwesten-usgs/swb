# Model Description {#model_desc}

## Model Theory

The SWB code uses a modified Thornthwaite-Mather soil-moisture accounting method \citep{thornthwaite_instructions_1957} to calculate recharge; recharge is calculated separately for each grid cell in the model domain. Sources and sinks of water within each grid cell are determined on the basis of input climate data and landscape characteristics; recharge is calculated as the difference between the change in soil moisture and these sources and sinks (eq.~\ref{eq:swb_mass_bal}):


\f[
   recharge = \underbrace{(rainfall + snowmelt + inflow)}_\text{sources} - \underbrace{(interception + outflow + ET)}_\text{sinks} - \underbrace{\Delta soil moisture}_\text{change in storage}  
  \label{eq:swb_mass_bal}
\f]

Each of the water-budget components given in equation ~\ref{eq:swb_mass_bal} is handled by one or more modules within the SWB model. Specific water-balance components are discussed briefly below.

**rainfall.** Precipitation data are input as daily values either as a time series at a single gage or as a series of daily Arc ASCII or Surfer grid files created by the user. Precipitation-gage records from an unlimited number of sites may be used if the user supplies precipitation as a series of grid files. SWB converts partitions precipitation into rainfall and snowfall components on the basis of daily minimum and maximum air temperatures.

**snowmelt.** Snow is allowed to accumulate and\slash or melt on a daily basis. The daily mean, maximum, and minimum air temperatures are used to determine whether precipitation takes the form of rain or snow. Precipitation that falls on a day when the mean temperature minus one-third the difference between the daily high and low temperatures is less than or equal to the freezing point of water is considered to fall as snow (Dripps and Bradbury, 2005).

Snowmelt is based on a temperature-index method. In the SWB code it is assumed that 1.5 mm (0.059 in.) of water-equivalent snow melts per day per average degree Celsius that the daily maximum temperature is above the freezing point (Dripps and Bradbury, 2005).

**inflow.** Inflow is calculated by use of a flow-direction grid derived from a digital elevation model to route outflow (surface runoff) to adjacent downslope grid cells. Inflow is considered to be zero if flow routing is turned off.

**interception.** Interception is treated simply by means of a \quotes{bucket} model approach---a user-specified amount of rainfall is assumed to be trapped and used by vegetation and evaporated or transpired from plant surfaces. Daily precipitation values must exceed the specified interception amount before any water is assumed to reach the soil surface. Interception values may be specified for each land-use type and season (growing and dormant).

**outflow (or surface runoff).** from a cell is calculated by use of the U.S. Department of Agriculture, Natural Resources Conservation Service (NRCS) curve number rainfall-runoff relation (Cronshey and others, 1986). This rainfall-runoff relation is based on four basin properties: soil type, land use, surface condition, and antecedent runoff condition.
The curve number method defines runoff in relation to the difference between precipitation and an \quotes{initial abstraction} term. Conceptually, this initial abstraction term represents the summation of all processes that might act to reduce runoff, including interception by plants and fallen leaves, depression storage, and infiltration (Woodward and others, 2003). Equation 2 is used to calculate runoff volumes (Woodward and others, 2002):

\f[
R =  \dfrac{(P - I_a)^2}{(P + [S_{max} - I_a])} %\mbox{,}
\label{eq:runoff_volumes_scs_cn}
\f]

where $R$ is runoff, $P$ is daily precipitation, $S_{max}$ is the maximum soil-moisture holding capacity, and $I_a$ is initial abstraction, the amount of precipitation that must fall before any runoff is generated.

The initial abstraction ($I_a$) term is related to a maximum storage term ($S_{max}$) as follows:

\f[
I_a = 0.2 S_{max}
\label{eq:initial_abstraction_scs_cn}
\f]


The maximum storage term is defined by the curve number for the land-cover type under consideration:

\f[
S_{max} = \left( \dfrac{1000}{CN}\right) - 10
\label{eq:s_max_scs_cn}
\f]


Curve numbers are adjusted upward or downward depending on how much precipitation has occurred in the previous 5-day period. The amount of precipitation that has fallen in the previous 5-day period is used to describe soil-moisture conditions; three classes of moisture conditions are defined and are called antecedent runoff condition I, II, and III, defined as shown in table~\ref{tab: antecedent_runoff_cond}.

Runoff condition number | Description | Nongrowing Season | Growing Season
----------|-----------------------|------------|--------------------
    I     |    Dry                | 0.05       | 1.4     
    II    |    Average            | 0.5 -- 1.1 | 1.4 -- 2.1 
    III   |    Near Saturation    | 1.1        | 2.1    
