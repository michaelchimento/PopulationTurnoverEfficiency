## Installation ##
Installation can take several minutes if Python is already installed.
The agent based model was written and tested on a Linux desktop computer (Pop!_OS 20.04 LTS) for Python 3.8. Please visit https://www.python.org/downloads/ for instructions on how to install Python on your system.
Install required python3 dependencies by running the following command from the terminal:
```
pip3 install numpy networkx
```

## Running the model ##
Navigate to the containing directory in terminal, and run the model with the following command
```
python3 efficiency_abm.py ["negative","positive"] [True, False]
```
The 1st argument "negative" or "positive" controls the probabilities with which agents will socially learn the task. Running with the argument "negative" will run sims with original conditional probability of learning function inferred from experimental data. Running with the argument "positive" reverses these probabilities, and was used to test how important this function was to the results. Please see the manuscript for more details. The 2nd argument, True or False, indicates whether there is population turnover in the simulations (True), or if populations are static (False). 

## Expected output ##
The simulation will start running, and will give feedback on where you are in parameter space. This can take hours to finish, depending on your system. Data will be written to csv files in the containing directory. This contains the following columns: 

Column name  | Description
------------- | -------------
sim  | identifying number for simulation within parameter space
timestep  | timestep number
condition | whether turnover or static, as well as the conditional probability of learning function
pop_size | population size
g_i | parameter value for current payoff bias in EWA model
s_i | parameter value for social information bias in EWA model
inverse_temp | parameter value for conservatism in EWA model
conformity | parameter value for conformity exponent in EWA model
count_inefficient | number of productions of inefficient behaviors in that timestep
count_efficient | number of productions of efficient behaviors in that timestep
num_solvers | number of knowledgable agents in that timestep
