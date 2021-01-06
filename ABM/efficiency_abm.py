#!/usr/bin/env python
# coding: utf-8

#run like so: python3 efficiency_abm.py ["negative","positive"] [True,False]
#running with arg negative will run sims with original conditional probability of learning function inferred from experimental data
#running with arg positive reverses this, used to test how important it was to the results

import numpy as np
import networkx as nx
import random
import warnings
from sys import argv
import scipy.stats as stats
warnings.filterwarnings('ignore')

solutions = 2
t_steps = 35 #timesteps to run simulation
graph_type = "complete" #sets network structure
#median solves per day was like 98
solves_per_day = 100
turnover = eval(argv[2])
num_turnover = 2
N = 6

#payoffs for inefficient, efficient behavior
payoffs = [10,20]

#provide populations with a tutor that has init_obs observations of the inefficient behavior
init_tutor = True
init_obs = 100

learning_function = str(argv[1]) if len(argv) > 1 else "negative"
condition="turnover{}_learn_func{}_payoffs{}-{}".format(turnover,learning_function,payoffs[0],payoffs[1])

master_sim_no = 0
#conformity_values = [1,5,10] #conformity exponent
conformity_values = [5] #conformity exponent
parameter_values = np.around(np.arange(0.1, 1.1, 0.2), 1)
inverse_temp_values = np.around(np.arange(1,6,1), 1)
num_replicates = 500

def trunc_dist(mean, sigma, lower, upper):
    value = stats.truncnorm.rvs(
            (lower - mean) / sigma, (upper - mean) / sigma,
            loc=mean,
            scale=sigma, size=1)[0]
    return value

#read learning hazard data
learning_hazard = [0.0607558419663852,
0.0558706609510212,
0.0512184413178045,
0.0473066111308692,
0.0440346627775601,
0.0412669810788324,
0.0388943876121157,
0.0368349431418403,
0.0350274742024094,
0.0334258046650327,
0.0319945442791914,
0.0307061406767733,
0.0295388219519917,
0.0284751466177429,
0.0275009670541005,
0.0266046771089386,
0.0257766576268061,
0.0250088619027292,
0.0242945015343434,
0.0236278053499515,
0.0230038322514222,
0.0224183243440254,
0.0218675905295166,
0.021348413390461,
0.0208579740667709,
0.0203937911650874,
0.0199536707114165,
0.0195356648673001,
0.0191380376550309,
0.0187592363299087,
0.0183978673335524,
0.0180526759876139,
0.0177225292601104,
0.0174064010703219,
0.0171033597024023,
0.0168125569796138
]

if learning_function=="positive":
    learning_hazard = learning_hazard[::-1]

masterID = 0
class agent:

    def __init__(self):
        global masterID
        '''
        social memory records frequencies of observations, which I've equated to the frequency
        of choice k among social cues at time t (or the  n_kt term in the S_kit equation). Does
        this need to have a memory constraint attached to it, like a sliding window?
        '''
        self.social_memory = np.zeros(shape=solutions, dtype=(float, 1))

        '''
        Attraction_matrix is composed of the individual attraction scores for choice k
        before being passed to the soft-max choice rule implemented in I_mat.
        current A_kit = (1-g_i)* previous(A_kit) + g_i* pi_k
        g_i is the importance of newly experienced payoff from choice k, pi_k
        '''
        self.A_mat = np.zeros(solutions, dtype=(float, 1))

        '''
        Individual_attraction_matrix is the result of raising e to the values from A_matrix
        and dividing by the sum of all other exponentiated entries in A_matrix.
        '''
        self.I_mat = np.zeros(shape=solutions, dtype=(float, 1))

        '''
        Social_matrix is the frequency of choice k among all social cues at time t, raised to
        the conformist exponent lambda (1= is neutral, >1 is conformist, <1 is anti-conformist)
        These probabilities will influence choice in the probability_matrix

        S_kit = n_kt^lambda / sum(n_kt^lambda)
        '''
        self.S_mat = np.zeros(shape=solutions, dtype=(float, 1))

        '''
        Probability_matrix contains the probability of observing choice k at time t by individual i
        by taking into account individual experience and social information. It's initially set to
        equal probabilities for all choices. Is this right?

        p_kit= (1-s_i)I_kit + s_i*S_kit
        s_i is social influence parameter, where higher values discount individual information and
        prioritize social information.
        '''
        self.P_mat = np.zeros(shape=solutions, dtype=(float, 1))

        self.id = masterID
        masterID += 1

        self.s_i = s_i
        self.g_i = g_i
        self.conformity = conformity
        self.inverse_temp = inverse_temp

        self.learn_prob = learning_hazard[0]
        self.days_exposure = 0
        self.generation = 1
        self.solve_count = [0,0] #keep track of personal solves


        #print("s_i {}; g_i {}, lambda {}".format(self.s_i, self.g_i, self.conformity))
        self.naive = True
        self.I_mat_update()
        self.P_mat_update()

    def reset_solve_count(self):
        self.solve_count = [0,0]

    def A_kit_update(self, solution):
        #print("Agent{} A_kit_update(). old A_mat: {}".format(self.id, self.A_mat))
        payoff = payoffs[solution]
        #payoff = trunc_dist(payoffs[solution], 4, 0, 30)
        #print("payoff {}".format(payoff))
        new_A_kit = (1 - self.g_i) * self.A_mat[solution] + self.g_i * payoff
        #print("new attraction score {}".format(new_A_kit))
        self.A_mat[solution] = new_A_kit
        #print("Agent{} A_kit_update(). new A_mat: {}".format(self.id, self.A_mat))


    def I_mat_update(self):
        #print("Agent{} I_mat_update(). old I_mat: {}".format(self.id, self.I_mat))
        exp_A_mat = np.exp(np.multiply(self.A_mat, self.inverse_temp))
        for solution in range(solutions):
            self.I_mat[solution] = exp_A_mat[solution] / np.sum(exp_A_mat)
        #print("Agent{} I_mat_update(). new I_mat: {}".format(self.id, self.I_mat))

    def S_mat_update(self):
        #print("Agent{} S_mat_update(). old S_mat: {}".format(self.id, self.S_mat))
        if self.social_memory.any():
            #print("Agent{} S_mat_update(). has social memory {}.".format(self.id,self.social_memory))
            for solution in range(solutions):
                new_S_kit = (self.social_memory[solution]**self.conformity
                            / np.sum(self.social_memory**self.conformity))
                self.S_mat[solution] = new_S_kit
        else:
            #print("Agent{} S_mat_update(). has no social memories.".format(self.id))
            for solution in range(solutions):
                new_S_kit = 0
                self.S_mat[solution] = new_S_kit
        #print("Agent{} S_mat_update(). new S_mat: {}".format(self.id, self.S_mat))


    def P_mat_update(self):
        #print("Agent{} P_mat_update(). Old P_mat{}".format(self.id, self.P_mat))
        if self.social_memory.any():
            for solution in range(solutions):
                self.P_mat[solution] = (1 - self.s_i)*self.I_mat[solution] + self.s_i*self.S_mat[solution]

        else:
            self.P_mat[0:solutions] = self.I_mat

        #print("Agent{} P_mat_update(). New P_mat{}".format(self.id, self.P_mat))

    def produceBehavior(self):
        #print("Agent{} produceBehavior(). P_mat is {}".format(self.id, self.P_mat))
        production = np.random.choice(np.arange(0, solutions), p=self.P_mat)
        #print("Agent{} produced {}".format(self.id, production))

        self.solve_count[production] += 1
        self.A_kit_update(production)
        self.I_mat_update()
        self.P_mat_update()

        return production

def generateNetwork(graph_type):
    if graph_type == "complete":
        G = nx.complete_graph(N)

    for i in range(N):
        G.add_node(i, data=agent())

    if init_tutor == True:
        G.nodes[0]["data"].naive=False
        #print("***start training***")
        G.nodes[0]["data"].social_memory[0] = 10
        G.nodes[0]["data"].S_mat_update()
        for i in range(100):
            G.nodes[0]["data"].A_kit_update(0)
            G.nodes[0]["data"].I_mat_update()
            G.nodes[0]["data"].P_mat_update()
        #print("***end training***")
    return G

def update_learn_prob(G):
    for agent in range(N):
        G.nodes[agent]["data"].days_exposure += 1
        if G.nodes[agent]["data"].naive == True:
            G.nodes[agent]["data"].learn_prob =  learning_hazard[G.nodes[agent]["data"].days_exposure]

def choose_solver(G, knowledgable):
    choice = np.random.choice(knowledgable)
    return choice

def game(G, solver):
    #print("\n***new game***\nsolver is agent {:d} ".format(solver))
    solution = G.nodes[solver]["data"].produceBehavior()
    return solution

def state_switch(G, knowledgable):
    naive_agents = [agent for agent in range(N) if G.nodes[agent]["data"].naive == True ]
    for agent in naive_agents:
        chance = random.random()
        if chance <= G.nodes[agent]["data"].learn_prob:
            #print("new agent has learned")
            G.nodes[agent]["data"].naive = False
            knowledgable.append(agent)

    return knowledgable

def obs_learning(G, frequencies):
    for agent in range(N):
        #print("obs_learning() agent {}".format(G.nodes[agent]["data"].id) )
        for variant in range(solutions):
            #subtraction here excludes their own solves from their social memory
            G.nodes[agent]["data"].social_memory[variant] = frequencies[variant] - G.nodes[agent]["data"].solve_count[variant]
        G.nodes[agent]["data"].S_mat_update()
        G.nodes[agent]["data"].P_mat_update()

def turnover_event(G,timestep):
    if timestep+1 <= 21:
        generation_for_turnover = 1
    else:
        generation_for_turnover = 2

    #checks to see if any agents are from the previous generation are due to be replaced
    exposure_list = [i for i in range(N) if G.nodes[i]["data"].generation == generation_for_turnover]
    turnover_list = np.random.choice(exposure_list,replace = False, size = 1*num_turnover)
    for n in turnover_list:
        G.nodes[n]["data"] = agent()
        G.nodes[n]["data"].generation = generation_for_turnover+1
    #print("turnover_event(). turnover event: {} replaced".format(turnover_list))


def extract_data(G,solution_list,knowledgable):
    frequencies = [solution_list.count(n) for n in range(solutions)]
    naives = [agent for agent in range(N) if G.nodes[agent]["data"].naive==True]

    count_inefficient = frequencies[0]
    count_efficient = frequencies[1]

    num_solvers = len(knowledgable)

    return count_inefficient, count_efficient, num_solvers

def create_csv():
    #writes header for main data
    fout=open("data_"+str(condition)+".csv","w")
    fout.write("sim\t" + "timestep\t"+ "condition\t"+
          "pop_size\t"+ "g_i\t"+ "s_i\t"+ "inverse_temp\t" + "conformity\t" +
          "count_inefficient\t"+ "count_efficient\t"+ "num_solvers\t"+"payoffs")
    fout.write("\n")
    fout.close()

def write_csv(sim_num, timestep, condition,
              pop_size, g_i, s_i,
              count_inefficient, count_efficient, num_solvers):

    fout = open("data_"+str(condition)+".csv","a")
    fout.write(str(sim_num) + "\t"+
               str(timestep) + "\t" +
               str(condition) + "\t" +
               str(pop_size)+"\t"+
               str(g_i)+ "\t" +
               str(s_i)+"\t"+
               str(inverse_temp)+"\t"+
               str(conformity)+"\t"+
               str(count_inefficient) +"\t" +
               str(count_efficient)+ "\t"+
               str(num_solvers)+ "\t"+
               str(payoffs))
    fout.write("\n")
    fout.close()

def simulation(num_replicates):
    global master_sim_no
    global payoffs
    for sim_num in range(num_replicates):
        print("simulation{}".format(sim_num))
        G = generateNetwork(graph_type)
        for timestep in range(t_steps):
            #print("**** the timestep {} starts".format(timestep))
            if timestep < 7:
                payoffs=[10,0]
            else:
                payoffs=[10,20]

            solution_list = []
            solver_list = []
            knowledgable = [agent for agent in range(N) if G.nodes[agent]["data"].naive == False ]
            if len(knowledgable)==0:
                print("behavior is extinct")
                break
            for interactions in range(solves_per_day*len(knowledgable)):
                solver = choose_solver(G, knowledgable)
                solution = game(G, solver)
                solution_list.append(solution)
                solver_list.append(solver)

            #update social cue matrix
            frequencies = [solution_list.count(n) for n in range(solutions)]
            obs_learning(G, frequencies)
            for agent in range(N):
                G.nodes[agent]["data"].reset_solve_count()
            count_inefficient, count_efficient, num_solvers = extract_data(G,solution_list,knowledgable)
            #if not all the agents know how to solve, check to see if a new agent can learn
            if len(knowledgable) < N:
                knowledgable = state_switch(G, knowledgable)
            update_learn_prob(G)
            write_csv(master_sim_no,timestep,condition,N,g_i,s_i,count_inefficient,count_efficient,num_solvers)
            if (timestep+1)%7 == 0 and turnover==True:
                turnover_event(G,timestep)

        master_sim_no += 1

if __name__ == "__main__":
    create_csv()
    for conformity in conformity_values:
        for s_i_param in parameter_values:
                for g_i_param in parameter_values:
                    for inverse_temp in inverse_temp_values:
                        s_i = s_i_param
                        g_i = g_i_param
                        print("payoffs {} conformity {} inverse_temp {} s_i{} g_i{}".format(payoffs,conformity,inverse_temp,s_i,g_i))
                        simulation(num_replicates)
