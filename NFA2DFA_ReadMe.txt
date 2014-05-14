NFATODFA1 takes a brute-force approach.

Algorithm:
1. Start with finding all subsets of given nfa states, let us consider the subsets will be new states of target DFA.
2. for each newstate find the transistions with the given input nfa transisitons.
3. from the new transistions , start from the initial state and include all the states which can be reachable.
4. find the states which contain the final nodes of given nfa, which forms final states of target DFA. 


Step1 corresponds ->  function allCombinations
step2 corresponds -> function findTransistions
step3 correspoonds -> function refinestates
step4 corresponds -> function removeFinal


#use "NFATODFA1.ml"







