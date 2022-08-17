# Progress Report (May 23, 2022)



We are given an input set of propositions $I\subseteq L$, which we can combine into a single proposition $\varphi \in L$, via
$$
\varphi \equiv i_1\wedge \ldots \wedge i_n \qquad \text{for $i_1,\ldots,i_n\in I$.}
$$
Then, via the inference rules, we can factor, distribute, eliminate terms with the knowledge operator, via
$$
K_\alpha (\varphi \wedge \psi)\equiv K_\alpha\varphi\wedge K_\alpha\psi\\
K_\alpha (\varphi \vee \psi)\equiv K_\alpha\varphi\vee K_\alpha\psi\\
K_\alpha \varphi \vee \neg K_\alpha \varphi \equiv\top\\
%K_\alpha \varphi \wedge \neg K_\alpha \varphi \equiv \neg K_\alpha \varphi
$$
 and others. Then once we have the form 
$$
\varphi\equiv K_\alpha\varphi_1 \wedge \ldots \wedge \neg K_\alpha \varphi_k\wedge \ldots \wedge K_\alpha\varphi_n
$$
which is conjunctive normal form with the knowledge operators completely distributed, we can more easily analyze the formula.

A Kripke Model $M$ in the state $s\in D(M)$, models the formula $\varphi$, based on some constraints given in the definition of Kripke Models, and is denoted by $M,s\vDash \varphi$. 

We are given $\varphi$, assumed current state $r$, and want to find a model $M$, such that $M,r\models \varphi$. By thinking about the definitions for a Kripke Model, and entailment, we have the corresponding problem of finding a graph $G=(\{\circ \}\cup V,E)$, where $r=\circ$ is the real world, $V$ is a set of vertices, corresponding to the states, a set of primitive propositions $At$, set of agents $Ag$, a subset $v_s \subseteq At$ parameterized by a vertex $s\in V$, and for any primitive proposition $\psi$, $v_s$ contains $\psi$ or its negation, an edge set $E$, such that $E\subseteq \{\circ\}\times V$, and an edge-agent-label relation $\ell \subseteq E\times Ag$, an abbreviation $(e,\alpha)\in \ell\iff e\in \ell_\alpha$ is used, that assigns an agent to zero or more directed edges, starting at $\circ$, this corresponds to the accessibility relation.



EZ Part:

The sets $At$ and $Ag$ can decided on quite easily by examining $\varphi$. Then each state/vertex $s\in V$, can be indexed by a natural number $k=1,\ldots,2^{|At|}$, which allows there to be a unique subset $v_i\subseteq At$, such that $v_i$ contains a primitive proposition if and only if it does not contain it's negation, $\psi \in v_i\iff \neg \psi \notin v_i$. 



Hard Part:

Now we only need to find the relation $E$, along with the labeling relation $\ell_a$ for each agent $\alpha \in Ag$. 

$$
K_a (p_1\wedge p_2)
$$


$$

$$

