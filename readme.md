
# RTECv2

This an experimental, not properly tested extension of RTEC that supports cycles and deadlines.

# Applications

###Voting for Multi-Agent Systems

In /examples there is a formalisation of a voting protocol for multi-agent systems that includes cycles and deadlines. To test it, navigate to

**/examples/voting/experiments/queries/setting-1**

launch YAP by typing 


**yap -l mass-queries.prolog**


and then type in YAP


**YAP> performFullER('sometextfile', 10, 10, 20).**


**YAP> holdsFor(F,I), not I=[].**


The assimilated narative is 

**/examples/voting/experiments/data/stream.prolog**

while the constants used for grounding are

**/examples/voting/experiments/data/static_information.prolog**

###Business Processes

In /examples there is also a toy formalisation of business processes. To test it, navigate to

**/examples/BPM/experiments/queries/setting-1**

launch YAP by typing 


**yap -l mass-queries.prolog**


and then type in YAP


**YAP> performFullER('sometextfile', 327240000, 327240000, 327240000).**


**YAP> holdsFor(F,I), not I=[].**


The assimilated narative and the constants for grounding are in 

**/examples/BPM/experiments/data/event_logprolog**


