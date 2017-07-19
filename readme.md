

# RTECv2

This an experimental, not properly tested extension of RTEC that supports cycles and deadlines.

# Applications

In /examples there is a formalisation of a voting protocol for multi-agent systems that includes cycles and deadlines. To test it, navigate to

**/examples/voting/experiments/queries/setting-1**

launch YAP by typing 


**yap -l mass-queries.prolog**


and then type in YAP


**YAP> performFullER('sometextfile', 10, 10, 20).**


**YAP> holdsFor(F,I).**


The assimilated narative is 

**/examples/voting/experiments/data/stream.prolog**

while the constants used for grounding are

**/examples/voting/experiments/data/static_information.prolog**


