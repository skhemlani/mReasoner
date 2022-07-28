# mReasoner

mReasoner is a unified computational implementation of the model theory of thinking and reasoning (see https://modeltheory.org).

The system parses premises in natural language to build and revise mental models and generate deductive and inductive conclusions.

## Installing mReasoner

mReasoner is written in Lisp and can be interpreted by loading +mReasoner.lisp in a Lisp interpreter of your choice (such as LispWorks).

## Quick start

### Parsing language into *intensions*

Once mReasoner is loaded, it's possible to use the system to build mental models based on natural language input. The system can parse natural language input using the **parse** command, as in:

    > (parse "All A are B")
    #<Q-INTENSION 4020141973>
This command returns an "intension", i.e., a blueprint for building mental models. You can inspect the contents of the intension as follows:

    > (describe (parse "All A are B"))
    
    #<Q-INTENSION 40200741C3> is a Q-INTENSION
    FIRST-ARGUMENT       A
    SECOND-ARGUMENT      B
    CARDINALITY          ((? 4) (>= 1))
    NUMPROP              (? 4)
    BOUNDARY             ((= CARDINALITY))
    POLARITY             T
    FOOTNOTES            T
    RELATION             INCLUDE
    ACTIVE               T

The intension provides a set-theoretic modal semantics for building representations. Hence, intensions capture the intended meaning of a natural language sentence. These intensions are then used to construct models using the **interpret** function.

### Building models ("interpreting" intensions)

The system can build models using the **interpret** function. This function takes a list of premises (strings), parses them into intensions, and then attempts to stochastically construct a model in which all those intensions are true. For example:

    > (interpret '("All A are B" "Some B are C"))
    (#<Q-MODEL 4020347923>)

The command **print-model** allows you to view the model you just constructed:

    > (print-model (first *))
    
       A   B
       A   B
       A   B   C

This model, which represents an individual on each separate line, shows that *all* of the As are Bs and that only *some* of the Bs are As.

### Making deductive inferences

What follows given the two premises above? In particular, what's the relation between As and Cs? According to the model theory, people inspect and scan internal models to build conclusions. mReasoner simulates the process using the command **infer**, as in:

    > (infer '("All A are B" "Some B are C") :task #'what-follows?)
    (#<Q-INTENSION 4020157D53> #<Q-INTENSION 402016F873>)

The system generates two conclusions, which correspond to "Some A are C" and "Some C are A".

### The iconic structure of model representations

A fundamental proposal of the model theory is that mental models are *iconic*: their structure mimics the structure of the situations they represent. For that reason, mReasoner constructs different models for different domains. It can construct quantificational models, as shown above. It can also construct temporal models, e.g.,

    > (interpret '("A happened during B" "C happened after B" "D happened after C"))
    (#<T-MODEL 8210048643>)

    > (print-model (first *))
	
	[B   ]    
	   A   C D

and spatial models, e.g.,

    > (interpret '("A is to the left of B" "B is to the left of C" "C is above D"))
    (#<SP-MODEL 80101280D3>)

    > (print-model (first *))
	
	A                B                C                
	                                  D

Each of these forms of model representation are scanned, processed, and revised in ways unique to their structure.

### References

* Khemlani, S., & Johnson-Laird, P.N. (2022). Reasoning about properties: A computational theory. Manuscript in press at Psychological Review. [PDF](https://modeltheory.org/papers/2021mReasoner.pdf)
* Khemlani, S., Byrne, R.M.J., & Johnson-Laird, P.N. (2018). Facts and possibilities: A model-based theory of sentential reasoning. Cognitive Science. [PDF](https://modeltheory.org/papers/2018sentential-reasoning.pdf)
* Johnson-Laird, P. N., Khemlani, S., & Goodwin, G.P. (2015). Logic, probability, and human reasoning. Trends in Cognitive Sciences, 19, 201-214. [PDF](https://modeltheory.org/papers/2015logic-prob-reasoning.pdf)
* Khemlani, S., Harrison, A. M., & Trafton, J. G. (2015). Episodes, events, and models. Frontiers in Human Neuroscience, 9, 590, 1-13. [PDF](https://modeltheory.org/papers/2015events-episodes-models.pdf)
* Khemlani, S., Lotstein, M., & Johnson-Laird, P. N. (2015). Naive probability: Model-based estimates of unique events. Cognitive Science, 39, 1216–1258. [PDF](https://modeltheory.org/papers/2015naive-probability.pdf)
* Khemlani, S., Lotstein, M., Trafton, J.G., & Johnson-Laird, P. N. (2015). Immediate inferences from quantified assertions. Quarterly Journal of Experimental Psychology, 68, 2073–2096. [PDF](https://modeltheory.org/papers/2015events-episodes-models.pdf)
* Ragni, M., Khemlani, S., & Johnson-Laird (2014). The evaluation of the consistency of quantified assertions. Memory & Cognition, 42, 53-66. [PDF](https://modeltheory.org/papers/2013consistency-of-quantified.pdf)
* Khemlani, S., & Johnson-Laird, P. N. (2013). The processes of inference. Argument & Computation, 4, 1-20. [PDF](https://modeltheory.org/papers/ssk/ssk2013procsofinf.pdf)