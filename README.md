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

The intension provides a set-theoretic modal semantics for building representations. Multiple intentions can be used to 

### Building models ("interpreting" intensions)

The system can build models using the **interpret** command, which processes intensions to stochastically construct a model in which all the intensions are true. For example:

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