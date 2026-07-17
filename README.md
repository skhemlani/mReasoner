# mReasoner

mReasoner is a unified computational implementation of the model theory of thinking and reasoning (see https://modeltheory.org).

The system parses premises in natural language to build and revise mental models and generate deductive and inductive conclusions.

# 1. Project overview

**mReasoner** is a Common Lisp implementation of the mental model theory of thinking and reasoning. It converts controlled natural-language premises into semantic representations called **intensions**, uses those intensions to construct iconic mental models, and scans or revises those models to produce deductive and inductive conclusions.

The implementation covers several reasoning domains:

- Quantified assertions
- Sentential connectives
- Temporal relations
- Spatial relations
- Causal relations
- Epistemic constructions
- Possibility and necessity judgments
- Consistency judgments
- Counterexample-based reasoning

The repository is written entirely in Common Lisp. 

---

# 2. Distribution and warranty statements

Distribution and warranty statements are embedded in source-file [source-file](+mReasoner.lisp) comments.

---

# 3. Main concepts

## 3.1 Intensions

An **intension** is a semantic blueprint used to construct one or more mental models.

For example:

```lisp
(parse "All A are B")
```

returns an instance of `q-intension`, representing the quantified assertion that every member of class `A` belongs to class `B`.

Inspect it with:

```lisp
(describe (parse "All A are B"))
```

A quantified intension contains information such as:

- Subject
- Object
- Cardinality constraints
- Numerical proportion
- Boundary constraints
- Polarity
- Relation type
- Whether footnotes are required
- Whether the intension is active

Intensions are not themselves models. They encode the constraints that a model must satisfy.

---

## 3.2 Models

A **model** is an iconic representation constructed from one or more intensions.

Different reasoning domains use different model classes:

| Domain | Intension class | Model class | Model entities |
|---|---|---|---|
| Quantified reasoning | `q-intension` | `q-model` | Individuals |
| Sentential reasoning | `s-intension` | `s-model` | Possibilities |
| Temporal reasoning | `t-intension` | `t-model` | Moments |
| Spatial reasoning | `sp-intension` | `sp-model` | Things in spatial dimensions |
| Causal reasoning | `c-intension` | Usually temporal/sentential structures | Temporally constrained clauses |
| Epistemic reasoning | `e-intension` | Domain-dependent | Agents and clauses |
| Failed construction | — | `null-model` | Conflict information |

The base `model` class stores:

- `entities`
- `capacity`
- `footnote`

The `footnote` records the intensions associated with the model and is used during validation and revision.

---

## 3.3 System 1 and System 2

mReasoner distinguishes between two forms of processing:

### System 1

System 1 performs relatively direct, heuristic model construction and conclusion formation.

Typical behavior:

- Build an initial model
- Scan that model
- Generate an initial conclusion
- Do not exhaustively search for alternatives

### System 2

System 2 performs additional search and revision.

Typical behavior:

- Search for counterexamples
- Construct alternative models
- Test whether conclusions hold necessarily
- Weaken conclusions when configured to do so
- Examine whether a proposition could be true or false in another model

The global `+sigma+` parameter controls the probability that System 2 processing is invoked automatically. Passing an explicit `:system2` argument overrides the probabilistic default for many high-level operations.

---

## 3.4 Iconicity

Mental models are intended to be **iconic**: their structure corresponds to the structure of what they represent.

Examples:

- A quantified model uses one row per individual.
- A temporal model arranges events along a timeline.
- A spatial model arranges objects on a grid.
- A sentential model represents alternative possibilities.

Consequently, printing and scanning behavior varies by domain.

---

# 4. Repository structure

The loader processes modules in this order:

```text
+mReasoner.lisp
├── Classes.lisp
├── Utilities.lisp
├── API.lisp
├── Parser.lisp
├── Observer.lisp
├── Builder.lisp
├── Scanner.lisp
├── FormConclusions.lisp
├── Counterexamples.lisp
├── HighLevel.lisp
├── Diagnostics.lisp
└── jpd-belief-icon.lisp
```

The repository also includes:

```text
jsown-master/
```

which contains the bundled JSOWN JSON library.

## 4.1 Module responsibilities

### `+mReasoner.lisp`

Top-level loader and global configuration.

Responsibilities:

- Defines the system name and version
- Locates source files
- Loads the bundled JSON dependency
- Loads modules in dependency order
- Defines stochastic-model parameters
- Defines stochastic control functions

### `Classes.lisp`

Core object model.

Contains:

- Condition classes
- Intension classes
- Model classes
- JSON-output class
- Class-copying utilities
- Generic equality infrastructure

### `Utilities.lisp`

General-purpose utilities used throughout the project.

### `API.lisp`

Low-level model and intension manipulation.

Includes operations for:

- Finding referents
- Comparing models
- Comparing properties and individuals
- Creating individuals
- Negating properties
- Merging and modifying structures
- Graph and matrix traversal
- Domain-specific model operations

### `Parser.lisp`

Controlled natural-language parser and compositional semantics.

Contains:

- Grammar
- Lexical rules
- Bottom-up parser
- Semantic composition
- Parsing memoization
- Morphological processing
- Built-in intensions

The parser is based partly on parsing code associated with Peter Norvig’s *Paradigms of Artificial Intelligence Programming*, as stated in the source comments.

### `Observer.lisp`

Observation and intake-related processing used around model construction.

### `Builder.lisp`

Model construction.

Responsibilities include:

- Starting a new model
- Adding a first or second argument
- Combining existing models
- Confirming premises against existing models
- Raising consistency errors when a new premise cannot be integrated

### `Scanner.lisp`

Model validation and scanning.

Responsibilities include:

- Checking whether an intension holds in a model
- Checking conclusions across model sets
- Filtering invalid models
- Applying domain-specific validation rules
- Supporting initial-conclusion heuristics

### `FormConclusions.lisp`

Creates candidate conclusions from model structures and generates weaker alternatives when necessary.

### `Counterexamples.lisp`

Searches for model revisions or alternative models that contradict a candidate conclusion.

### `HighLevel.lisp`

User-facing reasoning operations:

- `interpret`
- `infer`
- `what-follows?`
- `what-possibly-follows?`
- `what-necessarily-follows?`
- `possible?`
- `necessary?`
- `consistent?`

### `Diagnostics.lisp`

Tracing and diagnostic support.

### `jpd-belief-icon.lisp`

Additional belief/icon-related functionality.

### `UserInterface.lisp`

A GUI-related source file exists, but it is included in the LispWorks system declaration rather than the generic loader branch. The global variable `*mR-interface*` is used to access the interface.

---

# 5. Installation

## 5.1 Requirements

You need:

1. A Common Lisp implementation
2. A local copy of the repository
3. A working Common Lisp REPL or script runner

The loader contains implementation-specific branches for :

- LispWorks
- Clozure CL / CCL
- CLISP
- SBCL
- CMUCL
- ECL
- Scieneer Common Lisp
- Allegro CL
- Related implementations recognized by the reader-conditionals

LispWorks receives the most explicit integration, including a system declaration and GUI loading. Other implementations load the source files sequentially.

---

## 5.2 Clone the repository

~~~bash
git clone https://github.com/skhemlani/mReasoner.git
cd mReasoner
~~~

---

## 5.3 Load from a REPL

### Important Note on Working Directories
Before executing the `(load ...)` command, you must ensure that your Lisp environment's **Current Working Directory** is set to the root of the cloned `mReasoner` repository. If it is not, the loader will fail with a "File not found" error.

Depending on your environment, here is how to set it:

* **From a terminal:** Navigate to the folder before starting your Lisp REPL:
  ~~~bash
  cd /path/to/mReasoner
  sbcl # or ccl, clisp, etc.
  ~~~
* **In LispWorks (Listener):** Use the graphical "Change Directory" button in the toolbar, or execute:
  ~~~lisp
  (hcl:change-directory "/path/to/mReasoner/")
  ~~~
* **Using UIOP (Most modern Common Lisps):**
  ~~~lisp
  (uiop:chdir "/path/to/mReasoner/")
  ~~~

Once in the correct directory, you can safely evaluate:

~~~lisp
(load "+mReasoner.lisp")
~~~

The exact formatting of the output may vary by Common Lisp implementation.

---

## 5.4 SBCL example

Start SBCL from the project directory:

~~~bash
sbcl
~~~

Then:

~~~lisp
(load "+mReasoner.lisp")
~~~

Or load it directly from the shell:

~~~bash
sbcl --load +mReasoner.lisp
~~~

### Troubleshooting: Encoding Errors

Depending on your system's locale settings, SBCL's strict file parser might crash with a STREAM-DECODING-ERROR when loading certain files (such as Utilities.lisp). If you encounter this, ensure your terminal and SBCL environment are explicitly configured to use UTF-8 or Latin-1 encoding. Alternatively, use LispWorks, which natively handles these files more permissively.

### Important portability note

The string version of `parse` explicitly tokenizes strings only under LispWorks and CCL reader-conditionals. On other implementations, passing a token list can be safer :

~~~lisp
(parse '(all a are b))
~~~

instead of :

~~~lisp
(parse "All A are B")
~~~

The generic loader recognizes several Common Lisp implementations, but some parser and path-handling code remains implementation-specific.

---

## 5.5 LispWorks example

**Option 1: From the Listener (REPL)**
Once your working directory is set to the project root (see section 5.3), load the entry point:

~~~lisp
(load "+mReasoner.lisp")
~~~

**Option 2: Using the GUI editor**
You can also simply open the `+mReasoner.lisp` file in the LispWorks IDE editor and click the graphical **Load** button in the toolbar. This automatically handles the path context for you.

Under LispWorks, the loader declares an mReasoner system and loads it through the LispWorks system mechanism. It also includes `UserInterface.lisp`.

---

## 5.6 JSON dependency

The repository bundles JSOWN under:

~~~text
jsown-master/
~~~

When the `:jsown` feature is not already present and `jsown-master/load.lisp` exists, the loader loads it automatically. No separate package-manager installation is normally required for the bundled dependency.

# 6. Quick start

## 6.1 Parse a premise

```lisp
(defparameter *premise*
  (parse '(all a are b)))
```

Under LispWorks or CCL, this can also be written as:

```lisp
(defparameter *premise*
  (parse "All A are B"))
```

Inspect the result:

```lisp
(describe *premise*)
```

---

## 6.2 Build a model

```lisp
(defparameter *models*
  (interpret
    '((all a are b)
      (some b are c))))
```

If string parsing is supported by your implementation:

```lisp
(defparameter *models*
  (interpret
    '("All A are B"
      "Some B are C")))
```

`interpret` returns a list of models, not a single model. Even when only one model is produced, retrieve it with:

```lisp
(first *models*)
```

The function parses any non-intension input, processes premises in order, and incrementally calls `build-model`.

---

## 6.3 Print a model

```lisp
(print-model (first *models*))
```

A quantified model may look conceptually like this:

```text
A   B
A   B
A   B   C
```

Each line represents an individual and each symbol represents one of that individual’s properties.

---

## 6.4 Draw a conclusion

```lisp
(infer
  '((all a are b)
    (some b are c))
  :task #'what-follows?)
```

With strings where supported:

```lisp
(infer
  '("All A are B"
    "Some B are C")
  :task #'what-follows?)
```

The result is a list of intensions. To render compact descriptions:

```lisp
(mapcar #'abbreviate
        (infer
          '((all a are b)
            (some b are c))
          :task #'what-follows?))
```

This example can produce conclusions corresponding to “Some A are C” and its converse “Some C are A.” ie "Ica" and "Iac" 

---

# 7. Controlled natural-language input

mReasoner does not implement unrestricted English understanding. It uses a controlled grammar and lexicon.

Unknown tokens can be interpreted as members of open categories such as:

```lisp
Nouns
Noun
Var
Name
Num
Adj
```

This allows symbolic terms such as `A`, `B`, `C`, or domain-specific nouns to participate in recognized sentence templates.

---

## 7.1 Quantified assertions

Common examples include:

```text
All A are B
Some A are B
No A are B
Some A are not B
```

Additional scalar and proportional quantifiers are represented in the grammar, including concepts such as:

```text
more
less
majority
minority
```

The quantified representation stores:

- Subject
- Object
- Cardinality
- Numerical proportion
- Boundaries
- Polarity
- Relation
- Footnote behavior

Example:

```lisp
(describe (parse '(all artists are beekeepers)))
```

---

## 7.2 Sentential connectives

The grammar includes sentence patterns for:

- Conjunction
- Inclusive disjunction
- Exclusive disjunction
- Conditional
- Biconditional
- Negation
- Neither/nor constructions

Representative inputs include:

```text
A and B
A or B
Either A or B
If A then B
A if and only if B
Neither A nor B
Not A
```

The exact accepted surface form depends on the grammar and lexicon. Token-list input is useful when debugging a sentence that fails to parse.

---

## 7.3 Temporal assertions

Examples from the project include:

```text
A happened during B
C happened after B
D happened after C
```

Usage:

```lisp
(defparameter *temporal-models*
  (interpret
    '((a happened during b)
      (c happened after b)
      (d happened after c))))

(print-model (first *temporal-models*))
```

The resulting `t-model` stores moments and lays events out along a timeline.

---

## 7.4 Spatial assertions

Examples:

```text
A is to the left of B
B is to the left of C
C is above D
```

Usage:

```lisp
(defparameter *spatial-models*
  (interpret
    '((a is to the left of b)
      (b is to the left of c)
      (c is above d))))

(print-model (first *spatial-models*))
```

The resulting `sp-model` contains things and dimensional information. Spatial intensions can record subject and object modifiers, relation, dimension, template, and distance. 

---

## 7.5 Causal assertions

Causal intensions are represented by `c-intension`, a subclass of `s-intension`.

In addition to the sentential possibility fields inherited from `s-intension`, a causal intension contains:

```lisp
temporal-constraint
```

accessible through:

```lisp
(constraint causal-intension)
```

This reflects the implementation’s treatment of causal assertions as sentential structures with temporal constraints. 

---

## 7.6 Epistemic assertions

Epistemic constructions use `e-intension`.

An epistemic intension records:

- `agent`
- `affirms`
- `negates`
- `factivity`
- `clause`

The parser’s compositional semantics includes constructions for agents that affirm or deny clauses, with factive or non-factive behavior encoded in the resulting object.

---

# 8. Core API

## 8.1 `parse`

### Signature

```lisp
(parse input)
```

### Accepted input

- A string, with implementation-specific tokenization
- A list of tokens

### Returns

An intension representing the selected semantic interpretation.

### Examples

```lisp
(parse '(all a are b))
```

```lisp
(parse '(a is to the left of b))
```

```lisp
(parse '(a happened before b))
```

### Behavior

The parser:

1. Finds lexical rules for each token.
2. Performs bottom-up parsing.
3. Retains complete sentence parses.
4. Applies compositional semantic functions.
5. Removes duplicate parses.
6. Returns the semantic value of the first valid parse.

If there is no valid sentence parse, it signals `parser-error` with a syntax-error message. If multiple semantic parses remain, the current implementation returns the first rather than raising an ambiguity error. 

### Error handling

```lisp
(handler-case
    (parse '(unsupported input sequence))
  (parser-error (condition)
    (format t "Parser failed: ~A~%"
            (text condition))))
```

---

## 8.2 `interpret`

### Signature

```lisp
(interpret intensions)
```

### Parameters

`intensions` is a list containing either:

- Natural-language strings
- Token lists
- Already-created intension objects

### Returns

A list of model objects.

### Example

```lisp
(interpret
  '((all a are b)
    (some b are c)))
```

### Processing model

For each premise:

1. Parse it if it is not already an intension.
2. Build a new model if no model exists.
3. Otherwise, integrate the premise into the current model set.
4. Store the first model as the tracer’s initial model.
5. Return the complete model set.

Premise order may affect initial model construction because premises are processed incrementally.

---

## 8.3 `build-model`

### Signature

```lisp
(build-model intension &key models)
```

### Purpose

Low-level incremental model builder.

### Decision process

The function checks whether the intension’s first and second arguments are already represented:

| Condition | Operation |
|---|---|
| Both arguments occur together | Confirm the intension |
| Arguments occur in different models | Combine models |
| Only the first argument occurs | Add the second argument |
| Only the second argument occurs | Add the first argument |
| Neither occurs | Start a new model |

If the new intension cannot be validated against an existing model, model confirmation can signal `consistency-error`.

### Example

```lisp
(defparameter *m1*
  (build-model (parse '(all a are b))))

(defparameter *m2*
  (build-model
    (parse '(some b are c))
    :models *m1*))
```

---

## 8.4 `build-n-print`

### Signature

```lisp
(build-n-print intension &key m)
```

Builds a model and immediately prints it.

```lisp
(build-n-print (parse '(all a are b)))
```

The abbreviation is:

```lisp
(bnp (parse '(all a are b)))
```

The existing model set can be supplied using `:m`.

---

## 8.5 `validate`

### Signature

```lisp
(validate intension modelset
          &key
            (validate-true t)
            (verbose nil)
            (s2 nil))
```

### Purpose

Checks whether an intension holds across a set of models.

### Parameters

| Parameter | Meaning |
|---|---|
| `intension` | Assertion to test |
| `modelset` | List of models |
| `validate-true` | When true, test that the assertion holds; otherwise test the negative condition |
| `verbose` | Emit tracing information |
| `s2` | Label diagnostic output as System 2 processing |

### Example

```lisp
(let* ((models
         (interpret
           '((all a are b)
             (some a are c))))
       (claim
         (parse '(some b are c))))
  (validate claim models))
```

The function returns true only if the required validation succeeds for every model in the supplied model set.

---

## 8.6 `validate-all-conclusions`

### Signature

```lisp
(validate-all-conclusions conclusions models
                          &key
                            (validate-true t))
```

Recursively verifies that every conclusion satisfies `validate`.

```lisp
(validate-all-conclusions
  (list conclusion-1 conclusion-2)
  models)
```

---

## 8.7 `filter-validated-models`

### Signature

```lisp
(filter-validated-models conclusion models
                         &key
                           (validate-true t))
```

Returns models in which:

1. The supplied conclusion validates.
2. The model’s own footnoted intensions also validate.

This is useful when processing alternative or revised models.

---

## 8.8 `infer`

### Signature

```lisp
(infer intensions
       &key
         (task #'what-follows?)
         (system2 (system2-enabled?))
         (v nil)
         (given nil))
```

### Parameters

| Parameter | Description |
|---|---|
| `intensions` | Input assertions or target assertions |
| `task` | Reasoning function to execute |
| `system2` | Whether to perform System 2 processing |
| `v` | Internal task option propagated to the task |
| `given` | Separate premises for evaluation tasks |

### Default task

```lisp
#'what-follows?
```

### Deductive inference

```lisp
(infer
  '((all a are b)
    (some b are c)))
```

### Explicit System 1

```lisp
(infer
  '((all a are b)
    (some b are c))
  :system2 nil)
```

### Explicit System 2

```lisp
(infer
  '((all a are b)
    (some b are c))
  :system2 t)
```

If the first input is not already an intension, `infer` maps `parse` over the input list and invokes the selected task.

---

## 8.9 `what-follows?`

### Signature

```lisp
(what-follows? intensions
               &key
                 (system2 (system2-enabled?))
                 (v nil))
```

This is an alias-like high-level operation for necessary-conclusion reasoning. Internally it calls:

```lisp
what-necessarily-follows?
```

It returns a list of conclusion intensions and records an abbreviated textual response in the diagnostic tracer.

---

## 8.10 `what-necessarily-follows?`

### Signature

```lisp
(what-necessarily-follows? intensions
                           &key
                             (system2 (system2-enabled?))
                             (v nil))
```

### System 1 behavior

Returns initial heuristic conclusions formed by scanning the initial model.

### System 2 behavior

For each initial conclusion:

1. Search for counterexamples.
2. Add discovered counterexample models to the model set.
3. Mark refuted conclusions as invalid.
4. Form weaker conclusions when possible.
5. Validate conclusions against the expanded model set.
6. Return conclusions that remain valid.

This is the principal function for distinguishing what follows from the initial model from what follows across relevant alternative models.

---

## 8.11 `what-possibly-follows?`

### Signature

```lisp
(what-possibly-follows? intensions
                        &key
                          (system2 (system2-enabled?))
                          (v nil))
```

### Purpose

Generates conclusions about what is possible.

Without System 2, it returns initial heuristic conclusions. With System 2, it considers initial and weaker conclusions and retains those that validate in the relevant models.

Example:

```lisp
(infer
  '((all a are b)
    (some b are c))
  :task #'what-possibly-follows?
  :system2 nil)
```

---

## 8.12 `possible?`

### Signature

```lisp
(possible? intensions
           &key
             (system2 (system2-enabled?))
             (given nil))
```

### Purpose

Determines whether the first target intension can hold given a set of premises.

### Example

```lisp
(possible?
  (list (parse '(some a are c)))
  :given
  (mapcar #'parse
          '((all a are b)
            (some b are c)))
  :system2 t)
```
**Note** *: Do not invoke `possible?` through infer in the current version. infer always passes a :v keyword, but `possible?` does not accept it. Call `possible?` directly and supply parsed intensions.*

### Return value

- A model object if the target is possible
- `nil` if it is not possible

### Processing

1. Build models from `given`.
2. Test the target in existing models.
3. If it fails and System 2 is enabled, search for a revised or alternative model that makes it true.
4. Return that model if one is found.

The function also records `"Possible"` or `"Not possible"` in the tracer response.

---

## 8.13 `necessary?`

### Signature

```lisp
(necessary?
  (list (parse '(some b are a)))
  :given
  (mapcar #'parse
          '((all a are b)
            (some a are c)))
  :system2 t)
```

### Purpose

Determines whether the first target intension is necessary given a set of premises.

### Example

```lisp
(infer
  '((some b are a))
  :task #'necessary?
  :given
  '((all a are b)
    (some a are c))
  :system2 t)
```

**Note** *: Do not invoke `necessary?` through infer in the current version. infer passes an unsupported :v keyword. Call `necessary?` directly and supply parsed intensions.*

### Return value

- A model set or validated-model result when necessary
- `nil` when not necessary

### Processing

1. Validate the target in the initial models.
2. If it already fails, return `nil`.
3. Without System 2, return the initial validation result.
4. With System 2, search for a counterexample model.
5. If a counterexample is found, return `nil`.
6. Otherwise return the models supporting necessity. 

---

## 8.14 `consistent?`

### Signature

```lisp
(consistent? intensions
             &key
               (system2 (system2-enabled?))
               (v nil))
```

### Purpose

Attempts to build a model in which all supplied assertions hold.

Example:

```lisp
(let ((intensions
        (mapcar #'parse
                '((all a are b)
                  (no a are b)))))
  (consistent? intensions :system2 t))
```
*Note : Unlike `interpret`, `consistent?` expects intension objects rather than raw token lists. Parse the assertions before calling it.*

A successful result contains model information; an inconsistent set yields no successful model. The surrounding high-level code also includes specialized consistency routines that compare initial and revised models and can calculate model distance.

---

# 9. Printing and inspection

## 9.1 `print-model`

```lisp
(print-model model)
```

Prints a domain-appropriate iconic representation.

Example:

```lisp
(let ((model
        (first
          (interpret
            '((all a are b)
              (some b are c))))))
  (print-model model))
```

---

## 9.2 `print-models`

```lisp
(print-models models)
```

Use this when model construction produces multiple alternatives.

---

## 9.3 `describe`

Because intensions and models are CLOS objects, standard Common Lisp inspection works:

```lisp
(describe (parse '(all a are b)))
```

```lisp
(describe
  (first
    (interpret
      '((all a are b)
        (some b are c)))))
```

---

## 9.4 `abbreviate`

```lisp
(abbreviate intension)
```

Produces the project’s compact representation of an intension.

```lisp
(mapcar #'abbreviate
        (infer
          '((all a are b)
            (some b are c))))
```

The exact notation depends on the intension type.

---

# 10. Class reference

## 10.1 Condition classes

### `parser-error`

Signaled when input cannot be parsed.

Slot:

```lisp
text
```

### `consistency-error`

Signaled when model construction cannot consistently integrate a premise.

Slot:

```lisp
text
```

### `ambiguity-warning`

Represents ambiguity-related warnings.

Slot:

```lisp
text
```

### `false-heuristic-conclusion-error`

Used when initial conclusion formation produces a false heuristic conclusion.

Slot:

```lisp
text
```

These conditions are defined in `Classes.lisp`.

---

## 10.2 Base `intension`

```lisp
(defclass intension ...)
```

Slots:

| Slot | Accessor | Meaning |
|---|---|---|
| `first-argument` | `first-argument` | First participant or argument |
| `second-argument` | `second-argument` | Second participant or argument |
| `active` | `active` | Whether the constraint is active |

---

## 10.3 `null-intension`

Used for special outcomes such as “No valid conclusion.”

Slots:

| Slot | Accessor |
|---|---|
| `description` | `description` |
| `abbreviation` | `abbreviation` |

Built-in constant:

```lisp
*nvc*
```

which represents:

```text
No valid conclusion
```

with abbreviation:

```text
NVC
```

---

## 10.4 `q-intension`

Quantified assertion.

| Slot | Accessor |
|---|---|
| `first-argument` | `subject` |
| `second-argument` | `object` |
| `cardinality` | `cardinality` |
| `numprop` | `numprop` |
| `boundary` | `boundary` |
| `polarity` | `polarity` |
| `footnotes` | `footnotes` |
| `relation` | `relation` |

---

## 10.5 `t-intension`

Temporal assertion.

| Slot | Accessor |
|---|---|
| `first-argument` | `subject` |
| `second-argument` | `object` |
| `precedence` | `precedence` |
| `start-time` | `start-time` |
| `end-time` | `end-time` |
| `reference-time` | `reference-time` |
| `relation-to-utterance` | `relation-to-utterance` |

---

## 10.6 `sp-intension`

Spatial assertion.

| Slot | Accessor |
|---|---|
| `first-argument` | `subject` |
| `second-argument` | `object` |
| `first-argument-modifier` | `subject-modifier` |
| `second-argument-modifier` | `object-modifier` |
| `relation` | `spatial-relation` |
| `dimension` | `spatial-dimension` |
| `template` | `spatial-template` |
| `distance` | `spatial-distance` |

---

## 10.7 `s-intension`

Sentential assertion.

| Slot | Accessor | Meaning |
|---|---|---|
| `first-argument` | `first-clause` | First clause |
| `second-argument` | `second-clause` | Second clause |
| `both` | `both` | Both clauses hold |
| `first-only` | `first-only` | Only the first holds |
| `second-only` | `second-only` | Only the second holds |
| `neither` | `neither` | Neither holds |

These four possibility fields encode the cases licensed by a connective.

---

## 10.8 `c-intension`

Subclass of `s-intension`.

Additional slot:

| Slot | Accessor |
|---|---|
| `temporal-constraint` | `constraint` |

---

## 10.9 `e-intension`

Epistemic assertion.

| Slot | Accessor |
|---|---|
| `first-argument` | `agent` |
| `affirms` | `affirms` |
| `negates` | `negates` |
| `factivity` | `factivity` |
| `second-argument` | `clause` |

The intension and model class definitions above come from `Classes.lisp`. 

---

## 10.10 Base `model`

Slots:

| Slot | Accessor | Default |
|---|---|---|
| `entities` | `entities` | Required |
| `capacity` | `capacity` | `infinity` |
| `footnote` | `footnote` | Required by constructor usage |

---

## 10.11 `q-model`

Quantified model.

```lisp
(individuals model)
```

returns the model’s individuals.

---

## 10.12 `s-model`

Sentential model.

```lisp
(possibilities model)
```

returns the represented possibilities.

---

## 10.13 `t-model`

Temporal model.

```lisp
(moments model)
```

returns temporal moments.

---

## 10.14 `sp-model`

Spatial model.

```lisp
(things model)
(dimensions model)
```

return its objects and dimensions.

---

## 10.15 `null-model`

Conflict model.

```lisp
(conflict model)
```

returns conflict information.

---

## 10.16 `json-output`

Container for structured output.

| Slot | Accessor | Default |
|---|---|---|
| `description` | `description` | `nil` |
| `validation` | `validation` | `nil` |
| `model-string` | `model-string` | `nil` |
| `model` | `model` | `nil` |
| `warning` | `warn-log` | `nil` |
| `error` | `error-log` | `nil` |

This class is suitable for adapters that need to return model, validation, warning, and error information together.

---

# 11. Stochastic configuration

mReasoner contains a probabilistic parameter system for simulating variation in reasoning.

## 11.1 Main parameters

| Parameter | Default | Purpose |
|---|---:|---|
| `*stochastic*` | `t` at initial load | Enables or disables stochastic operation |
| `*build-attempts*` | `1000` | Maximum stochastic model-building attempts |
| `*exhaustive-search-depth*` | `7` | Depth bound for exhaustive search |
| `+lambda+` | `4.0` | Controls generated model size |
| `+sigma+` | `0.0` | Probability of invoking System 2 |
| `+epsilon+` | `0.0` | Probability-related control for canonical construction |
| `+omega+` | `0.0` | Probability of weakening a refuted conclusion |
| `+kappa+` | `0.0` | Probability of spatial compression |

The source describes `+lambda+` as the parameter of a left-truncated Poisson distribution used to generate model cardinality. Values `0` and `1` are excluded by `generate-size`.

---

## 11.2 Set all reasoning parameters

```lisp
(set-system-parameters
  :lambda 4.0
  :sigma 0.5
  :epsilon 0.1
  :omega 0.75
  :kappa 0.25)
```

This also enables stochastic mode.

### Parameter interpretation

```lisp
:sigma 0.5
```

means System 2 is selected with approximately 50% probability when the caller does not explicitly supply `:system2`.

```lisp
:omega 0.75
```

means conclusion weakening is selected with approximately 75% probability when the relevant process is reached.

---

## 11.3 Reset parameters

```lisp
(reset-system-parameters)
```

This sets:

```lisp
*stochastic* => nil
+lambda+      => 4.0
+sigma+       => 0.0
+epsilon+     => 0.0
+omega+       => 0.0
+kappa+       => 0.0
```

Note that resetting disables stochastic mode, whereas the initial declaration of `*stochastic*` in the loader is true.

---

## 11.4 Enable or disable stochastic mode

```lisp
(enable-stochastic-mode)
```

```lisp
(disable-stochastic-mode)
```

Check status:

```lisp
(stochastic-enabled?)
```

---

## 11.5 Individual stochastic decisions

The loader defines:

```lisp
(generate-size)
(build-canonical?)
(system2-enabled?)
(weaken-conclusions?)
(spatial-compression?)
```

These functions sample the global parameters and return a generated size or Boolean decision.

---

# 12. Low-level model utilities

The project exposes many internal utilities in the global Lisp environment. The following are among the more useful ones.

## 12.1 Referent lookup

```lisp
(find-referent-in-modelset referent models)
```

Returns models containing the referent.

```lisp
(find-referent-in-model referent model)
```

Performs domain-specific lookup.

```lisp
(find-referent-in-individual referent individual)
```

Checks for a referent in an individual.

Example:

```lisp
(find-referent-in-individual
  '(a)
  '((a) (b)))
```

---

## 12.2 Property operations

```lisp
(property-equal property-1 property-2)
```

```lisp
(negative-property property)
```

```lisp
(negate-property property)
```

Examples:

```lisp
(property-equal '(a) '(a))
;; => T
```

```lisp
(negative-property '(- a))
;; => T
```

```lisp
(negate-property '(a))
;; => (- A)
```

```lisp
(negate-property '(- a))
;; => (A)
```

---

## 12.3 Individual operations

```lisp
(individual-equal individual-1 individual-2)
```

Compares property sets regardless of ordering.

```lisp
(has-property property individual)
```

Checks whether an individual contains a property.

```lisp
(have-all-properties individual-1 individual-2)
```

Checks for equality through mutual subset relations.

```lisp
(conflicting-properties individual-1 individual-2)
```

Checks whether the individuals contain contradictory properties.

```lisp
(merge-individuals individual-1 individual-2)
```

Combines compatible individuals.

---

## 12.4 Creating quantified individuals

```lisp
(make-individuals number argument)
```

Example:

```lisp
(make-individuals 3 '(a))
```

constructs a quantified model containing three `A` individuals.

```lisp
(make-complex-individuals number individual)
```

Example:

```lisp
(make-complex-individuals
  3
  '((a) (- b)))
```

creates three structurally identical individuals.

---

## 12.5 Model comparison

The generic function:

```lisp
(equals entity-1 entity-2)
```

has methods for model and intension structures.

For intensions, equality compares corresponding direct slots recursively. Additional functions compare:

- Individuals
- Possibilities
- Properties
- Model contents

These low-level functions are implemented primarily in `API.lisp`.

---

# 13. Model construction lifecycle

The practical lifecycle of a reasoning request is:

```text
Natural-language premise
        │
        ▼
      parse
        │
        ▼
    Intension object
        │
        ▼
     interpret
        │
        ▼
    build-model
        │
        ├── start model
        ├── add argument
        ├── combine models
        └── confirm premise
        │
        ▼
    Initial model set
        │
        ▼
      scanner
        │
        ▼
Initial heuristic conclusions
        │
        ▼
 Counterexample search, if System 2
        │
        ▼
Alternative/revised models
        │
        ▼
Validated or weakened conclusions
```

The builder determines how a premise should be integrated based on the presence of its first and second arguments. The scanner checks whether assertions hold, while the counterexample component searches for alternatives that can invalidate a conclusion.

---

# 14. Error handling

## 14.1 Parser errors

```lisp
(handler-case
    (parse '(all a unexpectedly b))
  (parser-error (condition)
    (format t "Parse error: ~A~%"
            (text condition))))
```

---

## 14.2 Consistency errors

```lisp
(assert
  (handler-case
      (progn
        (interpret
          '((all a are b)
            (no a are b)
            (some a are a)))
        nil)
    (consistency-error () t)))
```

Whether an apparently contradictory pair immediately raises an error can depend on existential commitments and the model-theoretic interpretation. Adding an explicit existential premise can make contradictions operationally clearer.

---

## 14.3 General safe wrapper

```lisp
(defun safely-interpret (premises)
  (handler-case
      (values (interpret premises) nil)
    (parser-error (condition)
      (values nil
              (format nil "Parser error: ~A"
                      (text condition))))
    (consistency-error (condition)
      (values nil
              (format nil "Consistency error: ~A"
                      (text condition))))
    (error (condition)
      (values nil
              (format nil "Unexpected error: ~A"
                      condition)))))
```

Usage:

```lisp
(multiple-value-bind (models error-message)
    (safely-interpret
      '((all a are b)
        (some a are c)))
  (if error-message
      (format t "~A~%" error-message)
      (print-models models)))
```

---

# 15. Extension guide

## 15.1 Adding vocabulary

The lexicon is stored in:

```lisp
*lexicon*
```

The parser recognizes categories for nouns, verbs, adjectives, connectives, adverbs, prepositions, punctuation, negation, and other grammatical elements.

For simple symbolic nouns and variables, explicit lexicon changes may not be necessary because open categories support unknown tokens.

For irregular plural forms, add mappings to:

```lisp
*plural-words*
```

Conceptually:

```lisp
(push '(geese goose) *plural-words*)
```

After modifying lexical data, clear memoized parser lookups:

```lisp
(clear-memoize 'lexical-rules)
(clear-memoize 'rules-starting-with)
```

---

## 15.2 Adding grammar rules

Grammar rules are stored in:

```lisp
*grammar*
```

The general rule form is:

```lisp
(LHS -> (RHS-CATEGORY-1 RHS-CATEGORY-2 ...)
     SEMANTIC-FUNCTION)
```

Example structure:

```lisp
(S -> (Quant-NP Pred-VP) sem-monadic)
```

A semantic function receives the semantic values of its constituents and returns an intension or an intermediate semantic structure.

After changing the grammar, call:

```lisp
(use *grammar*)
```

The `use` function replaces the active grammar and clears memoized lexical and rule lookups. 

---

## 15.3 Adding a new intension type

A new intension type should generally:

1. Subclass `intension` or an appropriate existing subclass.
2. Define slots and accessors.
3. Add parser semantics that construct the class.
4. Add model-building methods.
5. Add validation/scanning methods.
6. Add printing and abbreviation methods.
7. Add counterexample-generation logic if System 2 should support it.
8. Extend equality behavior if direct-slot comparison is insufficient.

Example skeleton:

```lisp
(defclass custom-intension (intension)
  ((relation
     :accessor custom-relation
     :initarg :relation))
  (:documentation
   "Represents a custom reasoning relation."))
```

---

## 15.4 Adding a new model type

A new iconic model should subclass `model`:

```lisp
(defclass custom-model (model)
  ((entities
     :accessor custom-entities
     :initarg :entities)))
```

You would then normally implement methods for:

```lisp
start-model
add-first-argument
add-second-argument
confirm
validate-model
print-model
equals
```

Depending on the domain, you may also need:

- Model combination
- Model revision
- Initial-conclusion formation
- Weaker-conclusion formation
- Counterexample search
- Referent lookup

---

# 16. Recommended application wrapper

Because mReasoner returns CLOS objects and uses global parameters, applications should isolate it behind a small API.

```lisp
(defun reason-about (premises
                     &key
                       (system2 t))
  "Parse PREMISES, build models, and return conclusions."
  (let* ((intensions
           (mapcar #'parse premises))
         (models
           (interpret intensions))
         (conclusions
           (what-follows?
             intensions
             :system2 system2)))
    (list
      :intensions intensions
      :models models
      :conclusions conclusions
      :abbreviated-conclusions
      (mapcar #'abbreviate conclusions))))
```

Example:

```lisp
(reason-about
  '((all a are b)
    (some a are c))
  :system2 t)
```

For external services, convert model and conclusion objects to application-specific structures rather than exposing raw CLOS instances.

---

# 17. Testing recommendations

The repository does not expose a conventional modern test suite from its top-level structure, so downstream users should add regression tests around the public behavior they rely on.

## 17.1 Parser tests

```lisp
(assert
  (typep
    (parse '(all a are b))
    'q-intension))
```

## 17.2 Model construction tests

```lisp
(let ((models
        (interpret
          '((all a are b)
            (some a are c)))))
  (assert models)
  (assert (typep (first models) 'q-model)))
```

## 17.3 Validation tests

```lisp
(let* ((models
         (interpret
           '((all a are b)
             (some a are c))))
       (claim
         (parse '(some b are c))))
  (assert
    (validate claim models)))
```

## 17.4 Contradiction tests

```lisp
(assert
  (null
    (handler-case
        (progn
          (interpret
            '((all a are b)
              (no a are b)
              (some a are a)))
          nil)
      (consistency-error () t))))
```

Adapt the assertion to the exact contradiction behavior you expect in your target version.

## 17.5 Deterministic tests

Disable stochastic behavior before running repeatable tests:

```lisp
(reset-system-parameters)
(disable-stochastic-mode)
```

Pass explicit task settings:

```lisp
(infer premises :system2 nil)
```

or:

```lisp
(infer premises :system2 t)
```

This avoids relying on a probabilistic `+sigma+` decision.

---

# 18. Known limitations and caveats

## 18.1 Controlled language only

The parser recognizes grammar templates and lexical categories rather than unrestricted natural language. Semantically equivalent sentences may not all parse.

Use simple canonical forms such as:

```text
All A are B
Some A are B
A happened before B
A is to the left of B
If A then B
```

---

## 18.2 Common Lisp portability

Although the loader includes branches for several implementations, parts of the code are implementation-specific.

In particular:

- String tokenization in `parse` is explicitly implemented for LispWorks and CCL.
- LispWorks has dedicated system and GUI integration.
- Path handling differs by implementation.
- Direct token lists are generally the safest portable parser input.

These limitations are visible in the loader and parser reader-conditionals.

---

## 18.3 No ASDF system at repository root

The public top-level file listing does not include an `.asd` system definition. Generic implementations load files sequentially through `+mReasoner.lisp`, while LispWorks uses an inline `defsystem` declaration. 

---

## 18.4 No versioned release artifacts

The repository currently has no published GitHub release. Pin integrations to a specific commit rather than assuming that `master` is a stable release line. 

Example:

```bash
git rev-parse HEAD
```

Record that value in your application’s dependency documentation.

---

## 18.5 Global mutable state

Configuration and tracing use global variables, including:

```lisp
*stochastic*
*build-attempts*
*exhaustive-search-depth*
+lambda+
+sigma+
+epsilon+
+omega+
+kappa+
*mR-interface*
```

Consequences:

- Parallel requests can interfere with each other.
- Reproducibility requires controlled parameter values and random state.
- A server wrapper should serialize access or isolate execution by process.

---

## 18.6 Stochastic results

When stochastic processing is active, repeated calls may produce different model sizes, System 2 decisions, weakening decisions, or spatial compression behavior.

For reproducibility:

```lisp
(reset-system-parameters)
```

and pass an explicit `:system2` value.

You may also bind or initialize Common Lisp’s `*random-state*` according to your implementation.

---

# 19. Complete worked example

```lisp
;; Load the system.
(load "+mReasoner.lisp")

;; Use deterministic configuration.
(reset-system-parameters)
(disable-stochastic-mode)

;; Define premises as token lists for portability.
(defparameter *premises*
  '((all a are b)
    (some a are c)))

;; Parse the premises.
(defparameter *intensions*
  (mapcar #'parse *premises*))

;; Inspect semantic representations.
(dolist (intension *intensions*)
  (describe intension))

;; Construct models.
(defparameter *models*
  (interpret *intensions*))

;; Print all constructed models.
(print-models *models*)

;; Generate direct heuristic conclusions.
(defparameter *system-1-conclusions*
  (what-follows?
    *intensions*
    :system2 nil))

;; Generate conclusions after counterexample search.
(defparameter *system-2-conclusions*
  (what-follows?
    *intensions*
    :system2 t))

;; Print compact forms.
(format t "~%System 1: ~{~A~^, ~}~%"
        (mapcar #'abbreviate
                *system-1-conclusions*))

(format t "System 2: ~{~A~^, ~}~%"
        (mapcar #'abbreviate
                *system-2-conclusions*))

;; Evaluate a target claim.
(defparameter *target*
  (parse '(some b are c)))

(format t "Target validates in initial model(s): ~A~%"
        (validate *target* *models*))

;; Ask whether the target is possible.
(defparameter *possibility-model*
  (possible?
    (list *target*)
    :given *intensions*
    :system2 t))

(if *possibility-model*
    (progn
      (format t "The target is possible.~%")
      (print-model *possibility-model*))
    (format t "The target is not possible.~%"))

;; Ask whether the target is necessary.
(defparameter *necessity-result*
  (necessary?
    (list *target*)
    :given *intensions*
    :system2 t))

(format t "The target is necessary: ~A~%"
        (not (null *necessity-result*)))
```

---

# 20. Academic references


### References

* Khemlani, S., & Johnson-Laird, P.N. (2022). Reasoning about properties: A computational theory. Manuscript in press at Psychological Review. [PDF](https://modeltheory.org/papers/2021mReasoner.pdf)
* Khemlani, S., Byrne, R.M.J., & Johnson-Laird, P.N. (2018). Facts and possibilities: A model-based theory of sentential reasoning. Cognitive Science. [PDF](https://modeltheory.org/papers/2018sentential-reasoning.pdf)
* Johnson-Laird, P. N., Khemlani, S., & Goodwin, G.P. (2015). Logic, probability, and human reasoning. Trends in Cognitive Sciences, 19, 201-214. [PDF](https://modeltheory.org/papers/2015logic-prob-reasoning.pdf)
* Khemlani, S., Harrison, A. M., & Trafton, J. G. (2015). Episodes, events, and models. Frontiers in Human Neuroscience, 9, 590, 1-13. [PDF](https://modeltheory.org/papers/2015events-episodes-models.pdf)
* Khemlani, S., Lotstein, M., & Johnson-Laird, P. N. (2015). Naive probability: Model-based estimates of unique events. Cognitive Science, 39, 1216–1258. [PDF](https://modeltheory.org/papers/2015naive-probability.pdf)
* Khemlani, S., Lotstein, M., Trafton, J.G., & Johnson-Laird, P. N. (2015). Immediate inferences from quantified assertions. Quarterly Journal of Experimental Psychology, 68, 2073–2096. [PDF](https://modeltheory.org/papers/2015events-episodes-models.pdf)
* Ragni, M., Khemlani, S., & Johnson-Laird (2014). The evaluation of the consistency of quantified assertions. Memory & Cognition, 42, 53-66. [PDF](https://modeltheory.org/papers/2013consistency-of-quantified.pdf)
* Khemlani, S., & Johnson-Laird, P. N. (2013). The processes of inference. Argument & Computation, 4, 1-20. [PDF](https://modeltheory.org/papers/ssk/ssk2013procsofinf.pdf)

---

## Summary

The principal user-facing workflow is:

```lisp
(load "+mReasoner.lisp")

(defparameter *models*
  (interpret
    '((all a are b)
      (some b are c))))

(print-models *models*)

(mapcar #'abbreviate
        (infer
          '((all a are b)
            (some b are c))
          :task #'what-follows?
          :system2 t))
```

The most important public operations are:

| Operation | Purpose |
|---|---|
| `parse` | Convert controlled language into an intension |
| `interpret` | Construct models from premises |
| `print-model` | Display one iconic model |
| `print-models` | Display a set of models |
| `validate` | Test an assertion in models |
| `infer` | Run a selected reasoning task |
| `what-follows?` | Generate necessary conclusions |
| `what-possibly-follows?` | Generate possible conclusions |
| `possible?` | Test whether a target can hold |
| `necessary?` | Test whether a target must hold |
| `consistent?` | Test whether assertions can hold together |
| `set-system-parameters` | Configure stochastic cognition parameters |
| `reset-system-parameters` | Restore deterministic default parameter values |