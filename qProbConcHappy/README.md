# qProbConc

This README explains how to use `qProbConc`, a Haskell tool first developed during my PhD. See [qProbConc](https://github.com/vegf17/qProbConc) for the original version of the tool.

## Installation and quick start

`qProbConc` is a Haskell library. To build it and load its modules into GHCi, you need:

- [GHC](https://www.haskell.org/ghc/)
- [cabal-install](https://cabal.readthedocs.io/en/stable/)

The recommended way to install both is [GHCup](https://www.haskell.org/ghcup/).

This project uses **GHC 9.6.7**. Using a different GHC version may lead to dependency conflicts.

### Clone and build the project

In a terminal, run:

```bash
git clone --filter=blob:none --sparse -b qProbConc https://github.com/vegf17/playground.git
cd playground
git sparse-checkout set qProbConcHappy
cd qProbConcHappy
cabal build
cabal repl
```

These commands clone only the `qProbConcHappy` folder from the `qProbConc` branch, build the Cabal project, and open a GHCi session.

Once GHCi is open, load the `Run` module:

```haskell
:l Run.hs
```

### Run your first example

Create a file called `myExample.txt` inside the `qProbConcHappy` folder with the following contents:

```text
---QtCoinTossConc---
hist: 10
k: 10
cs: 2
qs: 2

H(q0); Meas(c0,q0) || H(q1); Meas(c1,q1)
---QtCoinTossConc---
```

Then, in the open GHCi session, run:

```haskell
runSem "./myExample.txt" initSch
```

This executes the program using the k-step operational semantics and prints the result.

To generate an interactive histogram, run:

```haskell
runHist "./myExample.txt" initSch
```

This prints a link. Open the link in a web browser to interact with the generated histogram.

The argument `initSch` is a scheduler defined in `KStep.hs`. It is used by `runSem` and `runHist` to decide how concurrent programs are scheduled.



## How to use the tool (to do)

1. Inside the downloaded folder, create a txt file with one or more programs with the following format
(to see some examples, take a look at the txt files we have developed inside the folder "examples")
   >``---ProgramName---``
   >
   >``hist: int_value``
   >
   >``k: int_value``
   >
   > ``cs: int_value OR list_variables OR list_variables_and_initial_values ``
   >
   > ``qs: int_value OR list_variables OR link_function_plus_quantum_state``
   >
   >``C``
   >
   >``---ProgramName---``
    - ``ProgramName``: is the name of the program without spaces
    - ``hist``: holds the information to build the histogram 
      - ``int_value``: is the number of samples for building the histogram
    - ``k``: is the number of computational steps the $k$-step semantics performs
    - ``cs``: is the classical state 
      - ``int_value``: number of classical variables initialised at 0 (the name of the classical
  variables are given by "c_i" where 0<=i<=int_value-1)
      - ``list_variables``: list of variables defined by the user, initialised at 0
      - ``list_variables_and_initial_values``: list of variables and respective initial value defined by the user
    - ``qs``: is the quantum state 
      - ``int_value``: number of quantum variables initialised at |0> (the name of the quantum
  variables are given by "q_i" where 0<=i<=int_value-1); a linking function is automatically generated
     - ``list_variables``: list of variables defined by the user, initialised at |0>; a linking
        function is automatically generated
     - ``link_function_plus_quantum_state``: linking function and initial quantum state defined by
       the user;
     - ``C``: is the command to be evaluated
2. Open the terminal inside the downloaded folder and run ``cabal repl``
3. Load the module Run.hs by executing ``:l Run``
4. To obtain a histogram run ``runHist "path" sch``
  - ``path`` is the path to the file with the programs to be evaluated
  - ``sch`` is a scheduler that needs to be defined inside the file ``KStep.hs``
  - this will print a link in the command line to a webpage, which displays an interactive histogram
5. To obtain results given by the k-step semantics run ``runSem "filename" sch``
  - ``path`` is the path to the file with the programs to be evaluated
  - ``sch`` is a scheduler that needs to be defined inside the file ``KStep.hs``
  - this shows the results obtained for each program inside the file

For example, if we wish to evaluate the commands inside the **prob.txt** file we write 
- ``runHist "./examples/prob.txt"``, to obtain a histogram from each program inside **prob.txt**
- ``runSem "./examples/prob.txt" initSch``, to obtain the evaluation of the k-step semantics from each program inside **prob.txt**

