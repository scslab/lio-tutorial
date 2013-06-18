% Intro to LIO
% Amit Levy and Deian Stefan
%

# `LIO` Monad

* Let's define `Label`s as points on a lattice
    (type with $\sqsubseteq$, $\sqcap$, and $\sqcup$)

    ~~~~ {.haskell}
    class (Eq l, Show l) => Label l where
        lub :: l -> l -> l
        glb :: l -> l -> l
        canFlowTo :: l -> l -> Bool
    ~~~~

* Define privileges as any type with $\sqsubseteq_p$ for a given label
      type

    ~~~~ {.haskell}
    class Label l => PrivDesc l p where
        canFlowToPrivDesc :: p -> l -> l -> Bool
        partDowngradePrivDesc :: p -> l -> l -> l
        -- use p to get as close to second l as possible from first
    ~~~~

    * But how to prevent malicious code from synthesizing privileges?

    * Really, `PrivDesc` instance _describes_ privileges, but doesn't
      confer any

    * Wrap them in type `Priv` and they give you power:

    ~~~~ {.haskell}
    newtype Priv a = PrivTCB a deriving (Show, Eq, Typeable)
    ~~~~

    * Use Safe Haskell to ensure only trusted code sees `PrivTCB` constructor

# Using labels in Haskell

~~~~ {.haskell}
instance (Label l) => Monad (LIO l) where ...
~~~~

* Introduce new _labeled IO_ monad `LIO`.  Like `RIO`, except:
    * Also keeps track of thread's _current label_ and _current clearance_

* Represent labeled pure values with type wrapper

    ~~~~ {.haskell}
    data Labeled l t = LabeledTCB l t
    ~~~~

* Can label and unlabel values within `LIO` monad:

    ~~~~ {.haskell}
    label :: Label l => l -> a -> LIO l (Labeled l a)
    unlabel :: (Label l) => Labeled l a -> LIO l a
    unlabelP :: Priv l p => p -> Labeled l a -> LIO l a
    ~~~~

    * `label` requires value label to be between current label & clearance
    * `unlabel` raises current label to:  old current label $\sqcap$ value label
    * `unlabelP` uses privileges to raise label less

# What is the LIO Monad?

~~~~ {.haskell}
data LIOState l = LIOState { lioCurrentLabel :: l, lioClearance :: l }
data LIO l a = UnsafeLIO { unLIO :: IORef (LIOState l) -> IO a }
~~~~

# Current Label

~~~~ {.haskell}
getLabel :: Label l => LIO l l
~~~~

* Represents the label of executing thread at a point in time

* Restricts which data we can read and what side-effects we can perform

    * Can write if current label `canFlowTo` target's label

    * Can read if target's label `canFlowTo` current label

* Certain operations (e.g. `unlabel`) change the current label

    * For example, to read a `Labeled` value with a high label, I must raise
      my current label.

# Clearance

~~~~ {.haskell}
getClearance :: Label l => LIO l l
~~~~

* Limits how high the current label can get

    * Both writes and reads must be below the clearance

* Allows "need-to-know" policies

    * Restricts the power of covert channels

* Can lower clearance to label, but raising requires privileges

# `Labeled` Values under the hood

* `Labeled` values respect the current labal and clearance of a thread:

    ~~~~ {.haskell}
    data Labeled l a = LabeledTCB l a

    label :: Label l => l -> v -> LIO l (Labeled l v)
    label l a = guardAlloc >> return $ LabeledTCB l v
      where guardAlloc = do
              currentLabel <- getLabel
              clearance <- getClearance
              unless (canFlowTo currentLabel l) $! throwLIO CurrentLabelViolation
              unless (canFlowTo l clearance) $! throwLIO ClearanceViolation

    unlabel :: Label l => Labeled l v -> LIO l v
    unlabel (LabeledTCB l v) = taint >> return v
      where taint = do
              currentLabel <- getLabel
              clearance <- getClearance
              unless (canFlowTo l clearance) $! throwLIO ClearanceViolation
              setLabelTCB (l `lub` currentLabel)
    ~~~~

# Concurrency - [`LMVar`s][`LMVar`]

  * Labeled version of `MVar`s with fixed label

    ~~~~ {.haskell}
    module LIO.Concurrent.LMVar

    data LMVar l v = LMVarTCB l (MVar v)

    newEmptyLMVar :: l -> LIO l (LMVar l a)

    putLMVar :: Label l => LMVar l v -> -> v -> LIO l ()
    putLMVar (LMVarTCB l mvar) v = guardWrite l >> ioTCB $ IO.putMVar mvar v

    takeLMVar :: Label l => LMVar l v -> LIO l v
    takeLMVar (LMVar l mvar) = guardWrite l >> ioTCB $ IO.takeMVar mvar

    guardWrite :: Label l => l LIO l ()
    guardWrite l = do
      taint l
      currentLabel <- getLabel
      clearance <- getClearance
      unless (canFlowTo currentLabel lref) $! throwLIO CurrentLabelViolation
      unless (canFlowTo lref clearance) $! throwLIO ClearanceViolation
    ~~~~

# Concurrency - Threads

~~~~ {.haskell}
module LIO.Concurrent
forkLIO :: LIO l () -> LIO l ()
~~~~

* Can use threads to compute things at different labels  
  (e.g., interact with multiple web sites before combining the data)

* `LMVars` used to synchronize and share data between threads

    ~~~~ {.haskell}
    liomain :: LIO DCLabel ()
    liomain = do
      secretVar <- newEmptyLMVar ("alice" %% True)
      forkLIO $ do
        taint $ "alice" %% True
        putLMVar secretVar "Please do not share"

      forkLIO $ do
        taint $ "bob" %% True
        logP bobPriv "I'll wait for a message from Alice"
        secret <- takeLMVar passwordVar
        logP bobPriv password -- This will fail!
    ~~~~

# Miscellany

* Remeber privileges?

    ~~~~ {.haskell}
    labelP :: PrivDesc l p => Priv p -> a -> LIO l (Labeled a)
    ulabelP :: PrivDesc l p => Priv p -> Labeled a -> LIO l a
    putMVarP :: PrivDesc l p => Priv p -> LMVar l a -> a -> LIO l ()
    takeLMVarP :: PrivDesc l p => Priv p -> LMVar l a -> LIO l a
    ~~~~

    * Haskell's abstractions at work - curry privilege into first argument of
      function

* [`LIORef`s][`LIORef`] - labeled, mutable values

# DC Labels

* We would ideally like $L_\emptyset$ to be in middle of lattice
* Define labels as consisting of **2** components
  $L = \langle$Secrecy `%%` Integrity$\rangle$
* Label components & privileges are boolean formulas over *principals*
    * Represent as minimal formulas in CNF, without negation
    * Makes labels unique, operations decidable
* $\langle S_1$ `%%` $I_1\rangle\sqsubseteq_p
  \ \langle S_2$ `%%` $I_2\rangle$
  iff
    * $S_2 \wedge p \Longrightarrow S_1$, and
    * $I_1 \wedge p \Longrightarrow I_2$ (note reversed order)
* Means you need privileges to weaken S, or to add to I
    * $p=$`True` means no privileges, $p=$`"David"` means some privileges
    * $p=$`False` would confer all privileges
* Note disjunctive clauses in CNF formulas called
    _categories_

# What is a principal?

* Principals are just strings
    * E.g., might correspond to users or web sites

* Also have pseudo-principals starting with `#`
    * By convention, system never grants privileges starting `#`
    * Pseudo-principals let you subdivide privileges
    * Example: $\texttt{dm}\vee\texttt{#friends}$
      ($\texttt{dm}\Longrightarrow\texttt{dm}\vee\texttt{#friends}$,
      but not vice versa)


# Clearance and DC labels

* Convenient to have different default $L_\mathrm{cur}$ and $C_\mathrm{cur}$
* Set default label $L_\mathrm{def} = L_\emptyset = \langle\emptyset$ 
  `%%` $\emptyset\rangle = \langle$True `%%` True$\rangle$
* Set default clearance to $C_\mathrm{def} =\langle$`#clearance %%` True$\rangle$
* Example policy: Everyone can read, only I can export
    * Say my privileges are $p=$`dm`
    * Label object with $L = \langle S$ `%%` True$\rangle$
     where $S = $`dm` $\vee$ `#clearance`$\rangle$

    * $p\Longrightarrow S$, so $L\sqsubseteq_p L_\emptyset$ and I can
      export data

    * Also $S\Longrightarrow$ `#clearance`, so $L\sqsubseteq
      C_\mathrm{def}$--i.e., others can taint themselves to read data,
      but not export it

# Defining labeled objects

* Create `NetLib.hs`, play with variations on `test`:

    ~~~ {.haskell}
    {-# LANGUAGE Trustworthy #-}

    module NetLib where
    import Network (PortID(..), HostName, PortNumber)
    import qualified Network as IO
    import safe qualified System.IO as IO

    import safe LIO
    import safe LIO.DCLabel
    import LIO.TCB.LObj

    type Handle = LObj DCLabel IO.Handle

    hPutStrLnP :: PrivDesc l p =>
       Priv p -> LObj l IO.Handle -> String -> LIO l ()
    hPutStrLnP = blessPTCB IO.hPutStrLn

    hPutStrLn :: Label l => LObj l IO.Handle -> String -> LIO l ()
    hPutStrLn = blessTCB IO.hPutStrLn

    hGetLine :: Label l => LObj l IO.Handle -> LIO l String
    hGetLine h = blessTCB IO.hGetLine h

    test = evalDC $ hGetLine (LObjTCB (True %% True) IO.stdin)
    ~~~

# Main exercise: LIO multi-player game

* `NetLib.hs` -- `{-# LANGUAGE Trustworthy #-}`
* `Common.hs` -- `{-# LANGUAGE Safe #-}` (`Move`, `outcome`, etc.)
* `Play.hs` -- `{-# LANGUAGE Safe #-}`
* `liorock.hs` (mainly IO code)

* Goal: Make it impossible for third-party translations of game (that
  substitute alternate `Play.hs`) to cheat

* Label each socket with the host/port number, as follows

    ~~~ {.haskell}
    type Socket = LObj DCLabel IO.Socket

    acceptP :: DCPriv -> Socket -> DC (Handle, Principal)
    acceptP p s = do
      (ioh, name, port) <- blessPTCB IO.accept p s
      let net = principal $ "tcp://" ++ name ++ ":" ++ show port
          label = dcLabel (privDesc p \/ net) (privDesc p \/ net)
      guardAllocP p label
      let h = LObjTCB label ioh
      hSetBufferingP p h IO.LineBuffering
      return (h, net)
    ~~~ 


[`LIORef`]: http://hackage.haskell.org/packages/archive/lio/latest/doc/html/LIO-LIORef.html
[`LMVar`]: http://hackage.haskell.org/packages/archive/lio/latest/doc/html/LIO-Concurrent-LMVar.html
