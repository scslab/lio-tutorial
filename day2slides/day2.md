% Intro to LIO


# Labels in Haskell

* Labels are points on a lattice with
  well defined $\sqsubseteq$, $\sqcap$, and $\sqcup$:

    ~~~~ {.haskell}
    class (Eq l, Show l) => Label l where
        -- Relation that dictates how information flows
        canFlowTo :: l -> l -> Bool
        lub :: l -> l -> l -- Least upper bound
        glb :: l -> l -> l -- Greatest lower bound
    ~~~~

* Example label:

    ~~~~ {.haskell}
    data SimpleLabel = Public | Classified | TopSecret deriving (Eq, Ord, Show)
    
    instance Label SimpleLabel where
      x `canFlowTo` y = x <= y
      lub = max
      glb = min
    ~~~~

# Excercise: SimpleLabel

~~~~
$ git clone https://github.com/scslab/lio-exercises.git
~~~~

or [https://bit.ly/1295qG8](https://bit.ly/1295qG8)

~~~~
*Main> Public `canFlowTo` TopSecret
True
*Main> TopSecret `canFlowTo` Public 
False
*Main> Public `lub` TopSecret
TopSecret
~~~~

<!--# Extending SimpleLabel

* This is called a _total-order_: for every label $L_1$ and $L_2$,
  either $L_1 \sqsubseteq L_2$ or $L_2 \sqsubseteq L_1$

* Extend `SimpleLabel` with compartment:

    ~~~~ {.haskell}
    import Data.Set (Set)

    data Compartment = Nuclear | Crypto deriving (Eq, Ord, Show)

    data MilLabel = MilLabel { sensitivity  :: SimpleLabel
                             , compartments :: Set Compartment
                             } deriving (Eq, Show)
    ~~~~


![](millattice.png)

# Exercise: define the `Label` instance

* Is this lattice a total order?

    * No! Consider `(TopSecret, {Crypto})` and `(TopSecret, {Nuclear})`

* Examples of use:

~~~
*Main> let set x = Data.Set.fromList x
*Main> MilLabel Public (set [Nuclear]) `canFlowTo`
       MilLabel TopSecret (set [Nuclear, Crypto])
True
*Main> MilLabel Public (set [Nuclear]) `canFlowTo` 
       MilLabel Classified (set [Crypto])
False
*Main> MilLabel Classified (set [Crypto]) `canFlowTo`
       MilLabel Public (set [Nuclear])
False
*Main> MilLabel Classified (set [Crypto]) `glb` MilLabel Public (set [Nuclear])
MilLabel {sensitivity = Public, compartments = fromList []}
~~~

* Hint: Start by expressing the lattice properties using set theory

* Use library `Data.Set` vs. set as lists

    ~~~~ {.haskell}
    import Data.Set (Set)
    import qualified Data.Set as Set
    ~~~~


# Answer

* Defining the lattice properties using set theory:

    * Can flow to: $(L_1,C_1) \sqsubseteq (L_2, C_2)$ if $L_1 \sqsubseteq L_2$ and $C_1 \subseteq C_2$
    * Least upper bound: $(L_1,C_1) \sqcup (L_2, C_2) \equiv (L_1 \sqcup L_2, C_1 \cup C_2)$
    * Greatest lower bound: $(L_1,C_1) \sqcap (L_2, C_2) \equiv (L_1 \sqcap L_2, C_1 \cap C_2)$


* Simple translation to Haskell:

    ~~~~ {.haskell}
    instance Label MilLabel where
      (MilLabel s1 c1) `lub` (MilLabel s2 c2) =
        MilLabel (s1 `lub` s2) (c1 `Set.union` c2)

      (MilLabel s1 c1) `glb` (MilLabel s2 c2) =
        MilLabel (s1 `glb` s2) (c1 `Set.intersection` c2)

      (MilLabel s1 c1) `canFlowTo` (MilLabel s2 c2) =
        (s1 `canFlowTo` s2) && (c1 `Set.isSubsetOf` c2)
    ~~~~

* Why you shouldn't use lists?
    * Slow, bug prone: need to reduce to set (even for constructor),  etc.

-->
# LIO privileges

* Privileges allow us to `downgrade`, rather than just upgrade labels in some
  cases
  
* Lets us define a modified version of the `canFlowTo` relation,
  called `canFlowToP`.

    ~~~~ {.haskell}
    canFlowToP myPriv l1 l2
    ~~~~

  1. Downgrade `l1` as much as possible given privilege, `p`

  2. Then check if `l1` `canFlowTo` `l2` normally

* Privileges for `SimpleLabel`:

    ~~~~ {.haskell}
    data SimplePriv = SimplePriv SimpleLabel
    ~~~~

    * Argument is the highest `SimpleLabel` we can downgrade to `Public`

    * E.g., Privilege `TopSecret` can declassify anything, while privilege
      `Public` can declassify nothing

# Exercise

* Define `downgrade`

    ~~~~ {.haskell}
    downgrade (SimplePriv priv) lbl =  ???
    ~~~~

* Example use

    ~~~~
    *Main> downgrade (SimplePriv TopSecret) TopSecret
    SimpleLabel Public
    *Main> downgrade (SimplePriv Public) Classified
    SimpleLabel Classified
    *Main> downgrade (SimplePriv TopSecret) Classified
    SimpleLabel Public
    ~~~~

# Answer: 

~~~~ {.haskell}
downgrade (SimplePriv priv) lbl = 
  if priv >= lbl then Public
    else lbl
~~~~


# Let's define a class for `canFlowToP`

* Privileges are instances of the `PrivDesc` class

    ~~~~ {.haskell}
    class Label l => PrivDesc l p where
      canFlowToP :: p -> l -> l -> Bool
      downgradeP :: p -> l -> l
    ~~~~

* `downgradeP priv lbl`
   
     * Downgrade `lbl` as much as possible using `priv`
     
     * We'll see where this is used later!

* Define instance for `SimpleLabel` and `SimplePriv`

    ~~~~ {.haskell}
    instance PrivDesc SimpleLabel SimplePriv where
      canFlowToP p l1 l2 = (downgradeP p l1) `canFlowTo` l2
      downgradeP (SimplePriv priv) lbl =
        if priv >= lbl then Public
          else lbl
    ~~~~
    

# Example use

~~~
*Main> canFlowToP (SimplePriv TopSecret)
                  (SimpleLabel TopSecret)
                  (SimpleLabel Public)
True
*Main> canFlowToP (SimplePriv TopSecret)
                  (SimpleLabel Classified)
                  (SimpleLabel Public)
True
*Main> canFlowToP (SimplePriv Classified)
                  (SimpleLabel TopSecret)
                  (SimpleLabel Public)
False
~~~

> - Q: What's the problem with relying on `PrivDesc` in security-critical
  code?

> - A: Anybody can create an instance of `PrivDesc`: can't run
    untrusted code!

# Controlling privilege creation

* Really, `PrivDesc` instance _describes_ privileges, but doesn't
  confer any

    * Need to prevent malicious code from synthesizing privileges!

* So, wrap `PrivDesc` in `Priv` and they give you power:

    ~~~~ {.haskell}
    {-# LANGUAGE Unsafe #-}
    newtype Priv a = PrivTCB a deriving (Show, Eq, Typeable)

    privDesc :: Priv a -> a
    privDesc (PrivTCB pd) = pd
    ~~~~

    * Use Safe Haskell to ensure only trusted code sees `PrivTCB` constructor

* Internally, LIO privileged functions expect `Priv` types:

    ~~~~ {.haskell}
    -- Can we allocate object with label l?
    guardAllocP :: PrivDesc l p => Priv p -> l -> LIO l ()
    guardAllocP p l = do
      ...
      unless (canFlowToP (privDesc p) ...) $ throwLIO CurrentLabelViolation
      ...

    ~~~~

# Using labels in Haskell

~~~~ {.haskell}
instance (Label l) => Monad (LIO l) where ...
~~~~

* Introduce new _labeled IO_ monad `LIO`.  Like `RIO`, except it
  carries two labels:

    * _Current label_ restricts (with $\sqsubseteq$) where the thread
      can write to and read from

* Example: current label is `Classified`

    * Can write to objects labeled `Classified` and `TopSecret`,

    * Can read from objects labeled `Public` or `Classified`, but not `TopSecret`.

* Can we safely allow the thread with current label 
    `Public` to read a document labeled 
    `Classified`?

    > - Yes! If we can guarantee that it does not leak such data.

    > - How? Raise the current label to `Classified` -- now thread cannot write
        to objects labeled `Public` anymore.

# Labeling values

* Represent labeled pure values with type wrapper

    ~~~~ {.haskell}
    data Labeled l t = LabeledTCB l t
    ~~~~

* Can label and unlabel values within `LIO` monad:

    ~~~~ {.haskell}
    label :: Label l => l -> a -> LIO l (Labeled l a)
    unlabel :: (Label l) => Labeled l a -> LIO l a
    unlabelP :: Priv l p => p -> Labeled l a -> LIO l a
    labelOf  :: Labeled l a -> l
    ~~~~

    * `label` requires value label to be between current label & clearance
    * `unlabel` raises current label to:  old current label $\sqcup$ value label
    * `unlabelP` uses privileges to raise label less (remember
      `partDowngradePrivDesc`)
    * `labelOf` returns the label of a labeled value

<!--
# Example:

~~~~ {.haskell}
initCurLabel :: LIOState MilLabel
initCurLabel = 
  LIOState { lioLabel = MilLabel Public (set [])
           , lioClearance = MilLabel TopSecret (set [Crypto, Nuclear]) }

main = do
  res <- runLIO mainLIO initCurLabel 
  print res


mainLIO :: LIO MilLabel String
mainLIO = do
  lc <- label (MilLabel Classified (set [Crypto])) "w00t"
  c <- unlabel lc
  lts <- label (MilLabel TopSecret (set [Crypto, Nuclear])) $ 
            c ++ ";cbc-nuke-128"
  ts <- unlabel lts
  -- label (MilLabel TopSecret (set [Nuclear])) $ "leaking...crypto: " ++ ts
  return ts
~~~~

* In practice, want to catch all exception in `main`

* What happens when you uncomment the commented-out line?

* Exercise: what happens when you change clearance to `(TopSecret, {Crypto})`?
-->
    

# DC Labels

* Simple labels are not very powerful

* DCLabels consist of **2** components:

  $L = \langle$Secrecy `%%` Integrity$\rangle$

* Label components & privileges are boolean formulas over *principals*
    * Principals are just strings (might correspond to users)
    * Formulas represented in CNF, without negation
* $\langle S_1$ `%%` $I_1\rangle\sqsubseteq_p
  \ \langle S_2$ `%%` $I_2\rangle$
  iff
    * $S_2 \wedge p \Longrightarrow S_1$, and
    * $I_1 \wedge p \Longrightarrow I_2$ (note reversed order)
* Means you need privileges to weaken S, or to add to I
    * $p=$`True` means no privileges, $p=$`joe` means some privileges
    * $p=$`False` would confer all privileges

* In LIO:

    ~~~~ {.haskell}
    import LIO.DCLabel
    ~~~~ 


# Example of label relations: Secrecy

~~~~ {.haskell}
("alice" \/ "bob" %% True) `canFlowTo`
("alice" \/ "bob" \/ "charlie" %% True)
~~~~

> - False

~~~~ {.haskell}
("alice" \/ "bob" %% True) `canFlowTo`
("alice" /\ "dan" %% True)
~~~~

> - True

~~~~ {.haskell}
("alice" /\ "bob" %% True) `canFlowTo`
("alice" %% True)
~~~~
> - False

# Example of label relations: Integrity

~~~~ {.haskell}
(True %% "alice" \/ "bob") `canFlowTo`
(True %% "alice" \/ "bob" \/ "charlie")
~~~~

> - True

~~~~ {.haskell}
(True %% "alice") `canFlowTo`
(True %% "alice" \/ "bob")
~~~~

> - True

~~~~ {.haskell}
(True %% "alice") `canFlowTo`
(True %% "alice" /\ "bob")
~~~~
> - False

# DCLabel lattice

* Middle of the lattice is called "Public" -- 
  $\langle True$ `%%` $True\rangle$

    * Anyone can read, anyone can write

* Combining differently labeled data (`lub`)
    
    * $\langle S_1$ `%%` $I_1\rangle\sqcup \ \langle S_2$ `%%` $I_2\rangle=$
      $\langle S_1 \land S_2$ `%%` $I_1 \lor I_2\rangle$

    * Need consent of principals in $S_1$ and $S_2$ to observe data
    * Principals of $I_1$ or $I_2$ could have created the data

* Example 1

    ~~~~ {.haskell}
    ("alice" %% "alice") `lub` ("bob" /\ "claire" %% "bob")
    ~~~~

    > - `"alice" /\  "bob" /\ "claire" %% ("alice" \/ "bob")`

* Example 2

    ~~~~ {.haskell}
    ("alice" \/ "bob" %% True) `lub` ("dan" /\ "claire" %% "bob")
    ~~~~

    > - `"claire" /\ "dan" /\ ("alice" \/ "bob") %% True`

<!--
# Example: encoding `MilLabel` with DCLabels

~~~~ {.haskell}
topSecret  = "TopSecret" /\ "Classified" /\ "Public"
classified = "Classified" /\ "Public"
public     = "Public"

crypto  = "Crypto"
nuclear = "Nuclear"
~~~~

* Examples

~~~~ {.haskell}
*Main> (topSecret /\ nuclear /\ crypto %% True) `canFlowTo`
       (topSecret /\ crypto %% True)
~~~~

> - False

~~~~ {.haskell}
*Main> (topSecret /\ crypto %% True) `canFlowTo`
       (topSecret /\ nuclear /\ crypto %% True)
~~~~

> - True

~~~~ {.haskell}
*Main> (topSecret /\ crypto %% True) `canFlowTo`
       (topSecret  %% True)
~~~~

> - False

# DCLabel privileges 

* A privilege description (`DCPrivDesc`) is a label components (`Component`)

    ~~~~ {.haskell}
    type DCPrivDesc = Component
    ~~~~ 

    * No different from secrecy and integrity components

* A privilege is a wrapped description

    ~~~~ {.haskell}
    type DCPriv = Priv DCPrivDesc
    ~~~~ 

* Create privileges with `PrivTCB`:

    ~~~~ {.haskell}
    import LIO.TCB (PrivTCB)

    topSecretPriv = PrivTCB topSecret
    cryptoPriv    = PrivTCB crypto
    ~~~~



# Clearance and DC labels

* Convenient to have different default $L_\mathrm{cur}$ and $C_\mathrm{cur}$
* Set default label $L_\mathrm{def} = L_\emptyset = \langle$True `%%` True$\rangle$
    * Effectively the "public label"
* Set default clearance to $C_\mathrm{def} =\langle$ False `%%` True$\rangle$
    * Allow raising of label to read anything
* Example policy: Everyone can read, only I can export
    * Say my privileges are $p=$`deian`
    * Label object with $L = \langle$ `amit` $\vee$ `deian` `%%` True$\rangle$

    * $p\Longrightarrow$`amit` $\vee$ `deian`, so $L\sqsubseteq_p L_\emptyset$ and I can
      export data

    * Others can taint themselves to read data, but since they don't
      have my privilege, they cannot export it
-->

# What is the LIO Monad?

* Remember the RIO Monad?

    ~~~~ {.haskell}
    newtype RIO a = UnsafeRIO (IO a)
    ~~~~

* RIO statically restricts which actions can be performed (e.g. can only open
  files that match a fixed pattern)

* LIO is similar, but instead carries information about what kinds of actions
  a thread can perform.

    ~~~~ {.haskell}
    data LIOState l = LIOState { lioCurrentLabel :: l, lioClearance :: l }
    newtype LIO l a = UnsafeLIO { unLIO :: IORef (LIOState l) -> IO a }

    instance Monad (LIO l)
    ~~~~

    * Decisions about which side effects to allow are made dynamically

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

# Current Label - Example

~~~~ {.haskell}
advisorNickname :: Labeled DCLabel String
advisorNickname = LabeledTCB (True %% True) "dm"
~~~~

* Say we start running with public current label (True %% True). What's the
  current label after performing the following operations?

    ~~~~ {.haskell}
    -- current label is (True %% True)
    unlabel advisorNickname
    ~~~~

> - (True %% True)

# Current Label - Example

~~~~ {.haskell}
emailAddress :: Labeled DCLabel String
emailAddress = LabeledTCB ("dm" \/ "amit" \/ "deian" %% True)
                "mazieres-smeisvhac56hquuvuqdggqe@nospam.scs.stanford.edu"
~~~~

* Say we start running with public current label (True %% True). What's the
  current label after performing the following operations?

    ~~~~ {.haskell}
    -- current label is (True %% True)
    unlabel email
    ~~~~

> - ("dm" $\vee$ "amit" $\vee$ "deian" %% True)

# Current Label - Example

~~~~ {.haskell}
personalEmail :: Labeled DCLabel String
personalEmail = LabeledTCB ("dm" %% True) "iwishihadthis@scs.stanford.edu"
~~~~

~~~~ {.haskell}
-- current label is ("dm" \/ "amit" \/ "deian" %% True)
unlabel personalEmail
~~~~

> - ("dm" $\wedge$ "dm" $\vee$ "amit" $\vee$ "deian" %% True)

> - ("dm" %% True)

# Current Label - Bonus Round!

~~~~ {.haskell}
personalEmail :: Labeled DCLabel String
personalEmail = LabeledTCB ("dm" %% True) "iwishihadthis@scs.stanford.edu"

dmPriv :: DCPriv
dmPriv = PrivTCB $ toCNF "dm"
~~~~

* Say we start running with public current label (True %% True). What's the
  current label after performing the following operations?

    ~~~~ {.haskell}
    -- current label is (True %% True)
    unlabelP dmPriv personalEmail
    ~~~~

> - In last example was: ("dm" %% True)

> - Because of privileges: (True %% True)

# Clearance

~~~~ {.haskell}
getClearance :: Label l => LIO l l
~~~~

* Limits how high the current label can get

    * Both writes and reads must be below the clearance

* Allows "need-to-know" policies

    * Restricts the power of covert channels

* Can lower clearance to label, but raising requires privileges

# Clearance Example

~~~~ {.haskell}
personalEmail :: Labeled DCLabel String
personalEmail = LabeledTCB ("dm" %% True) "dm@scs.stanford.edu"
~~~~

* What happens if we try to read `personalEmail` when our clearance is lower?

    ~~~~ {.haskell}
    setClearance ("amit" %% True)
    unlabel personalEmail
    ~~~~

> - Without clearance restriction was: ("dm" %% True)

> - But ("dm" %% True) `canFlowTo` ("amit" %% True) == False

> - So we get "Exception: ClearanceViolation"

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
    guardWrite lref = do
      taint lref
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
      secretVar <- newEmptyLMVarP allPrivTCB ("alice" %% True)
      forkLIO $ do
        taint $ "alice" %% True
        putLMVar secretVar "Please do not share"

      forkLIO $ do
        taint $ "bob" %% True
        logP bobPriv "I'll wait for a message from Alice"
        secret <- takeLMVarP bobPriv secretVar
        logP bobPriv secret -- This will fail!
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

* Delegating DC privileges:

    ~~~~ {.haskell}
    module LIO.DCLabel.Privs
    dcDelegatePriv :: DCPriv -> DCPrivDesc -> Maybe DCPriv
    ~~~~

* [`LObj`][`LObj`] - associates an IO object with a label

    * Can use `blessTCB` to turn an `IO` action into an `LIO` action.
      `guardWrite` on the `LObj`'s label

    ~~~~ {.haskell}
    blessTCB :: Label l => (a -> IO b) -> LObj l a -> LIO l b 
    ~~~~

# Miscellany Continued

* [`LIORef`s][`LIORef`] - labeled, mutable values

    ~~~~ {.haskell}
    data LIORef l a
    newLIORefP :: PrivDesc l p => Priv p -> l -> a -> LIO l (LIORef l a)
    readLIORefP :: PrivDesc l p => Priv p -> LIORef l a -> LIO l a
    writeLIORefP :: PrivDesc l p => Priv p -> LIORef l a -> a -> LIO l ()
    ~~~~

* [`Gate`s][`Gate`] prove to untrusted code that you own a privilege

# Defining labeled objects

~~~~
$ git clone https://github.com/scslab/lio-exercises.git
~~~~

or [https://bit.ly/1295qG8](https://bit.ly/1295qG8)

# Defining labeled objects

~~~ {.haskell}
{-# LANGUAGE Trustworthy #-}

module ToyLIO where
import safe qualified System.IO as IO
import safe LIO
import safe LIO.DCLabel
import LIO.TCB.LObj

type Handle = LObj DCLabel IO.Handle

hPutStrLnP :: DCPriv -> Handle -> String -> LIO DCLabel ()
hPutStrLnP = blessPTCB IO.hPutStrLn

hPutStrLn :: Handle -> String -> LIO DCLabel ()
hPutStrLn = blessTCB IO.hPutStrLn

stdout :: Handle
stdout = LObjTCB (True %% True) IO.stdout

myPriv :: DCPriv
myPriv = PrivTCB (True %% True)

test = evalDC $ hPutStrLnP myPriv stdin "Hello world"
~~~

# Main exercise: LIO multi-player game

~~~~
$ git clone https://github.com/scslab/lio-exercises.git
~~~~

or [https://bit.ly/1295qG8](https://bit.ly/1295qG8)

* `NetLib.hs` -- trusted (labeled) network library

      `{-# LANGUAGE Trustworthy #-}`

* `liorock.hs` -- main 

* Probably also:

    * `Common.hs` -- `{-# LANGUAGE Safe #-}` (`Move`, `outcome`, etc.)

    * `Play.hs` -- `{-# LANGUAGE Safe #-}`

* Goal: Make it impossible for third-party translations of game (that
  substitute alternate `Play.hs`) to cheat

[`LIORef`]: http://hackage.haskell.org/packages/archive/lio/latest/doc/html/LIO-LIORef.html
[`LMVar`]: http://hackage.haskell.org/packages/archive/lio/latest/doc/html/LIO-Concurrent-LMVar.html
[`LObj`]: http://hackage.haskell.org/packages/archive/lio/latest/doc/html/LIO-TCB-LObj.html
[`Gate`]: http://hackage.haskell.org/packages/archive/lio/latest/doc/html/LIO-Privs.html#t:Gate

