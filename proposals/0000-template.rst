.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_. **After creating the pull request, edit this file again, update the number in the link, and delete this bold sentence.**

.. contents::


Explicit Specificity for Type Applications
==========================================

Currently, the behavior of visible type application (via the ``TypeApplications`` language extension) requires type arguments to be given to a vislbly-type-applied term in an order determined by a stable topological sort on exactly those type and kind variables mentioned by the user in the provided type signature for the aforementioned term. Occasionally, this ordering requirement is counterintuitive, leading to confusing non-local errors and necessarily cluttered source code.

We propose to add new syntax to universal quantifiers which allows the implementor of a function intended to be type-applied to "hide" certain type arguments from visible type application. At every type-application site of a function so defined, all such hidden type arguments need not (and indeed cannot) be visibly instantiated by the caller. The intention is that the implementor will make use of this option when they anticipate that these hidden type arguments will always be inferrable at use sites from a combination of the non-hidden visibly specified type arguments and value arguments.


Motivation
------------

Suppose we have the following definitions:

::

 data Proxy (a :: k) = P

 f :: forall a. Proxy a
 f = P

The user can visibly apply ``f`` to some type argument such as ``Int`` to instantiate ``a``: ``f @Int`` is well-typed.

Now consider the variation ``f'``:

::

 f' :: forall (a :: k). Proxy a

At a first glance, the user of ``f`` might expect that they can still visibly apply ``f'`` to ``Int``. However, in this case, the meaning of this expression has silently shifted:

::

 f  @Int :: Proxy Int
 f' @Int :: Proxy a

That is, while the user **thought** they were specifying the type of ``a``, they were actually specifying the **kind** of ``a``!

If we turn on ``-fprint-explicit-kinds`` in ghci, we can observe more closely what's going on:

::

 > :t f @Int
 f @Int :: Proxy * Int

 > :t f' @Int
 f' @Int :: Proxy Int a

Notice that in the former case, the compiler knows that ``Int`` is of kind ``*``, so it is able to infer ``k``, which means that there is no ambiguity in the type of ``f @Int``; the same cannot be said of the type of ``f' @Int``.

Why this silent shift in semantics? We return to the specification of visible type application: the order of arguments for visible type application is determined by a stable topological sort on all explicitly user-written type variables in the signature of the function. To be more precise: if some variable ``k`` appears in the user-written kind signature of some variable ``a``, then ``k`` comes before ``a``, ``a`` depends on ``k``, so ``k`` must be applied before ``a``. The order of arguments proceeds by dependency "level".

Thus, of ``f'`` behaves identically to this even-more-explicit ``f''``:

::

 f'' :: forall k (a :: k). Proxy a

As a further illustrative example, the **actual** order of arguments to visibly instantiate a quantifier ``forall (x :: k) (y :: l).`` is ``k l x y``. This is because ``x`` and ``y`` depend on ``k`` and ``l`` respectively, and there are no other dependencies: so ``x`` and ``y`` share a level of the topological sort, as do ``k`` and ``l``. Within each level, the variables occur in the left-to-right order as the user wrote them.

Arguably, this behavior is sensible in the majority of cases [NOTE: SCW believes otherwise, and I am coming around to this position also], but in situations like the above, it is counterintuitive, and can lead to very mysterious type errors, potentially an arbitrary distance away from the incorrectly-ordered type application site.

Another consideration is that when users **do** realize that an additional argument needs to come first, it is necessary for them to supply it at every application site, even if it can always be inferred. In order to use ``f'`` in the same way as ``f``, users must write ``f' @_ @T`` for every type ``T :: K`` with an inferrable kind ``K``.

While it might initially seem like this is a minor concern, this second issue can become arbitrarily burdensome in more complicated examples like the following.

Consider the following definitions:

::

 data Proxy (x :: k) = P

 g :: forall (c :: x (y (z a)) -> d). Proxy d
 g = P

 data D where
   C :: x (y (z a)) -> D

The user of ``g`` might expect that the visible type application ``g @C`` is well-formed, but as we have seen above, it is not! The compiler tells us:

::

    • Expected kind ‘x0 (y0 (z0 a0)) -> *’,
        but ‘C’ has kind ‘x0 (y0 (z0 a0)) -> D’
    • In the type ‘C’
      In the expression: g @C
      In an equation for ‘it’: it = g @C

In order to visibly apply ``g`` to ``C``, we must write ``g @_ @_ @_ @_ @_ @C``, which has the type we would expect: ``Proxy D``.

This issue becomes even more problematic should we wish to explicitly annotate the kind of ``x``, ``y``, ``z``, or ``a`` (perhaps for the purpose of documentation). We might wish we could give this type signature to ``g``:

::

 g :: forall (c :: x (y (z a)) -> d) (d :: Type) (x :: k -> Type) (y :: l -> k) (z :: m -> l) (a :: l) . Proxy d

While previously the compiler was happy to let us use ``x``, ``y``, ``z``, and ``a`` without explicitly binding them, we are now given one error for each of these variables:

::

 error: Not in scope: type variable ‘x’
 error: Not in scope: type variable ‘y’
 error: Not in scope: type variable ‘z’
 error: Not in scope: type variable ‘a’
 error: Not in scope: type variable ‘d’

Thus, in order to provide this more informative type signature, we are forced to write:

::

 g :: forall (x :: k -> Type) (y :: l -> k) (z :: m -> l) (a :: m) (c :: (x (y (z a))) -> d). Proxy d

With this definition of f, we are now required to give a total of **eight** blank type arguments to ``g`` before we can visibly apply it to ``C``: ``k``, ``l``, ``m``, ``x``, ``y``, ``z``, ``a``, ``d``:

::

 g @_ @_ @_ @_ @_ @_ @_ @_ @C

Notice that this clutter results from a combination of issues: that implicitly bound kind variables (``k``, ``l``, ``m``) appear before their corresponding type variables due to the topological ordering imposed by visible type application; and that this is compounded by the binding rules requiring us to place all variables which have explicit kind signatures before their use site (but not if they don't have such signatures!).

In this proposal, we introduce a syntax which allows the implementor of ``f`` to explicitly hide unnecessary (presumed always inferrable) type and kind variables from type application, enabling succinctness at use sites and preventing confusing errors. More generally, we show how this feature can be used to arbitrarily permute the argument order for visible type application, if it is desired to reorder without completely hiding some variables.


Proposed Change Specification
-----------------------------

We propose to implement the following new user-facing syntax in forall-binders on terms and datatype constructors.

In any explicit forall, it will be permissible to surround any argument in curly braces, to denote that this argument is to be excluded from visible type application at all use sites. For example, consider the following definitions:

::

 data Tagged k x (t :: k) = T x

 f :: forall {k} (t :: k) x. x -> Tagged k x t
 f = T

The type of ``f @True`` is ``forall x. Tagged Bool True x``.

We also allow kind annotations in curly-brace binders, as below:

::

 g :: forall {k :: Type} (t :: k). (Proxy k, Proxy t)
 g = (P, P)

The type of ``g @True`` is ``(Proxy Bool, Proxy True)``

Likewise, this change extends to the definition of datatypes, notably GADTs. Consider the following definition:

::

 data D :: (Type -> Type) -> Type -> Type where
   C :: forall {x} f. (x -> Bool) -> f x -> D f

The type of ``C @[]`` is ``forall x. (x -> Bool) -> [x] -> D []``. (Note that in this case, this proposed syntax is not strictly necessary to achieve this effect; we may freely reorder ``x`` and ``f`` in the forall-binder of ``C`` because they are not at different levels of the topological order on dependency.)

We claim that this is a conservative extension of GHC's grammar, as curly braces cannot appear anywhere in a forall-binder at present.

If the implementor of some function or datatype wishes not to **hide** some argument entirely from visible application but merely to place it later (or, without loss of generality, earlier) in the order of arguments, this is immediately possible using only this syntax.

Suppose we wish to make a polykinded proxy which can be visibly applied to both the type and the kind of its parameter, but which takes as a visible argument the type first and the kind second. That is, we wish to create something equivalent to the (illegal) definition:

::

 data FlipProxy (a :: k) k = FP

We can do this as follows:

::

 data FlipProxy a where
   FP :: forall {k} (a :: k) k1. (k ~ k1) => FlipProxy a k1

This means that ``FP @True @Bool`` has type ``FlipProxy True Bool``, whereas ``FlipProxy True ()`` is ill-typed--the exact semantics we wanted to achieve.

What's happening above? We initially hide the variable ``k`` from visible type application, but it is still brought into scope and so can be used to name the kind of ``a``. We then add an additional type argument ``k1`` which **is** visible to type application, and immediately constrain it to be equal to ``k``, forcing them to unify and make the result have type ``FlipProxy a k``.

Similarly, we can arbitrarily permute the visible application order for any dependent telescope, whether in a function or a datatype constructor. In general, we can take an arbitrary forall-binder and permute its arguments by the following procedure:

1. Add explicit bindings for every implicitly quantified variable (i.e. convert ``(a :: k)`` into ``k (a :: k)`` if no explicit binding for ``k`` exists).
2. Convert every non-hidden variable binding into a hidden one, preserving kind annotations if such exist.
3. Add N new visible bindings with a fresh name for each (no need for any kind signatures), where N is the number of names bound in the signature after [1].
4. Add equality constraints between the new and old names so as to form a the desired permutation.

As an example, consider how we might do this for the data constructor ``C`` from the datatype ``D``, seen earlier.

::

 data D where
   C :: x (y (z a)) -> D

 g :: forall (c :: x (y (z a)) -> d). Proxy d

Here is how we transform the type signature of ``g`` according to the procedure above, step by step:

1. ``g :: forall x y z a d (c :: x (y (z a)) -> d). Proxy d``
2. ``g :: forall {x} {y} {z} {a} {d} {c :: x (y (z a)) -> d}. Proxy d``
3. ``g :: forall {x} {y} {z} {a} {d} {c :: x (y (z a)) -> d} c' x' y' z' a' d'. Proxy d``
4. ``g :: forall {x} {y} {z} {a} {d} {c :: x (y (z a)) -> d} c' x' y' z' a' d'. (c ~ c', x ~ x', y ~ y', z ~ z', a ~ a', d ~ d') => Proxy d``

Note that while this general solution always exists, in non-pathological cases there is usually a way to permute the bound variables without introducing N new names and equality constraints.

Effect and Interactions
-----------------------

Detail how the proposed change addresses the original problem raised in the motivation.

Discuss possibly contentious interactions with existing language or compiler features.


Costs and Drawbacks
-------------------

Give an estimate on development and maintenance costs. List how this effects learnability of the language for novice users. Define and list any remaining drawbacks that cannot be resolved.


Alternatives
------------

List existing alternatives to your proposed change as they currently exist and discuss why they are insufficient.


Unresolved questions
--------------------

Explicitly list any remaining issues that remain in the conceptual design and specification. Be upfront and trust that the community will help. Please do not list *implementation* issues.

Hopefully this section will be empty by the time the proposal is brought to the steering committee.


Implementation Plan
-------------------

I will implement this plan, under the supervision of Richard Eisenberg and Stephanie Weirich.
