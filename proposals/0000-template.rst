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

Notes on reStructuredText - delete this section before submitting
==================================================================

The proposals are submitted in reStructuredText format.  To get inline code, enclose text in double backticks, ``like this``.  To get block code, use a double colon and indent by at least one space

::

 like this
 and

 this too

To get hyperlinks, use backticks, angle brackets, and an underscore `like this <http://www.haskell.org/>`_.


Explicit Specificity for Type Applications
==========================================

Currently, the behavior of visible type application (via the ``TypeApplications`` language extension) requires type arguments to be given to a vislbly-type-applied term in an order determined by a stable topological sort on exactly those type and kind variables mentioned by the user in the provided type signature for the aforementioned term. Occasionally, this ordering requirement is counterintuitive, leading to confusing errors and necessarily cluttered source code.

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

While it might initially seem like this is a minor aesthetic concern, this second issue can become arbitrarily burdensome in more complicated examples like the following. Note that while many incorrect "naive" visible applications of the functions below will yield errors rather than silent changes in semantics as above, this need not be the case--similar situations could make it very easy to forget an argument ordering and create confusing errors far away from the problematic application site.

So, consider the following definitions:

::

 data Proxy4 (x :: k) (y :: l) (z :: m) (a :: n) = P4

 g :: forall (d :: x (y (z a)) -> d x y z a). Proxy4 x y z a
 g = P4

 data D (x :: k -> Type) (y :: l -> k) (z :: m -> l) (a :: m) = D (x (y (z a)))

The user of ``g`` might expect that the visible type application ``g @D`` is well-formed, but as we have seen above, it is not! The compiler tells us:

::

• Expecting four more arguments to ‘D’
  Expected a type, but
  ‘D’ has kind
  ‘(k -> *) -> (k1 -> k) -> (k2 -> k1) -> k2 -> *’
• In the type ‘D’
  In the expression: g @D

In order to visibly apply ``f`` to ``D``, we must write ``f @_ @_ @_ @_ @D``.

This issue becomes even more problematic should we wish to explicitly annotate the kind of ``x``, ``y``, ``z``, or ``a`` (perhaps for the purpose of documentation). We might wish we could give ``g``'s type signature as:

::

 g :: forall (d :: x (y (z a)) -> d x y z a) (x :: k -> Type) (y :: l -> k) (z :: m -> l) (a :: m). Proxy4 x y z a

While previously the compiler was happy to let us use ``x``, ``y``, ``z``, and ``a`` without explicitly binding them, we are now given one error for each of these kind-annotated variables, of the form:

::

 Variable ‘x’ used as a kind variable before being bound
 as a type variable. Perhaps reorder your variables?
 the type signature for ‘g’

Thus, in order to provide this more informative type signature, we are forced to write:

::

 g :: forall (x :: k -> Type) (y :: l -> k) (z :: m -> l) (a :: m) (d :: x (y (z a)) -> d x y z a). Proxy4 x y z a

TODO: This currently results in a GHC internal error, and it really should not!! Proceeding below as if this bug is fixed...

With this definition of f, we are now required to give a total of **seven** blank type arguments to ``g`` before we can visibly apply it to ``D``: ``k``, ``l``, ``m``, ``x``, ``y``, ``z``, ``a``:

::

 g @_ @_ @_ @_ @_ @_ @_ @D

Notice that this clutter results from a combination of issues: that implicitly bound kind variables (``k``, ``l``, ``m``) appear before their corresponding type variables due to the topological ordering imposed by visible type application; and that this is compounded by the binding rules requiring us to place all variables which have explicit kind signatures before their use site (but not if they don't have such signatures!).

In this proposal, we introduce a syntax which allows the implementor of ``f`` to explicitly hide unnecessary (always inferrable) type and kind variables from type application, enabling succinctness at use sites and preventing confusing errors. More generally, we show how this feature can be used to arbitrarily permute the argument order for visible type application, if it is desired to reorder without completely hiding some variables.


Proposed Change Specification
-----------------------------
Specify the change in precise, comprehensive yet concise language. Avoid words like should or could. Strive for a complete definition. Your specification may include,

* grammar and semantics of any new syntactic constructs
* the types and semantics of any new library interfaces
* how the proposed change interacts with existing language or compiler features, in case that is otherwise ambiguous

Note, however, that this section need not describe details of the implementation of the feature. The proposal is merely supposed to give a conceptual specification of the new feature and its behavior.


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
(Optional) If accepted who will implement the change? Which other ressources and prerequisites are required for implementation?
