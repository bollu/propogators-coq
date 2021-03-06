(** This is me scribbling down kmett's thoughts over stream - Could be
totally innacurate.  - Useful to have a Coq file because `hs-to-coq`
is a little annoying.
*) Class JoinSemiLattice T : Type := { join: T -> T -> T; bottom : T; top: T;


    join_assoc: forall (t1 t2 t3: T), (join (join t1 t2) t3) = (join t1 (join t2 t3));

    
    join_commute: forall (t1 t2: T), join t1 t2 = join t2 t1;

    join_idempotent: forall (t: T), (join t t) = t;

    bottom_unit: forall (t: T), join bottom t = t; top_absorbent: forall (t: T), join top t = top;
    
  }.

(* Naive propogation *) (*

For every semilattice with ACC / finite height, propogators terminate
with deterministic solution.

*)

(* Datalog as propogator *) (* Datalog life lessons:

- We don't need ACC.  Topsort allows us to do less work.  Having a
- good way to perform "delta"/ "derivatives" can reduce cost.  *)

Require Import FMapList.

Module Datalog.  Variable K: Type.  Variable T: Type.

    Module MAP := FMapList.Make.  End Datalog.

(* CRDT's - convergent, replicated, data types Networking /
    distriubuted systems idea, seems like LVars Join the states with
    the semilattice operation to "combine".

    Because of CAP, we need to pick between consistency, availability,
    and partition tolerance.

    We really must have partition tolerance, so the real choice is
    between C/A.

    We usually pick AP, and then perform "eventual consistency":
    eventually stuff works out.

   We can have strong eventual consistency, which is when the network
   heals the partition, we are in sync in one timestep.

    https://en.wikipedia.org/wiki/Conflict-free_replicated_data_type
   CMRDT provides a way to send "deltas"

  Sid: Seems familiar to OT as well -
    https://en.wikipedia.org/wiki/Operational_transformation Another
    useful blogpost that does a deep dive:
    https://medium.com/@raphlinus/towards-a-unified-theory-of-operational-transformation-and-crdt-70485876f72f

    Darcs, theory of patches:
    https://en.wikibooks.org/wiki/Understanding_Darcs/Patch_theory

   CRDTs show us how to distribute propogators over the network,
   and allows us to exploit CAP.

 *)

(* Next, LVish / Par monad:
http://hackage.haskell.org/package/monad-par

- some semilattice of values.  NewLVar :: Par (LVar a) WriteLVar ::
- LVar a -> a -> Par (LVar a) fork :: Par a -> Par ()


Filter - upward closed set.  Filter :: JoinSemilattice A => Set A →
Prop.  Filter F = ∀ (a b: A), a ∈ F ∧ a < b → b ∈ F.

- ReadLVar :: LVar a -> Filter -> Par Bool (* threshold read *)

 *)

(* Any function in LVish *must* be monotone?

Every propogator in LVish is a valid propogator Something that cannot
be written in LVish: - Interval arithmetic with intersection of
intervals as join.  *)

(* Using Abstract interpretation / widening for approximating
propogators? (Open) *)

(**

Propogator network for addition: (a, b) -> (+) -> c (c, a) -> (-) -> b
(c, b) -> (-) -> a *)

(* This propogator network allows us to identify the third value given
any of the other two values.

This is important because there are "multiple" output nodes, so the
order of evaluation matters. *)

(**

If we have something that needs to be iterative, we can try to extend
Par with the propogation theory.  *)


(** We can decompose a propogator over the product A x B,

we can decompose a single propogator into a "state machine" of
multiple propogators which are sequenced within Par.

eg.

ReadIVar :: IVar a -> Par a IVar a = Bottom | Exact (a)

Note that the lattice is "flat". ie, bottom is below everything, and
that's it.  So, we can read trivially, by having our filter set be one
element sets. So, we can create "exact reads".

and :: IVar Bool -> IVar Bool -> IVar Bool -> Par () and a b c = fork
part1 >> fork part2 where part1 = readIVar a >>= \case False ->
writeIVarC false True -> readIVar b >>= writeIVar c part2 = readIVar b
>>= \case False -> writeIVar c False _ -> return () *)

(* Now, steal ideas from SAT:
https://www.princeton.edu/~chaff/zchaff.html

Put stuff in CNF, so we get (a \/ b \/ c ...) /\ (p \/ q \/ r ..) /\


The propogator network behaves like this: Note, each clause is a cell
in the propogator network

unit propogation ---------------- if ANY of a, b, c is TRUE, then the
total clause is TRUE And if we have something like: a = false b =
false then we can propogate that c = true (since it must be, for this
solution to work.)


2-watch-literal scheme ---------------------
http://www.cs.utexas.edu/users/moore/acl2/seminar/2008.12.10-swords/sat.pdf

When any variable changes, we wake up all the clause variables to see
if we have only one variable left, to perform unit prop.

So, what we do is to watch for two variables per clause which have not
been assigned values.  Wake up when the two variables that are being
watched are written to.

CDCL (conflict driven clause learning) ---- Once we have finished unit
propogation, now we need to guess.

https://en.wikipedia.org/wiki/Conflict-Driven_Clause_Learning

We add guesses, and see what happens.  *)

(*

A propogator which has *all* of its values right below Top is
done/satisfied/cannot gain more information. So, we can use this to
"stop watching" such a propogator. (1-watch literal on a propogator, and
just stop watching). This is some kind of GC for propogators.

*)






(*
Finite domain solving: AC3
https://en.wikipedia.org/wiki/AC-3_algorithm

Arc consistency - constraint solving algorithm, covered in Peter Norvig.

We want to rule out things that can be assigned.

eg.
x ∈ [1..9]
y ∈ [1..9]
x  < y

This immediately tells us that:
¬ (x < 9), ¬ (y < 1)

Later on, if we learn that (x = 3), then we know that
y ∈ [4, 9]
 *)

(*
Linear programming / simplex:
- Solution is at a vertex, so we can jump between vertices
- Interior point methods as well, that start from the inside and move up a potential
  to find the vertex.
- We can cut away regions that do not matter, with respect to a
  lattice of regions all of which preserves a point.
- LP as a valid propogator problem? (I do not understand this )

- ILP as LP + gomory cuts. We can view the ILP style gomory cut
technique as a propogator problem? (still slightly unclear to me), I
assume we can look at it as something that happens in the Par context.
We can do MILP + finite domain / constraint solving on the _same_
variables?  This is huge if true.

 *)


