Module Type JoinSemiLattice.
  Variable T: Type.
  Variable join: T -> T -> T.
  Variable bottom : T.
  Variable top: T.

  Infix "<+>" := join (at level 50).

  Axiom join_assoc: forall (t1 t2 t3: T),
      (t1 <+> t2) <+> t3 = t1 <+> (t2 <+> t3).

  Axiom join_commute: forall (t1 t2: T),
      t1 <+> t2 = t2 <+> t1.

  Axiom join_idempotent: forall (t : T),
      (t <+> t) = t.

  Axiom bottom_unit: forall (t: T),
      bottom <+> t = t.

  Axiom top_absorbent: forall (t: T),
      top <+> t = top.

  Definition lt (small large: T): Prop :=
    exists t, large = small <+> t.

  Infix "<<" := lt (at level 50).

  (* Formulate what it means to have ACC
   informaklly, in each chain, the joins will stabilise*)
  Definition lattice_acc : Prop := True. 
End JoinSemiLattice.

Module SmallestJoinSemiLattice.
  Inductive BotOrTop := Bottom | Top.
  Definition T := BotOrTop.

  Definition join (t1 t2: BotOrTop): BotOrTop :=
    match t1 with
    | Bottom => t2
    | Top => Top
    end.

  Infix "<+>" := join (at level 50).

  Lemma join_assoc: forall (t1 t2 t3: T),
      (t1 <+> t2) <+> t3 = t1 <+> (t2 <+> t3).
  Proof.
    intros.
    destruct t1, t2, t3; auto.
  Qed.

  Lemma join_commute: forall (t1 t2: T),
      t1 <+> t2 = t2 <+> t1.
  Proof.
    destruct t1, t2; auto.
  Qed.

  Lemma join_idempotent: forall (t1: T),
      (t1 <+> t1) = t1.
  Proof.
    destruct t1; auto.
  Qed.

  Definition bottom := Bottom.
  Definition top := Top.

  
  Definition bottom_unit: forall (t: T),
      bottom <+> t = t.
  Proof.
    destruct t; auto.
  Qed.

  Definition top_absorbent: forall (t: T),
      top <+> t = top.
  Proof.
    destruct t; auto.
  Qed.
End SmallestJoinSemiLattice.




