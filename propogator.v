Module JoinSemiLattice.
  Variable T: Type.
  Variable join: T -> T -> T.
  Variable bottom : T.
  Variable top: T.

  Infix "<+>" := join (at level 50).

  Axiom join_assoc: forall (t1 t2 t3: T),
      (t1 <+> t2) <+> t3 = t1 <+> (t2 <+> t3).

  Axiom join_commute: forall (t1 t2: T),
      t1 <+> t2 = t2 <+> t1.

  Axiom join_idempotent: forall (t1 t2 : T),
      (t1 <+> t1) = t1.

  Axiom bottom_unit: forall (t: T),
      bottom <+> t = t.

  Axiom top_absorbent: forall (t: T),
      top <+> t = t.
      
      
      
End JoinSemiLattice.