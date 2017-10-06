signature KERNEL = 
sig
  (* The kernel implements *forward inference*, which is reasoning from premises to conclusion. Forward inference
     is captured most cleanly via natural deduction. *)
  structure P : PROP
  structure S : SEQUENT

  type proof

  (* We will give a "final" encoding of natural deduction proofs. *)

  val infer : S.context -> proof -> P.prop
  val check : S.sequent -> proof -> unit

  (* instantiate the first hypothesis; "push (m,n)" means [m]*n  *)
  val push : proof * proof -> proof

  val hyp : S.hyp -> proof

  val unit : proof
  val abort : proof * P.prop -> proof

  val pair : proof * proof -> proof
  val fst : proof -> proof
  val snd : proof -> proof

  val inl : proof * P.prop -> proof
  val inr : P.prop * proof -> proof
  val case_ : proof * (proof * proof) -> proof

  val lam : P.prop * proof -> proof
  val app : proof * proof -> proof
end