structure TermData = 
struct
  type hyp = int
  type prop = Prop.prop
  type context = Sequent.context

  datatype 'a term = 
     HYP of hyp
   | UNIT
   | ABORT of 'a
   | PAIR of 'a * 'a
   | FST of 'a
   | SND of 'a
   | INL of 'a
   | INR of 'a
   | CASE of 'a * ('a * 'a)
   | LAM of 'a
   | APP of 'a * 'a
end

signature KERNEL = 
sig
  (* The kernel implements *forward inference*, which is reasoning from premises to conclusion. Forward inference
     is captured most cleanly via natural deduction. *)
  structure P : PROP
  structure S : SEQUENT

  type proof

  datatype term = datatype TermData.term
  val out : proof -> proof term
  val pretty : proof -> string

  (* We will give a "final" encoding of natural deduction proofs. *)

  val infer : proof -> S.sequent
  val check : S.sequent -> proof -> unit

  (* instantiate the first hypothesis; "push (m,n)" means [m]*n  *)
  val push : proof * proof -> proof

  val hyp : S.context * S.hyp -> proof

  val unit : S.context -> proof
  val abort : proof * P.prop -> proof

  val pair : proof * proof -> proof
  val fst : proof -> proof
  val snd : proof -> proof

  val inl : proof * P.prop -> proof
  val inr : P.prop * proof -> proof
  val case_ : proof * (proof * proof) -> proof

  val lam : proof -> proof
  val app : proof * proof -> proof
end