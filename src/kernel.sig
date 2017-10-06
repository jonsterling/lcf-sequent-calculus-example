(* The kernel implements *forward inference*, which is reasoning from premises to conclusion.
    Sequent calculus works best in *backward inference*, so for the kernel, we use natural
    deduction terms as proofs. Check out the REFINER module, which implements backward inference,
    to see how sequent rules are developed; the validations of the refinement/backward inference rules
    there witness the soundness of sequent calculus for natural dedution. *)

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

  structure P : PROP
  structure S : SEQUENT

  datatype term = datatype TermData.term

  (* We have an _ABSTRACT_ type of proofs; the only way to get one of these proofs is to 
     use the functions in this module, which are guaranteed to only produce correct
     proofs. *)
  type proof

  (* However, the proof can be inspected through the "term" view; the transformation goes only one way, though, so
     you cannot fabricate a proof by building a term. *)
  val out : proof -> proof term
  val pretty : proof -> string

  val infer : proof -> S.sequent
  val check : S.sequent -> proof -> unit

  (* instantiate the first hypothesis  *)
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