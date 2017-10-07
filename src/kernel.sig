(* This trusted kernel implements *forward inference*, which is reasoning from premises to conclusion. *)
structure TermData = 
struct
  type hyp = int
  type prop = Prop.prop
  type context = Sequent.context

  datatype 'a term = 
     INIT of hyp
   | TRUER
   | FALSEL of hyp
   | CONJR of 'a * 'a
   | CONJL1 of hyp * 'a
   | CONJL2 of hyp * 'a
   | DISJR1 of 'a
   | DISJR2 of 'a
   | DISJL of hyp * 'a * 'a
   | IMPLR of 'a
   | IMPLL of hyp * 'a * 'a
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

  val init : S.context * S.hyp -> proof

  val trueR : S.context -> proof
  val falseL : S.hyp * S.sequent -> proof

  val conjR : proof * proof -> proof
  val conjL1 : S.hyp * proof -> proof
  val conjL2 : S.hyp * proof -> proof

  val disjR1 : proof * P.prop -> proof
  val disjR2 : P.prop * proof -> proof
  val disjL : S.hyp * proof * proof -> proof

  val implR : proof -> proof
  val implL : S.hyp * proof * proof -> proof
end