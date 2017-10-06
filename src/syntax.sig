structure SyntaxData = 
struct
  datatype prop = 
     TRUE
   | FALSE
   | /\ of prop * prop
   | \/ of prop * prop
   | ~> of prop * prop

  type context = prop list
  type hyp = int

  datatype sequent =
    ===> of prop list * prop
end

signature PROP = 
sig
  datatype prop = datatype SyntaxData.prop
  val pretty : int -> prop -> string
end

signature SEQUENT = 
sig
  type context = SyntaxData.context
  type hyp = int

  datatype sequent = datatype SyntaxData.sequent
  val pretty : sequent -> string
end