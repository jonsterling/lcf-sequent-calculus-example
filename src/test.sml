structure Test = 
struct

  structure K = Kernel
  structure R = Refiner
  structure T = Tactic (R)
  structure P = Prop and S = Sequent

  local
    open P T S
    infix then_ thenl
    infixr 0 ===>
    infixr 3 ~>
    infixr 4 /\ \/
  in
    val goal = [`"A" /\ `"B"] ===> `"B" /\ `"A"
    val script = R.conjL1 0 then_ R.conjL2 1 then_ R.conjR thenl [R.init 0, R.init 1]
    val (subgoals, proof) = script goal
    val _ = print "\n\n"
    val _ = print (K.pretty (proof []))
    val _ = print "\n\n"
  end
end
