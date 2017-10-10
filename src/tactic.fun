structure ListUtil :
sig
  val splitAt : 'a list * int -> 'a list * 'a list
end =
struct
  local
    fun go _ [] = ([], [])
      | go 1 (x::xs) = ([x], xs)
      | go m (x::xs) =
        let val
          (xs', xs'') = go (m - 1) xs
        in
          (x::xs', xs'')
        end
  in
    fun splitAt (ls, n) =
      if n < 0 then
        raise Subscript
      else
        if n = 0 then ([], ls)
      else
        go n ls
  end
end

functor Tactic (R : REFINER) : TACTIC = 
struct
  open R

  type tactic = goal -> subgoals * validation

  fun gobbleWith ([], []) args = []
    | gobbleWith (n :: ns, f :: fs) args = 
      let
        val (xs, args') = ListUtil.splitAt (args, n)
      in
        f xs :: gobbleWith (ns, fs) args'
      end

  fun stitchProof (validation, subgoalss, validations) =
    (List.concat subgoalss,
      validation o
        gobbleWith (map length subgoalss, validations))


  fun then_ (t1, t2) goal =
    let
      val (subgoals, validation) = t1 goal
      val (subgoalss, validations) = ListPair.unzip (List.map t2 subgoals)
    in
      stitchProof (validation, subgoalss, validations)
    end

  fun thenl (t, ts) goal =
    let
      val (subgoals, validation) = t goal
      val (subgoalss, validations) = ListPair.unzip (ListPair.mapEq (fn (t, g) => t g) (ts, subgoals))
    in
      stitchProof (validation, subgoalss, validations)
    end

  fun orelse_ (t1, t2) (goal : goal) =
    t1 goal handle _ => t2 goal

  fun id goal = ([goal], fn [d] => d)
  fun fail _ = raise Fail "Fail!"
  fun complete t = then_ (t, fail)

  fun hole msg goal = 
    (print (msg ^ ":   " ^ R.prettyGoal goal);
     id goal)
end