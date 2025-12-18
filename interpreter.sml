structure Interpreter =
struct
  val NotFound = Fail "not found"
  val Unreachable = Fail "unreachable"
  val NotComparable = Fail "not comparable"

  datatype value =
    Unit
  | Int of int
  | Real of real
  | Bool of bool
  | Str of string
  | List of value list
  | Tuple of value list
  | Record of (string * value) list
  | Constructor of string * value option
  | Func of {params: string list, body: Ast.expr, env: context}

  and context =
    Context of
      {bindings: (string, value) HashTable.hash_table, parent: context option}

  fun valueToString Unit = "()"
    | valueToString (Int n) = Int.toString n
    | valueToString (Real r) = Real.toString r
    | valueToString (Bool b) = Bool.toString b
    | valueToString (Str s) = s
    | valueToString (List values) =
        "[" ^ String.concatWith ", " (List.map valueToString values) ^ "]"
    | valueToString (Tuple values) =
        "(" ^ String.concatWith ", " (List.map valueToString values) ^ ")"
    | valueToString (Record pairs) =
        "{ "
        ^
        String.concatWith ", "
          (List.map (fn (name, value) => name ^ " : " ^ valueToString value)
             pairs) ^ " }"
    | valueToString (Constructor (name, SOME value)) = name ^ "(" ^ valueToString value ^ ")"
    | valueToString (Constructor (name, NONE)) = name
    | valueToString (Func _) = "<func>"

  fun isValuesEqual (Int n1, Int n2) = n1 = n2
    | isValuesEqual (Real r1, Real r2) = Real.== (r1, r2)
    | isValuesEqual (Bool b1, Bool b2) = b1 = b2
    | isValuesEqual (Str s1, Str s2) = s1 = s2
    | isValuesEqual (List [], List []) = true
    | isValuesEqual (List [], List (_ :: nil)) = false
    | isValuesEqual (List (_ :: nil), List []) = false
    | isValuesEqual (List (x :: xs), List (y :: ys)) =
        isValuesEqual (x, y) andalso isValuesEqual (List xs, List ys)
    | isValuesEqual (Tuple [], Tuple []) = true
    | isValuesEqual (Tuple [], Tuple (_ :: nil)) = false
    | isValuesEqual (Tuple (_ :: nil), Tuple []) = false
    | isValuesEqual (Tuple (x :: xs), Tuple (y :: ys)) =
        isValuesEqual (x, y) andalso isValuesEqual (List xs, List ys)
    | isValuesEqual (Record vals1, Record vals2) =
        let
          fun compare (name, value) =
            let
              val otherVal = List.find (fn (name', _) => name = name') vals2
            in
              case otherVal of
                NONE => false
              | SOME (_, value') => isValuesEqual (value, value')
            end
        in
          List.all compare vals1
        end
    | isValuesEqual (Constructor (_, NONE), (Constructor (_, SOME _))) = false
    | isValuesEqual (Constructor (_, SOME _), (Constructor (_, NONE))) = false
    | isValuesEqual (Constructor (n1, NONE), (Constructor (n2, NONE))) = n1 = n2
    | isValuesEqual (Constructor (n1, SOME v1), (Constructor (n2, SOME v2))) = n1 = n2 andalso isValuesEqual (v1, v2)
    | isValuesEqual _ = raise NotComparable

  fun mkContext parent =
    Context
      { bindings = HashTable.mkTable (HashString.hashString, op=) (42, NotFound)
      , parent = parent
      }

  fun getBindingValue (Context ctx) name =
    let
      fun loop (Context ctx') =
        let
          val t = HashTable.find (#bindings ctx') name
        in
          case t of
            SOME t' => t'
          | NONE =>
              case #parent ctx' of
                SOME c => loop c
              | NONE => raise (Fail ("variable " ^ name ^ " is unbound"))
        end
    in
      loop (Context ctx)
    end

  fun addBinding (Context ctx) (name, value) =
    let val () = HashTable.insert (#bindings ctx) (name, value)
    in value
    end

  fun evalExpr _ Ast.Unit = Unit
    | evalExpr _ (Ast.Int n) = Int n
    | evalExpr _ (Ast.Real r) = Real r
    | evalExpr _ (Ast.Bool b) = Bool b
    | evalExpr _ (Ast.Str s) = Str s
    | evalExpr ctx (Ast.List exprs) =
        let val values = List.map (evalExpr ctx) exprs
        in List values
        end
    | evalExpr ctx (Ast.Tuple exprs) =
        let val values = List.map (evalExpr ctx) exprs
        in Tuple values
        end
    | evalExpr ctx (Ast.Record exprs) =
        let
          val values =
            List.map (fn (name, value) => (name, evalExpr ctx value)) exprs
        in
          Record values
        end
    | evalExpr ctx (Ast.Var v) = getBindingValue ctx v
    | evalExpr ctx (Ast.Let (decls, expr)) =
        let
          val ctx' = mkContext (SOME ctx)
          val _ = List.map (evalDecl ctx') decls
        in
          evalExpr ctx' expr
        end
    | evalExpr ctx (Ast.If (clause, t, e)) =
        let
          val clauseVal = evalExpr ctx clause
        in
          case clauseVal of
            Bool true => evalExpr ctx t
          | Bool false => evalExpr ctx e
          | _ => raise Unreachable
        end
    | evalExpr ctx (Ast.BinaryExpr (Ast.Equal, e1, e2)) =
        let
          val val1 = evalExpr ctx e1
          val val2 = evalExpr ctx e2
        in
          Bool (isValuesEqual (val1, val2))
        end
    | evalExpr ctx (Ast.BinaryExpr (Ast.NotEq, e1, e2)) =
        let
          val val1 = evalExpr ctx e1
          val val2 = evalExpr ctx e2
        in
          Bool (not (isValuesEqual (val1, val2)))
        end
    | evalExpr ctx (Ast.BinaryExpr (Ast.OrElse, e1, e2)) =
        let
          val val1 = evalExpr ctx e1
        in
          case val1 of
            Bool false => evalExpr ctx e2
          | Bool true => Bool true
          | _ => raise Unreachable
        end
    | evalExpr ctx (Ast.BinaryExpr (Ast.AndAlso, e1, e2)) =
        let
          val val1 = evalExpr ctx e1
        in
          case val1 of
            Bool false => Bool false
          | Bool true => evalExpr ctx e2
          | _ => raise Unreachable
        end
    | evalExpr ctx (Ast.BinaryExpr (Ast.Less, e1, e2)) =
        let
          val val1 = evalExpr ctx e1
          val val2 = evalExpr ctx e2
        in
          case (val1, val2) of
            (Int n1, Int n2) => Bool (n1 < n2)
          | (Real r1, Real r2) => Bool (r1 < r2)
          | _ => raise Unreachable
        end
    | evalExpr ctx (Ast.BinaryExpr (Ast.LessEq, e1, e2)) =
        let
          val val1 = evalExpr ctx e1
          val val2 = evalExpr ctx e2
        in
          case (val1, val2) of
            (Int n1, Int n2) => Bool (n1 <= n2)
          | (Real r1, Real r2) => Bool (r1 <= r2)
          | _ => raise Unreachable
        end
    | evalExpr ctx (Ast.BinaryExpr (Ast.Greater, e1, e2)) =
        let
          val val1 = evalExpr ctx e1
          val val2 = evalExpr ctx e2
        in
          case (val1, val2) of
            (Int n1, Int n2) => Bool (n1 > n2)
          | (Real r1, Real r2) => Bool (r1 > r2)
          | _ => raise Unreachable
        end
    | evalExpr ctx (Ast.BinaryExpr (Ast.GreaterEq, e1, e2)) =
        let
          val val1 = evalExpr ctx e1
          val val2 = evalExpr ctx e2
        in
          case (val1, val2) of
            (Int n1, Int n2) => Bool (n1 >= n2)
          | (Real r1, Real r2) => Bool (r1 >= r2)
          | _ => raise Unreachable
        end
    | evalExpr ctx (Ast.BinaryExpr (Ast.Cons, e1, e2)) =
        let
          val val1 = evalExpr ctx e1
          val val2 = evalExpr ctx e2
        in
          case (val1, val2) of
            (hd, List tl) => List (hd :: tl)
          | _ => raise Unreachable
        end
    | evalExpr ctx (Ast.BinaryExpr (Ast.Concat, e1, e2)) =
        let
          val val1 = evalExpr ctx e1
          val val2 = evalExpr ctx e2
        in
          case (val1, val2) of
            (List v1, List v2) => List (v1 @ v2)
          | _ => raise Unreachable
        end
    | evalExpr ctx (Ast.BinaryExpr (Ast.Add, e1, e2)) =
        let
          val val1 = evalExpr ctx e1
          val val2 = evalExpr ctx e2
        in
          case (val1, val2) of
            (Int n1, Int n2) => Int (n1 + n2)
          | (Real r1, Real r2) => Real (r1 + r2)
          | _ => raise Unreachable
        end
    | evalExpr ctx (Ast.BinaryExpr (Ast.Subtract, e1, e2)) =
        let
          val val1 = evalExpr ctx e1
          val val2 = evalExpr ctx e2
        in
          case (val1, val2) of
            (Int n1, Int n2) => Int (n1 - n2)
          | (Real r1, Real r2) => Real (r1 - r2)
          | _ => raise Unreachable
        end
    | evalExpr ctx (Ast.BinaryExpr (Ast.StrConcat, e1, e2)) =
        let
          val val1 = evalExpr ctx e1
          val val2 = evalExpr ctx e2
        in
          case (val1, val2) of
            (Str s1, Str s2) => Str (s1 ^ s2)
          | _ => raise Unreachable
        end
    | evalExpr ctx (Ast.BinaryExpr (Ast.Multiply, e1, e2)) =
        let
          val val1 = evalExpr ctx e1
          val val2 = evalExpr ctx e2
        in
          case (val1, val2) of
            (Int n1, Int n2) => Int (n1 * n2)
          | (Real r1, Real r2) => Real (r1 * r2)
          | _ => raise Unreachable
        end
    | evalExpr ctx (Ast.BinaryExpr (Ast.Divide, e1, e2)) =
        let
          val val1 = evalExpr ctx e1
          val val2 = evalExpr ctx e2
        in
          case (val1, val2) of
            (Real r1, Real r2) => Real (r1 / r2)
          | _ => raise Unreachable
        end
    | evalExpr ctx (Ast.BinaryExpr (Ast.IntDiv, e1, e2)) =
        let
          val val1 = evalExpr ctx e1
          val val2 = evalExpr ctx e2
        in
          case (val1, val2) of
            (Int n1, Int n2) => Int (n1 div n2)
          | _ => raise Unreachable
        end
    | evalExpr ctx (Ast.BinaryExpr (Ast.Modulo, e1, e2)) =
        let
          val val1 = evalExpr ctx e1
          val val2 = evalExpr ctx e2
        in
          case (val1, val2) of
            (Int n1, Int n2) => Int (n1 mod n2)
          | _ => raise Unreachable
        end
    | evalExpr ctx (Ast.BinaryExpr (Ast.Apply, e1, e2)) =
        let
          val lhsVal = evalExpr ctx e1
        in
          case lhsVal of
            Tuple values =>
              let val index = Ast.getIntValue e2
              in List.nth (values, index)
              end
          | Record pairs =>
              let
                val field = Ast.getVarValue e2
                val value = List.find (fn (name, _) => name = field) pairs
              in
                case value of
                  NONE =>
                    raise (Fail ("Field " ^ field ^ " not defined on record"))
                | SOME v => #2 v
              end
          | Func {params = [], body, env} =>
              evalExpr (mkContext (SOME env)) body
          | Func {params = p :: ps, body, env} =>
              let
                val rhsVal = evalExpr ctx e2
                val newEnv = mkContext (SOME env)
                val _ = addBinding newEnv (p, rhsVal)
              in
                if null ps then evalExpr newEnv body
                else Func {params = ps, body = body, env = newEnv}
              end
            | Constructor (name, NONE) => Constructor (name, SOME (evalExpr ctx e2))
          | _ => raise Unreachable
        end
    | evalExpr ctx (Ast.UnaryExpr (Ast.Plus, e)) =
        let
          val v = evalExpr ctx e
        in
          case v of
            Int n => Int n
          | Real r => Real r
          | _ => raise Unreachable
        end
    | evalExpr ctx (Ast.UnaryExpr (Ast.Minus, e)) =
        let
          val v = evalExpr ctx e
        in
          case v of
            Int n => Int (~n)
          | Real r => Real (~r)
          | _ => raise Unreachable
        end
    | evalExpr ctx (Ast.UnaryExpr (Ast.Not, e)) =
        let
          val v = evalExpr ctx e
        in
          case v of
            Bool b => Bool (not b)
          | _ => raise Unreachable
        end

  and evalDecl ctx (Ast.ValDecl (name, expr, _)) =
        addBinding ctx (name, evalExpr ctx expr)
    | evalDecl ctx (Ast.FunDecl (name, params, expr, _)) =
        let
          val func = Func {params = List.map #1 params, body = expr, env = ctx}
        in
          addBinding ctx (name, func)
        end
    | evalDecl _ (Ast.TypeAlias _) = Unit
    | evalDecl ctx (Ast.DataType (_, branches)) =
        let
          fun bindConstructor (name, _) =
            addBinding ctx (name, Constructor (name, NONE))
          val _ = List.map bindConstructor branches
        in
          Unit
        end

  fun evalProgram (Ast.Program decls) =
    let
      val ctx = mkContext NONE
      fun eval decl =
        let val value = evalDecl ctx decl
        in value
        end
    in
      List.map eval decls
    end

  fun interpret file =
    let
      val program = Parser.parse file
      val () = Ast.printAst program

      val types = Typing.typeCheckProgram program
      val () = List.app (fn v => print (v ^ "\n"))
        (List.map Typing.typeToString types)

      val values = evalProgram program
      val () = List.app (fn v => print (v ^ "\n"))
        (List.map valueToString values)
    in
      ()
    end
end
