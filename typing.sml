structure Typing:
sig
  datatype label = Field of string | Index of int

  datatype typ =
    Unit
  | Int
  | Real
  | Bool
  | Str
  | List of typ
  | Tuple of typ list
  | Record of (string * typ) list
  | RecordSelector of label
  | DataType of string * (string * typ option) list
  | Func of typ * typ

  val typeToString: typ -> string

  val typeCheckProgram: Ast.program -> typ list
end =
struct
  datatype label = Field of string | Index of int

  datatype typ =
    Unit
  | Int
  | Real
  | Bool
  | Str
  | List of typ
  | Tuple of typ list
  | Record of (string * typ) list
  | RecordSelector of label
  | DataType of string * (string * typ option) list
  | Func of typ * typ

  fun typeToString Unit = "unit"
    | typeToString Int = "int"
    | typeToString Real = "real"
    | typeToString Bool = "bool"
    | typeToString Str = "string"
    | typeToString (List ty) = typeToString ty ^ " list"
    | typeToString (Tuple types) =
        String.concatWith " * " (List.map typeToString types)
    | typeToString (Record pairs) =
        "{ "
        ^
        String.concatWith ", "
          (List.map (fn (name, ty) => name ^ " : " ^ typeToString ty) pairs)
        ^ " }"
    | typeToString (DataType (name, _)) = name
    | typeToString (Func (a, b)) =
        typeToString a ^ " -> " ^ typeToString b
    | typeToString (RecordSelector (Field f)) = "#" ^ f
    | typeToString (RecordSelector (Index i)) =
        "#" ^ (Int.toString i)

  fun isTypeEqual (Unit, Unit) = true
    | isTypeEqual (Int, Int) = true
    | isTypeEqual (Real, Real) = true
    | isTypeEqual (Bool, Bool) = true
    | isTypeEqual (Str, Str) = true
    | isTypeEqual (List ty1, List ty2) = isTypeEqual (ty1, ty2)
    | isTypeEqual (Tuple ty1, Tuple ty2) =
        let
          fun zip ([], _) = []
            | zip (_, []) = []
            | zip (x :: xs, y :: ys) =
                (x, y) :: zip (xs, ys)

          val l1 = List.length ty1
          val l2 = List.length ty2
          val types = if l1 <> l2 then NONE else SOME (zip (ty1, ty2))
        in
          case types of
            NONE => false
          | SOME ts => List.all isTypeEqual ts
        end
    | isTypeEqual (Record p1, Record p2) =
        let
          fun compare (name, ty) =
            let
              val s = List.find (fn (name', _) => name = name') p2
            in
              case s of
                NONE => false
              | SOME (_, ty') => isTypeEqual (ty, ty')
            end
        in
          List.all compare p1
        end
    | isTypeEqual (DataType (n1, _), DataType (n2, _)) = n1 = n2
    | isTypeEqual (Func (a1, b1), Func (a2, b2)) =
        isTypeEqual (a1, a2) andalso isTypeEqual (b1, b2)
    | isTypeEqual _ = false

  val Unbound = Fail "variable is not bound"
  val UnknownType = Fail "type is not defined"

  datatype context =
    Context of
      { bindings: (string, typ) HashTable.hash_table
      , types: (string, typ) HashTable.hash_table
      , parent: context option
      }

  fun mkContext parent =
    Context
      { bindings = HashTable.mkTable (HashString.hashString, op=) (42, Unbound)
      , types = HashTable.mkTable (HashString.hashString, op=) (42, UnknownType)
      , parent = parent
      }

  fun getBindingType (Context ctx) name =
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

  fun getType (Context ctx) name =
    let
      fun loop (Context ctx') =
        let
          val t = HashTable.find (#types ctx') name
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

  fun addBinding (Context ctx) (name, typ) =
    let val () = HashTable.insert (#bindings ctx) (name, typ)
    in typ
    end

  fun addType (Context ctx) (name, typ) =
    let val () = HashTable.insert (#types ctx) (name, typ)
    in typ
    end

  fun astTyToType _ Ast.UnitTy = Unit
    | astTyToType _ Ast.IntTy = Int
    | astTyToType _ Ast.RealTy = Real
    | astTyToType _ Ast.BoolTy = Bool
    | astTyToType _ Ast.StringTy = Str
    | astTyToType ctx (Ast.ListTy ty) =
        List (astTyToType ctx ty)
    | astTyToType ctx (Ast.TupleTy types) =
        Tuple (List.map (astTyToType ctx) types)
    | astTyToType ctx (Ast.RecordTy pairs) =
        Record (List.map (fn (name, ty) => (name, astTyToType ctx ty)) pairs)
    | astTyToType ctx (Ast.FuncTy (a, b)) =
        Func (astTyToType ctx a, astTyToType ctx b)
    | astTyToType (ctx: context) (Ast.VarTy t) = getType ctx t

  fun typeCheckExpr _ Ast.Unit = Unit
    | typeCheckExpr _ (Ast.Int _) = Int
    | typeCheckExpr _ (Ast.Real _) = Real
    | typeCheckExpr _ (Ast.Bool _) = Bool
    | typeCheckExpr _ (Ast.Str _) = Str
    | typeCheckExpr ctx (Ast.List exprs) =
        let
          val types = List.map (typeCheckExpr ctx) exprs
          val listTy = List.hd types
          val allSame = List.all (fn ty => isTypeEqual (ty, listTy)) types
        in
          if allSame then List listTy
          else raise (Fail "all expression's types must be same in the list")
        end
    | typeCheckExpr ctx (Ast.Sequence exprs) =
        let val types = List.map (typeCheckExpr ctx) exprs
        in List.nth (types, length types - 1)
        end
    | typeCheckExpr ctx (Ast.Tuple exprs) =
        let val types = List.map (typeCheckExpr ctx) exprs
        in Tuple types
        end
    | typeCheckExpr ctx (Ast.Record pairs) =
        let
          val types =
            List.map (fn (name, expr) => (name, typeCheckExpr ctx expr)) pairs
        in
          Record types
        end
    | typeCheckExpr ctx (Ast.Var v) = getBindingType ctx v
    | typeCheckExpr ctx (Ast.Let (decls, expr)) =
        let
          val ctx' = mkContext (SOME ctx)
          val _ = List.map (typeCheckDecl ctx') decls
        in
          typeCheckExpr ctx' expr
        end
    | typeCheckExpr ctx (Ast.If (clause, t, e)) =
        let
          val clauseTy = typeCheckExpr ctx clause
          val thenTy = typeCheckExpr ctx t
          val elseTy = typeCheckExpr ctx e
        in
          case (clauseTy, thenTy, elseTy) of
            (Bool, t1, t2) =>
              if isTypeEqual (t1, t2) then
                t1
              else
                raise
                  (Fail
                     ("if branches don't have the same types "
                      ^ typeToString thenTy ^ " " ^ typeToString elseTy))
          | (_, _, _) =>
              raise
                (Fail
                   ("if clause must be boolean expression but received "
                    ^ typeToString clauseTy))
        end
    | typeCheckExpr ctx (Ast.BinaryExpr (Ast.Equal, e1, e2)) =
        let
          val ty1 = typeCheckExpr ctx e1
          val ty2 = typeCheckExpr ctx e2
        in
          case (ty1, ty2) of
            (Int, Int) => Bool
          | (Real, Real) => Bool
          | (Bool, Bool) => Bool
          | (Str, Str) => Bool
          | (List t1, List t2) =>
              if isTypeEqual (t1, t2) then Bool else raise (Fail "zort")
          | (Tuple _, Tuple _) =>
              if isTypeEqual (ty1, ty2) then Bool else raise (Fail "zort")
          | (Record _, Record _) =>
              if isTypeEqual (ty1, ty2) then Bool else raise (Fail "zort")
          | _ =>
              raise
                (Fail
                   ("lhs value type " ^ typeToString ty1
                    ^ " doesn't match with rhs value type " ^ typeToString ty2))
        end
    | typeCheckExpr ctx (Ast.BinaryExpr (Ast.NotEq, e1, e2)) =
        let
          val ty1 = typeCheckExpr ctx e1
          val ty2 = typeCheckExpr ctx e2
        in
          case (ty1, ty2) of
            (Int, Int) => Bool
          | (Real, Real) => Bool
          | (Bool, Bool) => Bool
          | (Str, Str) => Bool
          | (List t1, List t2) =>
              if isTypeEqual (t1, t2) then Bool else raise (Fail "zort")
          | (Tuple _, Tuple _) =>
              if isTypeEqual (ty1, ty2) then Bool else raise (Fail "zort")
          | (Record _, Record _) =>
              if isTypeEqual (ty1, ty2) then Bool else raise (Fail "zort")
          | _ =>
              raise
                (Fail
                   ("lhs value type " ^ typeToString ty1
                    ^ " doesn't match with rhs value type " ^ typeToString ty2))
        end
    | typeCheckExpr ctx (Ast.BinaryExpr (Ast.OrElse, e1, e2)) =
        let
          val ty1 = typeCheckExpr ctx e1
          val ty2 = typeCheckExpr ctx e2
        in
          case (ty1, ty2) of
            (Bool, Bool) => Bool
          | _ =>
              raise
                (Fail
                   ("operations requires boolean expression but found "
                    ^ typeToString ty1 ^ " " ^ typeToString ty2))
        end
    | typeCheckExpr ctx (Ast.BinaryExpr (Ast.AndAlso, e1, e2)) =
        let
          val ty1 = typeCheckExpr ctx e1
          val ty2 = typeCheckExpr ctx e2
        in
          case (ty1, ty2) of
            (Bool, Bool) => Bool
          | _ =>
              raise
                (Fail
                   ("operations requires boolean expression but found "
                    ^ typeToString ty1 ^ " " ^ typeToString ty2))
        end
    | typeCheckExpr ctx (Ast.BinaryExpr (Ast.Less, e1, e2)) =
        let
          val ty1 = typeCheckExpr ctx e1
          val ty2 = typeCheckExpr ctx e2
        in
          case (ty1, ty2) of
            (Int, Int) => Bool
          | (Real, Real) => Bool
          | _ =>
              raise
                (Fail
                   ("lhs value type " ^ typeToString ty1
                    ^ " doesn't match with rhs value type " ^ typeToString ty2))
        end
    | typeCheckExpr ctx (Ast.BinaryExpr (Ast.LessEq, e1, e2)) =
        let
          val ty1 = typeCheckExpr ctx e1
          val ty2 = typeCheckExpr ctx e2
        in
          case (ty1, ty2) of
            (Int, Int) => Bool
          | (Real, Real) => Bool
          | _ =>
              raise
                (Fail
                   ("lhs value type " ^ typeToString ty1
                    ^ " doesn't match with rhs value type " ^ typeToString ty2))
        end
    | typeCheckExpr ctx (Ast.BinaryExpr (Ast.Greater, e1, e2)) =
        let
          val ty1 = typeCheckExpr ctx e1
          val ty2 = typeCheckExpr ctx e2
        in
          case (ty1, ty2) of
            (Int, Int) => Bool
          | (Real, Real) => Bool
          | _ =>
              raise
                (Fail
                   ("lhs value type " ^ typeToString ty1
                    ^ " doesn't match with rhs value type " ^ typeToString ty2))
        end
    | typeCheckExpr ctx (Ast.BinaryExpr (Ast.GreaterEq, e1, e2)) =
        let
          val ty1 = typeCheckExpr ctx e1
          val ty2 = typeCheckExpr ctx e2
        in
          case (ty1, ty2) of
            (Int, Int) => Bool
          | (Real, Real) => Bool
          | _ =>
              raise
                (Fail
                   ("lhs value type " ^ typeToString ty1
                    ^ " doesn't match with rhs value type " ^ typeToString ty2))
        end
    | typeCheckExpr ctx (Ast.BinaryExpr (Ast.Cons, e1, e2)) =
        let
          val ty1 = typeCheckExpr ctx e1
          val ty2 = typeCheckExpr ctx e2
        in
          case (ty1, ty2) of
            (elem, List listTy) =>
              if isTypeEqual (elem, listTy) then
                List listTy
              else
                raise
                  (Fail
                     ("lhs value type " ^ typeToString elem
                      ^ "is different from list type " ^ typeToString listTy))
          | _ =>
              raise
                (Fail
                   ("cons operation requires rhs to be list but received "
                    ^ typeToString ty2))
        end
    | typeCheckExpr ctx (Ast.BinaryExpr (Ast.Concat, e1, e2)) =
        let
          val ty1 = typeCheckExpr ctx e1
          val ty2 = typeCheckExpr ctx e2
        in
          case (ty1, ty2) of
            (List t1, List t2) =>
              if isTypeEqual (t1, t2) then
                List t1
              else
                raise
                  (Fail
                     ("list types are different " ^ typeToString t1 ^ " and "
                      ^ typeToString ty2))
          | _ =>
              raise
                (Fail
                   ("concat operation requires both values to be list but received "
                    ^ typeToString ty1 ^ " and " ^ typeToString ty2))
        end
    | typeCheckExpr ctx (Ast.BinaryExpr (Ast.Add, e1, e2)) =
        let
          val ty1 = typeCheckExpr ctx e1
          val ty2 = typeCheckExpr ctx e2
        in
          case (ty1, ty2) of
            (Int, Int) => Int
          | (Real, Real) => Real
          | _ =>
              raise
                (Fail
                   ("lhs value type " ^ typeToString ty1
                    ^ " doesn't match with rhs value type " ^ typeToString ty2))
        end
    | typeCheckExpr ctx (Ast.BinaryExpr (Ast.Subtract, e1, e2)) =
        let
          val ty1 = typeCheckExpr ctx e1
          val ty2 = typeCheckExpr ctx e2
        in
          case (ty1, ty2) of
            (Int, Int) => Int
          | (Real, Real) => Real
          | _ =>
              raise
                (Fail
                   ("lhs value type " ^ typeToString ty1
                    ^ " doesn't match with rhs value type " ^ typeToString ty2))
        end
    | typeCheckExpr ctx (Ast.BinaryExpr (Ast.StrConcat, e1, e2)) =
        let
          val ty1 = typeCheckExpr ctx e1
          val ty2 = typeCheckExpr ctx e2
        in
          case (ty1, ty2) of
            (Str, Str) => Str
          | _ =>
              raise
                (Fail
                   ("lhs value type " ^ typeToString ty1
                    ^ " doesn't match with rhs value type " ^ typeToString ty2))
        end
    | typeCheckExpr ctx (Ast.BinaryExpr (Ast.Multiply, e1, e2)) =
        let
          val ty1 = typeCheckExpr ctx e1
          val ty2 = typeCheckExpr ctx e2
        in
          case (ty1, ty2) of
            (Int, Int) => Int
          | (Real, Real) => Real
          | _ =>
              raise
                (Fail
                   ("lhs value type " ^ typeToString ty1
                    ^ " doesn't match with rhs value type " ^ typeToString ty2))
        end
    | typeCheckExpr ctx (Ast.BinaryExpr (Ast.Divide, e1, e2)) =
        let
          val ty1 = typeCheckExpr ctx e1
          val ty2 = typeCheckExpr ctx e2
        in
          case (ty1, ty2) of
            (Real, Real) => Real
          | _ =>
              raise
                (Fail
                   ("lhs value type " ^ typeToString ty1
                    ^ " doesn't match with rhs value type " ^ typeToString ty2))
        end
    | typeCheckExpr ctx (Ast.BinaryExpr (Ast.IntDiv, e1, e2)) =
        let
          val ty1 = typeCheckExpr ctx e1
          val ty2 = typeCheckExpr ctx e2
        in
          case (ty1, ty2) of
            (Int, Int) => Int
          | _ =>
              raise
                (Fail
                   ("lhs value type " ^ typeToString ty1
                    ^ " doesn't match with rhs value type " ^ typeToString ty2))
        end
    | typeCheckExpr ctx (Ast.BinaryExpr (Ast.Modulo, e1, e2)) =
        let
          val ty1 = typeCheckExpr ctx e1
          val ty2 = typeCheckExpr ctx e2
        in
          case (ty1, ty2) of
            (Int, Int) => Int
          | _ =>
              raise
                (Fail
                   ("lhs value type " ^ typeToString ty1
                    ^ " doesn't match with rhs value type " ^ typeToString ty2))
        end
    | typeCheckExpr ctx (Ast.BinaryExpr (Ast.Apply, e1, e2)) =
        let
          val lhsType = typeCheckExpr ctx e1
          val () = print (typeToString lhsType ^ "\n")
          val rhsType = typeCheckExpr ctx e2
          val () = print (typeToString rhsType ^ "\n")
        in
          case (lhsType, rhsType) of
            (Func (a, b), _) =>
              if isTypeEqual (a, rhsType) then
                b
              else
                raise
                  (Fail
                     ("argument type " ^ typeToString a
                      ^ " doesn't match with given argument type "
                      ^ typeToString rhsType))
          | (RecordSelector (Field f), Record fields) =>
              #2 (valOf (List.find (fn (field, _) => field = f) fields))
          | (RecordSelector (Index i), Tuple fields) => List.nth (fields, i - 1)
          | _ =>
              raise (Fail "left hand side of the expression is not a function")
        end
    | typeCheckExpr ctx (Ast.UnaryExpr (Ast.Plus, e)) =
        let
          val ty = typeCheckExpr ctx e
        in
          case ty of
            Int => Int
          | Real => Real
          | _ =>
              raise
                (Fail
                   ("unary plus requires int or real but received "
                    ^ typeToString ty))
        end
    | typeCheckExpr ctx (Ast.UnaryExpr (Ast.Minus, e)) =
        let
          val ty = typeCheckExpr ctx e
        in
          case ty of
            Int => Int
          | Real => Real
          | _ =>
              raise
                (Fail
                   ("unary minus requires int or real but received "
                    ^ typeToString ty))
        end
    | typeCheckExpr ctx (Ast.UnaryExpr (Ast.Not, e)) =
        let
          val ty = typeCheckExpr ctx e
        in
          case ty of
            Bool => Bool
          | _ =>
              raise
                (Fail
                   ("not operation requires boolean but received "
                    ^ typeToString ty))
        end
    | typeCheckExpr _ (Ast.RecordSelector (Ast.Field f)) =
        RecordSelector (Field f)
    | typeCheckExpr _ (Ast.RecordSelector (Ast.Index i)) =
        RecordSelector (Index i)
    | typeCheckExpr ctx (Ast.TypeAnnotation (e, t)) =
        let
          val exprTy = typeCheckExpr ctx e
          val tyTy = astTyToType ctx t
        in
          if isTypeEqual (exprTy, tyTy) then
            exprTy
          else
            raise
              (Fail
                 ("lhs value type " ^ typeToString exprTy
                  ^ " doesn't match with rhs value type " ^ typeToString tyTy))
        end

  and typeCheckDecl ctx (Ast.ValDecl (name, expr, ty)) =
        let
          val exprType = typeCheckExpr ctx expr
        in
          case ty of
            SOME t =>
              if isTypeEqual (exprType, (astTyToType ctx t)) then
                addBinding ctx (name, exprType)
              else
                raise
                  (Fail
                     ("lhs and rhs of declaration does't match expected "
                      ^ typeToString (astTyToType ctx t) ^ " received "
                      ^ typeToString exprType))
          | NONE => addBinding ctx (name, exprType)
        end
    | typeCheckDecl ctx (Ast.FunDecl (name, params, expr, ty)) =
        let
          val ctx' = mkContext (SOME ctx)
          val _ =
            List.map
              (fn (name', typ) => addBinding ctx' (name', astTyToType ctx typ))
              params
          val exprType = typeCheckExpr ctx' expr
          val funType =
            case params of
              [] => Func (Unit, exprType)
            | _ =>
                List.foldr
                  (fn ((_, typ), acc) => Func (astTyToType ctx typ, acc))
                  exprType params
        in
          case ty of
            SOME t =>
              if isTypeEqual (exprType, (astTyToType ctx t)) then
                addBinding ctx (name, funType)
              else
                raise
                  (Fail
                     ("lhs and rhs of declaration does't match expected "
                      ^ typeToString (astTyToType ctx t) ^ " received "
                      ^ typeToString exprType))
          | NONE => addBinding ctx (name, funType)
        end
    | typeCheckDecl ctx (Ast.TypeAlias (name, ty)) =
        let val ty' = astTyToType ctx ty
        in addType ctx (name, ty')
        end
    | typeCheckDecl ctx (Ast.DataType (name, branches)) =
        let
          val branchTypes =
            List.map
              (fn (name', ty) =>
                 ( name'
                 , case ty of
                     NONE => NONE
                   | SOME t => SOME (astTyToType ctx t)
                 )) branches
          val ty = addType ctx (name, DataType (name, branchTypes))
          val _ =
            List.map
              (fn (name', ty') =>
                 case ty' of
                   NONE => addBinding ctx (name', ty)
                 | SOME t =>
                     addBinding ctx (name', Func (astTyToType ctx t, ty)))
              branches
        in
          ty
        end

  fun typeCheckProgram (Ast.Program decls) =
    let
      val ctx = mkContext NONE
      fun check decl =
        let val typ = typeCheckDecl ctx decl
        in typ
        end
    in
      List.map check decls
    end
end
