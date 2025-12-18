structure Ast:
sig
  datatype typ =
    UnitTy
  | IntTy
  | RealTy
  | BoolTy
  | StringTy
  | ListTy of typ
  | TupleTy of typ list
  | RecordTy of (string * typ) list
  | FuncTy of typ * typ
  | VarTy of string

  datatype unary_expr = Plus | Minus | Not

  datatype binary_expr =
    Equal
  | NotEq
  | OrElse
  | AndAlso
  | Less
  | LessEq
  | Greater
  | GreaterEq
  | Cons
  | Concat
  | Add
  | Subtract
  | StrConcat
  | Multiply
  | Divide
  | IntDiv
  | Modulo
  | Apply

  datatype label = Field of string | Index of int

  datatype expr =
    Unit
  | Int of int
  | Real of real
  | Bool of bool
  | Str of string
  | List of expr list
  | Sequence of expr list
  | Tuple of expr list
  | Record of (string * expr) list
  (* | Lambda of typ * expr *)
  | Var of string
  | Let of decl list * expr
  | If of expr * expr * expr
  | BinaryExpr of binary_expr * expr * expr
  | UnaryExpr of unary_expr * expr
  | RecordSelector of label
  | TypeAnnotation of expr * typ

  and decl =
    ValDecl of string * expr * (typ option)
  | FunDecl of string * (string * typ) list * expr * (typ option)
  | TypeAlias of string * typ
  | DataType of string * (string * typ option) list

  datatype program = Program of decl list

  val getIntValue: expr -> int
  val getStringValue: expr -> string
  val getVarValue: expr -> string

  val printAst: program -> unit
end =
struct
  datatype typ =
    UnitTy
  | IntTy
  | RealTy
  | BoolTy
  | StringTy
  | ListTy of typ
  | TupleTy of typ list
  | RecordTy of (string * typ) list
  | FuncTy of typ * typ
  | VarTy of string

  datatype label = Field of string | Index of int

  datatype unary_expr = Plus | Minus | Not

  datatype binary_expr =
    Equal
  | NotEq
  | OrElse
  | AndAlso
  | Less
  | LessEq
  | Greater
  | GreaterEq
  | Cons
  | Concat
  | Add
  | Subtract
  | StrConcat
  | Multiply
  | Divide
  | IntDiv
  | Modulo
  | Apply

  datatype expr =
    Unit
  | Int of int
  | Real of real
  | Bool of bool
  | Str of string
  | List of expr list
  | Sequence of expr list
  | Tuple of expr list
  | Record of (string * expr) list
  (* | Lambda of typ * expr *)
  | Var of string
  | Let of decl list * expr
  | If of expr * expr * expr
  | BinaryExpr of binary_expr * expr * expr
  | UnaryExpr of unary_expr * expr
  | RecordSelector of label
  | TypeAnnotation of expr * typ

  and decl =
    ValDecl of string * expr * (typ option)
  | FunDecl of string * (string * typ) list * expr * (typ option)
  | TypeAlias of string * typ
  | DataType of string * (string * typ option) list

  datatype program = Program of decl list

  fun getIntValue (Int n) = n
    | getIntValue _ =
        raise (Fail "this function can only be called for integer literals")

  fun getStringValue (Str s) = s
    | getStringValue _ =
        raise (Fail "this function can only be called for string literals")

  fun getVarValue (Var s) = s
    | getVarValue _ =
        raise (Fail "this function can only be called for variables")

  fun typeToString UnitTy = "unit"
    | typeToString IntTy = "int"
    | typeToString RealTy = "real"
    | typeToString BoolTy = "bool"
    | typeToString StringTy = "string"
    | typeToString (ListTy ty) = typeToString ty ^ " list"
    | typeToString (TupleTy types) =
        String.concatWith " * " (List.map typeToString types)
    | typeToString (RecordTy pairs) =
        "{ "
        ^
        String.concatWith ", "
          (List.map (fn (name, ty) => name ^ " : " ^ typeToString ty) pairs)
        ^ " }"
    | typeToString (FuncTy (a, b)) =
        typeToString a ^ " -> " ^ typeToString b
    | typeToString (VarTy t) = t

  fun exprToString Unit = "()"
    | exprToString (Int n) = Int.toString n
    | exprToString (Real r) = Real.toString r
    | exprToString (Bool b) = Bool.toString b
    | exprToString (Str s) = s
    | exprToString (List exprs) =
        "[" ^ String.concatWith ", " (List.map exprToString exprs) ^ "]"
    | exprToString (Sequence exprs) =
        "[" ^ String.concatWith "; " (List.map exprToString exprs) ^ "]"
    | exprToString (Tuple exprs) =
        "(" ^ String.concatWith ", " (List.map exprToString exprs) ^ ")"
    | exprToString (Record pairs) =
        "{ "
        ^
        String.concatWith ", "
          (List.map (fn (name, expr) => name ^ " : " ^ exprToString expr) pairs)
        ^ " }"
    | exprToString (Var v) = v
    | exprToString (Let (decls, v)) =
        "let\n" ^ (String.concatWith "\n" (List.map declToString decls))
        ^ "\nin\n" ^ exprToString v ^ "\nend"
    | exprToString (If (clause, t, e)) =
        "if " ^ "(" ^ exprToString clause ^ ")" ^ " then " ^ exprToString t
        ^ " else " ^ exprToString e
    | exprToString (BinaryExpr (Equal, e1, e2)) =
        "(= " ^ exprToString e1 ^ " " ^ exprToString e2 ^ ")"
    | exprToString (BinaryExpr (NotEq, e1, e2)) =
        "(!= " ^ exprToString e1 ^ " " ^ exprToString e2 ^ ")"
    | exprToString (BinaryExpr (OrElse, e1, e2)) =
        "(orelse " ^ exprToString e1 ^ " " ^ exprToString e2 ^ ")"
    | exprToString (BinaryExpr (AndAlso, e1, e2)) =
        "(andalso " ^ exprToString e1 ^ " " ^ exprToString e2 ^ ")"
    | exprToString (BinaryExpr (Less, e1, e2)) =
        "(< " ^ exprToString e1 ^ " " ^ exprToString e2 ^ ")"
    | exprToString (BinaryExpr (LessEq, e1, e2)) =
        "(<= " ^ exprToString e1 ^ " " ^ exprToString e2 ^ ")"
    | exprToString (BinaryExpr (Greater, e1, e2)) =
        "(> " ^ exprToString e1 ^ " " ^ exprToString e2 ^ ")"
    | exprToString (BinaryExpr (GreaterEq, e1, e2)) =
        "(>= " ^ exprToString e1 ^ " " ^ exprToString e2 ^ ")"
    | exprToString (BinaryExpr (Cons, e1, e2)) =
        "(cons " ^ exprToString e1 ^ " " ^ exprToString e2 ^ ")"
    | exprToString (BinaryExpr (Concat, e1, e2)) =
        "(concat " ^ exprToString e1 ^ " " ^ exprToString e2 ^ ")"
    | exprToString (BinaryExpr (Add, e1, e2)) =
        "(+ " ^ exprToString e1 ^ " " ^ exprToString e2 ^ ")"
    | exprToString (BinaryExpr (Subtract, e1, e2)) =
        "(- " ^ exprToString e1 ^ " " ^ exprToString e2 ^ ")"
    | exprToString (BinaryExpr (StrConcat, e1, e2)) =
        "(^ " ^ exprToString e1 ^ " " ^ exprToString e2 ^ ")"
    | exprToString (BinaryExpr (Multiply, e1, e2)) =
        "(* " ^ exprToString e1 ^ " " ^ exprToString e2 ^ ")"
    | exprToString (BinaryExpr (Divide, e1, e2)) =
        "(/ " ^ exprToString e1 ^ " " ^ exprToString e2 ^ ")"
    | exprToString (BinaryExpr (IntDiv, e1, e2)) =
        "(div " ^ exprToString e1 ^ " " ^ exprToString e2 ^ ")"
    | exprToString (BinaryExpr (Modulo, e1, e2)) =
        "(mod " ^ exprToString e1 ^ " " ^ exprToString e2 ^ ")"
    | exprToString (BinaryExpr (Apply, e1, e2)) =
        "(apply " ^ exprToString e1 ^ " " ^ exprToString e2 ^ ")"
    | exprToString (UnaryExpr (Plus, e)) =
        "(+ " ^ exprToString e ^ ")"
    | exprToString (UnaryExpr (Minus, e)) =
        "(- " ^ exprToString e ^ ")"
    | exprToString (UnaryExpr (Not, e)) =
        "(! " ^ exprToString e ^ ")"
    | exprToString (RecordSelector (Field f)) = "#" ^ f
    | exprToString (RecordSelector (Index i)) = "#" ^ (Int.toString i)
    | exprToString (TypeAnnotation (e, t)) = exprToString e ^ " : " ^ typeToString t

  and declToString (ValDecl (id, v, _)) =
        "val " ^ id ^ " = " ^ (exprToString v)
    | declToString (FunDecl (id, params, v, _)) =
        "fun " ^ id ^ "("
        ^
        (String.concatWith ", "
           (List.map (fn (name, ty) => name ^ ": " ^ (typeToString ty)) params))
        ^ ")" ^ " = " ^ exprToString v
    | declToString (TypeAlias (name, ty)) =
        "type " ^ name ^ " = " ^ typeToString ty
    | declToString (DataType (name, _)) = "datatype " ^ name

  fun printAst (Program (p as (_ :: _))) =
        List.app (fn l => print (l ^ "\n")) (List.map declToString p)
    | printAst (Program []) = print "empty\n"
end
