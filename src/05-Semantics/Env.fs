module Env

open Types

type EnvEntry =
    | VarEntry of Type
    | FunEntry of formals: Type list * result: Type