open Closure
open Llvm
open Llvm_executionengine
open Llvm_target
open Llvm_scalar_opts

exception Compile_error of string
let compiler_error msg = raise (Compile_error msg)

let context = global_context ()
let the_module = create_module context "miniml module"
let builder = builder context
let named_values: (Id.t, llvalue) Hashtbl.t = Hashtbl.create 10
let double_type = double_type context
let bool_type = i1_type context
let void_type = void_type context
let struct_type = struct_type context
let i64_type = i64_type context
let i32_type = i32_type context
let i8_type = i8_type context
let i8_ptr_type = pointer_type i8_type
let malloc_type = function_type i8_ptr_type [| i64_type |]
let malloc_fun = declare_function "miniml_malloc" malloc_type the_module


(* A closure struct has the following form:
          struct closure {
                           i8* code; /* void *code is a function pointer to the code */
                           i8* env;  /* void *env is a pointer to an environment struct  */
                          }
    All closure functions must be in the following form
    ret_type name (struct closure *, arg_type_1 arg1, ..., arg_type_n argn)
*)

let closure_struct_type = named_struct_type context "closure"
let _ = struct_set_body closure_struct_type [| i8_ptr_type; i8_ptr_type |] false
let closure_type = pointer_type closure_struct_type

(*let named_type = named_struct_type context "closure"
let _ = struct_set_body named_type [| (function_type void_type [| named_type|]); i64_type |] false
let junk = declare_function "test" (function_type void_type [| named_type; double_type |]) the_module *)

type base_type =
  | UnitTy
  | BoolTy
  | FloatTy
  | ClosureTy

let rec basetype2llvm = function
  | UnitTy -> void_type
  | BoolTy -> bool_type
  | FloatTy -> double_type
  | ClosureTy -> closure_type

let rec type2llvm = function
  | Type.Unit -> void_type
  | Type.Bool -> bool_type
  | Type.Float -> double_type
  | Type.Fun (args, ret) -> function_type (type2llvm ret) (Array.of_list (List.map type2llvm args))
  | Type.Var _ -> assert false

(* This function constructs a return type for functions. Functions that return functions
   must return struct closure* in the generated LLVM IR *)
let return_type = function
  | Type.Fun(_, Type.Unit) -> UnitTy
  | Type.Fun(_, Type.Bool) -> BoolTy
  | Type.Fun(_, Type.Float) -> FloatTy
  | Type.Fun(_, Type.Fun(_,_)) -> ClosureTy
  | _ -> assert false

let string_of_base_type = function
  | UnitTy -> "unit"
  | BoolTy -> "bool"
  | FloatTy -> "float"
  | ClosureTy -> "closure"

exception Return_val of llvalue
let return_value v = raise (Return_val v)

let lookup x =
  try
    Hashtbl.find named_values x
  with
  | Not_found -> compiler_error ("Variable " ^ x ^ " not found")

let rec compile_expr = function
  | Unit -> const_null void_type (* not sure what to do here *)
  | Bool b -> let v = match b with
                | true -> 1
                | false -> 0
              in
              const_int bool_type v
  | Float f -> const_float double_type f
  | Neg (e) -> build_fneg (compile_expr e) "negtmp" builder
  | Add (e1, e2) -> build_fadd (compile_expr e1) (compile_expr e2) "addtmp" builder
  | Sub (e1, e2) -> build_fsub (compile_expr e1) (compile_expr e2) "subtmp" builder
  | Mult (e1, e2) -> build_fmul (compile_expr e1) (compile_expr e2) "multmp" builder
  | Div (e1, e2) -> build_fdiv (compile_expr e1) (compile_expr e2) "divtmp" builder
  | Eq (e1, e2) -> build_fcmp Fcmp.Ueq (compile_expr e1) (compile_expr e2) "eqtmp" builder
  | Le (e1, e2) -> build_fcmp Fcmp.Ult (compile_expr e1) (compile_expr e2) "letmp" builder
  | If (pe, ce, ae) ->
     let pred = compile_expr pe in

     (* Grab the first block so that we might later add the
      * conditional branch to it at the end of the function *)
     let start_bb = insertion_block builder in
     let the_function = block_parent start_bb in

     let then_bb = append_block context "then" the_function in

     (* Emit 'then' value *)

     position_at_end then_bb builder;
     let then_val = compile_expr ce in

     (* Compilation of 'then' can change the current block, update then_bb
      * for the phi. We create a new because one is used for the phi node
      * and the other is used for the conditional branch *)
     let new_then_bb = insertion_block builder in

     (* Emit 'else' value *)
     let else_bb = append_block context "else" the_function in
     position_at_end else_bb builder;
     let else_val = compile_expr ae; in

     (* Compilation of 'else' can change the current block, update else_bb
      * for the phi. *)

     let new_else_bb = insertion_block builder in

     (* Emit the merge block *)
     let merge_bb = append_block context "ifcont" the_function in
     position_at_end merge_bb builder;
     let incoming = [(then_val, new_then_bb); (else_val, new_else_bb)] in
     let phi = build_phi incoming "iftmp" builder in

     (* Return to the start block to add the conditional branch *)
     position_at_end start_bb builder;
     ignore (build_cond_br pred then_bb else_bb builder);

     (* Set an unconditional branch at the end of the 'then' block and the
      * 'else' block to the merge 'block' *)
     position_at_end new_then_bb builder;
     ignore (build_br merge_bb builder);
     position_at_end new_else_bb builder;
     ignore (build_br merge_bb builder);

     (* Finally, set the builder to the end of the merge block *)
     position_at_end merge_bb builder;

     phi
  | Let ((x, t), e1, e2) ->
       let value = compile_expr e1 in
       Hashtbl.add named_values x value;
       compile_expr e2
  | Var x -> lookup x
  | MakeCls ((name, t), { entry = fun_name; actual_fv = fv}, body) ->
     let callee =
       match lookup_function fun_name the_module with
       | Some func -> func
       | None -> compiler_error ("Closure Function" ^ fun_name ^ " not found")
     in

     (* create the closure struct type *)

     (*let fv_types = Array.of_list (List.map (fun (n, t) -> type2llvm t) fv) in *)
     (*let struct_ar = Array.append [| (type_of callee) |] fv_types in *)
     (*let struct_t  = type_of (param callee 0) in *)

     (* get the size of the struct *)
     let size_struct = size_of closure_struct_type in

     (* malloc the closure struct *)
     let malloc_ptr = build_call malloc_fun [| size_struct |] "malloctmp" builder in

     (* bitcast the pointer returned by malloc *)
     let closure_ptr = build_bitcast malloc_ptr closure_type "closure" builder in

     dump_module the_module;
     (* get the first element of the closure struct *)
     let name_elem = build_struct_gep closure_ptr 0 "name_elem" builder in

     (* cast the function pointer to an i8* *)
     let name_tmp = build_bitcast callee i8_ptr_type "nametmp" builder in

     (* store the casted function name into the name element *)
     ignore (build_store name_tmp name_elem builder);

     (* build the free variable struct *)
     let fvtyps = Array.of_list (List.map (fun r -> type2llvm (snd r)) fv) in
     let env_struct_t = struct_type fvtyps in
     let env_struct_ptr_t = pointer_type env_struct_t in

     let env_size = size_of env_struct_t in

     (* malloc the env struct *)
     let malloc_env_ptr = build_call malloc_fun [| env_size |] "malloctmp" builder in

     (* get the second element of the closure struct *)
     let env_elem = build_struct_gep closure_ptr 1 "env_elem" builder in

     (* store the env pointer into the closure struct *)
     ignore (build_store malloc_env_ptr env_elem builder);

     (* bitcast the i8* malloced env pointer to a pointer to the env struct *)
     let env_ptr = build_bitcast malloc_env_ptr env_struct_ptr_t "envptr" builder in

     (* Store the free variables into the struct *)
     List.iteri (fun i (name, typ) ->
                 (* get the ith elem of the env struct *)
                 let elem = build_struct_gep env_ptr i "fvtmp" builder in
                 (* get the value of the free variable *)
                 let value = lookup name in
                 (* store the value into the element *)
                 ignore (build_store value elem builder))
                fv;

    (* Add the closure struct pointer to the hash table *)
     Hashtbl.add named_values name closure_ptr;

    (* Compile the expression *)
     compile_expr body
  | AppCls ((f, typ, tys), elist) ->
     (* Lookup the closure struct *)
     let closure_ptr = lookup f in
     print_endline ("appling closure " ^ f ^ "!!!");
     print_endline ("closure " ^ f ^ " type " ^ Prettyprint.string_of_type typ);
     print_endline ("apply closure struct " ^ f ^ " " ^ (string_of_lltype (type_of closure_ptr)));

     (* construct the return type of the closure *)
     let ret_type = basetype2llvm (return_type typ) in

     (* compute the argument types of the closure *)
     let arg_type = Array.append [| closure_type |] (Array.of_list (List.map type2llvm tys)) in

     (* get the first element of the struct; corresponding to the function pointer *)
     let name_elem = build_struct_gep closure_ptr 0 "name_elem" builder in
     let name_tmp = build_load name_elem "calletmp" builder in

     let callee_type = function_type ret_type arg_type in

     let callee = build_bitcast name_tmp (pointer_type callee_type) ("callee_" ^ f) builder in

     (* build the argument array *)
     let args = Array.map compile_expr (Array.of_list elist) in
     let full_args = Array.append [| closure_ptr |] args in

     dump_module the_module;
     (* call the closure function *)
     print_endline ("calling closure " ^ (string_of_lltype (type_of callee)));
     print_endline (string_of_llvalue callee);
     Array.iter (fun s -> print_endline (string_of_llvalue s)) full_args;
     print_endline "making call!!!!";
     let ret = build_call callee full_args "rettmp" builder in
     ret

  | AppDir (f, elist) ->  let callee =
                            match lookup_function f the_module with
                            | Some func -> func
                            | None -> compiler_error ("Function " ^ f ^ " not found")
                          in
                          let params = params callee in

                          if Array.length params == List.length elist then
                            ()
                          else
                            (let error_str = "Incorrect # of args passed to " ^ f ^ " " in
                            let error_str = error_str ^ "Expected " ^ (string_of_int (Array.length params))  in
                            let error_str = error_str ^ " Got " ^ (string_of_int (List.length elist)) in
                            compiler_error (error_str));

                          let args = Array.map compile_expr (Array.of_list elist) in
                          build_call callee args "calltmp" builder

let compile_extern extern_pair =
  let name, typ = extern_pair in
  let args_typ, ret_typ = match typ with
    | Type.Fun(f, t) -> f, t
    | _ -> assert false in
  let args = Array.of_list (List.map (type2llvm) args_typ) in
  let ret  = type2llvm ret_typ in
  let func_name = "miniml_" ^ name in
  let ft = function_type ret args in

  declare_function func_name ft the_module

let compile_externs es = List.map (compile_extern) es


let compile_prototype { name = (func_name, ret_typ);
                        args = arg_lst;
                        formal_fv = fv_lst } =

  (* Make the argument types *)
  let args = Array.of_list (List.map (fun a -> type2llvm (snd a)) arg_lst) in

  (* Make the return type *)
  let ret_type = return_type ret_typ in
  let ll_ret_type = basetype2llvm ret_type in

  let final_args =
    if List.length fv_lst > 0 then
      Array.append [| closure_type |] args
    else
      args
  in
  (* Make the function type: ret_typ name(struct closure*, arg1, ..., argn) etc. *)
  let ft = function_type ll_ret_type final_args in

  print_endline ("Closure Return type: " ^ (string_of_base_type ret_type) ^ " fname:" ^func_name);
  print_endline ("Return type: " ^ (string_of_lltype ll_ret_type) ^ " fname:" ^ func_name);

  let f =
    match lookup_function func_name the_module with
    | None -> declare_function func_name ft the_module
    | Some f -> compiler_error ("Redefinition of function " ^ func_name )
  in

  (* Set name for the free variable env struct *)
  let env_param = param f 0 in
  (set_value_name func_name (param f 0);
   Hashtbl.add named_values func_name env_param;

   (* Set names for all regular arguments *)
   let reg_params =
     if List.length fv_lst > 0 then
       List.tl (Array.to_list (params f))
     else
       Array.to_list (params f)
   in
   List.iter2 (fun n v -> set_value_name n v;
                          Hashtbl.add named_values n v)
              (List.map fst arg_lst) reg_params;
   f)

let extract_fv fvs the_function =
  let closure_ptr = param the_function 0 in
  let zero  = const_int i32_type 0 in
  let one   = const_int i32_type 1 in
  let env_elem = build_gep closure_ptr [| zero; one |] "envintptr" builder in
  let env_i8 = build_load env_elem "env_i8" builder in

  (* build the fv struct type *)
  let fvtyps = Array.of_list (List.map (fun fv -> type2llvm (snd fv)) fvs) in
  let env_struct_t = struct_type fvtyps in
  let env_struct_ptr_t = pointer_type env_struct_t in

  (* bitcast the i8* env pointer to a pointer to the env struct *)
  let env_ptr = build_bitcast env_i8 env_struct_ptr_t "envptr" builder in

  dump_module the_module;

  (* Go through each fv and remove it from the env struct and name it *)
  List.iteri (fun i (name, typ) ->
              (* get ith element from closure struct *)
              print_endline ("set name: " ^  name);
              let elem = build_struct_gep env_ptr i name builder in
              let value = build_load elem "fvtmp" builder in
              Hashtbl.add named_values name value
             )
             fvs

let compile_func the_fpm func_def =
  Hashtbl.clear named_values;
  let (name, typ) = func_def.name in
  let the_function = compile_prototype func_def in
  let fvs  = func_def.formal_fv in
  let body = func_def.body in
  let takes_closure = func_def.takes_closure in

  (* Create a new basic block to start insertion into *)
  let bb = append_block context "entry" the_function in
  position_at_end bb builder;

  try
    (* Extract free variables from env struct *)
    if takes_closure then
      extract_fv fvs the_function
    else
      ();

    let ret_val = compile_expr body in
    let ret_type = type_of ret_val in
    print_endline ("CF Return type: " ^ (string_of_lltype ret_type) ^ " fname:" ^ name);


    (* Finish off the function *)
    let _ = build_ret ret_val builder in

    let oc = open_out "debug.ll" in
    Printf.fprintf oc "%s\n" (string_of_llmodule the_module);
    close_out oc;


    (* Validate the generate code, checking for consistency *)
    Llvm_analysis.assert_valid_function the_function;

    (* Optimize the function *)
    let _ = PassManager.run_function the_function the_fpm in

    the_function
  with e ->
    delete_function the_function;
    raise e

let compile_program the_fpm program =
  let funs, body = match program with
    | Prog (funs, body) -> funs, body in
  let protos = List.map (fun e -> let name, ty = e.name in
                                  (name, compile_func the_fpm e)) funs in

  (* Create an entry point function [ void miniml_main() ] *)
  let ft = function_type void_type [| |] in
  let miniml_main =  declare_function "miniml_main" ft the_module in

  (* Create a new basic block to start insertion into *)
  let bb = append_block context "entry" miniml_main in
  position_at_end bb builder;

  (* Clear previous names *)
  Hashtbl.clear named_values;

  (* Add prototypes back in *)
  List.iter (fun (name, p) -> Hashtbl.add named_values name p) protos;

  (* Compile body *)
  ignore (compile_expr body);

  ignore (build_ret_void builder);

  let oc = open_out "debug.ll" in
  Printf.fprintf oc "%s\n" (string_of_llmodule the_module);
  close_out oc;

  (* Validate the generate code, checking for consistency *)
  Llvm_analysis.assert_valid_function miniml_main;

  (* Optimize the function *)
  let _ = PassManager.run_function miniml_main the_fpm in ()


let compile outfname ast externs =
  let oc = open_out outfname in

  ignore (initialize ());

  let the_fpm = PassManager.create_function the_module in

  (* Promote allocas to registers *)
  add_memory_to_register_promotion the_fpm;

  (* Do simple "peephole" and bit-twiddling optimizations *)
  add_instruction_combination the_fpm;

  (* reassociate expressions *)
  add_reassociation the_fpm;

  (* Eliminate common sub-expressions *)
  add_gvn the_fpm;

  (* Simplify the control flow graph (delete unreachable blocks, etc.) *)
  add_cfg_simplification the_fpm;

  ignore (PassManager.initialize the_fpm);

  ignore (compile_externs externs);
  compile_program the_fpm ast;
  Printf.fprintf oc "%s\n" (string_of_llmodule the_module);
  close_out oc
