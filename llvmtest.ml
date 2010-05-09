open Printf
open Llvm

let main filename =
   let c = create_context () in

   let i8_t  = i8_type c in
   let i8_p_t = pointer_type i8_t in
   let i32_t = i32_type c in
   let zero = const_int i32_t 0 in

   let m = create_module c filename in

   (* @greeting = global [14 x i8] c"Hello, world!\00" *)
   let greeting =
     define_global "greeting" (const_string c "Hello, world!\000") m in

   (* declare i32 @puts(i8* ) *)
   let puts =
     declare_function "puts"
       (function_type i32_t [| i8_p_t |]) m in

   (* define i32 @strlen() { entry: *)
   let strlen = define_function "strlen" (function_type i32_t [| i8_p_t |]) m in
   let strlen_src = (params strlen).(0) in
   set_value_name "src" strlen_src;
   let entry_strlen = entry_block strlen in
   let builder = builder_at_end c entry_strlen in

   let loop_bb = append_block c "loop" strlen in
   let at_loop_bb = builder_at_end c loop_bb in

   let loopi_bb = append_block c "loopi" strlen in
   let at_loopi_bb = builder_at_end c loopi_bb in

   let loopend_bb = append_block c "loopend" strlen in
   let at_loopend_bb = builder_at_end c loopend_bb in
   
   let count = build_alloca i32_t "count" builder in
   ignore(build_store zero count builder);
   ignore (build_br loop_bb builder);

   let curcount = build_load count "count.0" at_loop_bb in
   let curcount1 = build_add curcount (const_int i32_t 1) "count.1" at_loopi_bb in
   ignore(build_store curcount1 count at_loopi_bb);
   ignore(build_br loop_bb at_loopi_bb);

   let curptr = build_gep strlen_src [| curcount |] "vp" at_loop_bb in
   let curval = build_load curptr "v" at_loop_bb in
   let isnull = build_icmp Icmp.Eq curval (const_int i8_t 0) "isnull" at_loop_bb in
   ignore (build_cond_br isnull loopend_bb loopi_bb at_loop_bb);
   
   let retval = build_load count "retval" at_loopend_bb in 
   ignore (build_ret retval at_loopend_bb);

   (* define i32 @main() { entry: *)
   let main_function = define_function "main" (function_type i32_t [| |]) m in
   let entry_main = entry_block main_function in
  
   let at_entry = builder_at_end c entry_main in
 
   (* %tmp = getelementptr [14 x i8]* @greeting, i32 0, i32 0 *)
   let str = build_gep greeting [| zero; zero |] "tmp" at_entry in

   (* call i32 @puts( i8* %tmp ) *)
   let _ = build_call puts [| str |] "" at_entry in
   (* call i32 @strlen( i8* %tmp ) *)
   let len = build_call strlen [| str |] "len" at_entry in

   (* ret void *)
   ignore (build_ret len at_entry);

   (* write the module to a file *)
   if not (Llvm_bitwriter.write_bitcode_file m filename) then exit 1;
   dispose_module m

let () = match Sys.argv with
  | [|_; filename|] -> main filename
  | _ -> main "a.out"

