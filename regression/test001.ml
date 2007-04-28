let len = Sys.max_array_length
   
let app = fun el l -> el::l
let app_r = fun l el -> el::l
let int_cmp i1 i2 = i1-i2


let _ =
 let init_test sz f k = 
 let a = Urray.init sz f in
 let res = ref (sz = Urray.length a) in
 for i = 0 to (Urray.length a)-1 do
    res := ((Urray.get a i) = (f i));
  done; 
  Printf.printf "init test #%d: %b\n" k !res 
in 
  init_test 0 (fun i -> 0) 0;
  init_test len (fun i -> (i*3)) 1;
  init_test  (len*2+76) (fun i -> (len mod (i+1))) 2;      

 let a = Urray.init (len*2+221434) (fun i->i) in
  let b = Urray.sub a (len-2) (len+245) in
   let res = ref true in
    for i = 0 to (Urray.length b)-1 do 
     res := ((Urray.get b i) = (Urray.get a (i+len-2)));
    done;
    Printf.printf "sub test: %b\n" !res;

 Printf.printf "to_list test #0: %b\n" ((Urray.to_list (Urray.make 0 0)) = [] );
 Printf.printf "to_list test #1: %b\n" ((Urray.to_list (Urray.make 1 1)) = [1] );

 let b = Urray.init (len+225454) (fun i->i/3) in
   Printf.printf "fold_right&to_list test: %b\n" ((Urray.to_list b) = (Urray.fold_right app b []));

 let b = Urray.init (len*2+24) (fun i->i/3) in
   Printf.printf "fold_left&to_list  test: %b\n" ((Urray.to_list b) = (Urray.fold_left app_r [] b));

 let to_of_list_test arr i=  
  Printf.printf "of_listt&to_list #%d: %b\n" i (Urray.of_list (Urray.to_list arr) = arr)
  in to_of_list_test b 0; 
      to_of_list_test (Urray.make 0 0) 1;
      to_of_list_test (Urray.make 1 0) 2;

 let c = Urray.copy b in
 Printf.printf "copy test: %b\n" (c=b);

 let res = ref true in
 Urray.fill b 40 (len+7) 10;
 for i = 0 to (Urray.length b)-1 do
   if (i<40) || (i>len+46) then
    begin if (Urray.get b i) != (Urray.get c i) then res := false end
  else begin if (Urray.get b i) != 10 then res := false  end
 done;
 Printf.printf "fill test: %b\n" !res;

let append_test ar1 ar2 k = 
 let res = ref true in
 let c = Urray.append ar1 ar2 in
 let c_ind = ref 0 in
  for i = 0 to (Urray.length ar1)-1 do
    res := ((Urray.get ar1 i) = (Urray.get c !c_ind));
    incr(c_ind)
  done;
  for i = 0 to (Urray.length ar2)-1 do
    res := ((Urray.get ar2 i) = (Urray.get c !c_ind));
    incr(c_ind)
  done;
  Printf.printf "append test #%d: %b\n" k !res
in 
  append_test (Urray.make 0 0) b 0;
  append_test b (Urray.make 0 2) 1;
  append_test b b                2; 

     
let blit_test a1 ofs1 a2 ofs2 len k =
  let c = Urray.copy a2 in
  Urray.blit a1 ofs1 a2 ofs2 len;
  let res = ref true in
  for i = 0 to (Urray.length a2)-1 do
   if (i<ofs2) or (i>=ofs2+len) then
     res := ((Urray.get a2 i) = (Urray.get c i))
   else res := ((Urray.get a2 i) = (Urray.get a1 (ofs1+i-ofs2))) 
  done; 
  Printf.printf "blit test #%d: %b\n" k !res
 in
  let d = Urray.copy a in blit_test d 1232 d (len+1) (len+333) 0;
  let d = Urray.copy a in blit_test a (len+100) d 0 (len+333)  1;
  let d = (Urray.init (len/2) (fun i->i)) in
   blit_test d 0 a 17 (len/2)  2;

let make_matrix_test size1 size2 v k = 
  let mm = Urray.make_matrix size1 size2 v in
   let res = ref ((Urray.length mm) = size1) in
    for i = 0 to (Urray.length mm)-1 do
      let ll = (Urray.length (Urray.get mm i)) in
      res := (ll = size2);
      for j = 0 to (ll-1) do
       res := ((Urray.get (Urray.get mm i) j) = v);
      done;   
    done;
    Printf.printf "make_matrix test #%d: %b\n" k !res
in
  make_matrix_test 0 0 0 0;
  make_matrix_test 2 (len*2) 1 1;


let a = Urray.init (len*2+23356) (fun i -> (Random.int len)) in
 Urray.stable_sort int_cmp a;
 let res = ref true in
 for i = 0 to (Urray.length a)-2 do
  res := ((int_cmp (Urray.get a (i+1)) (Urray.get a i)) >= 0)
 done;
 Printf.printf "stable_sort test: %b\n" !res
 






