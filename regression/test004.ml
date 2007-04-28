let len = Sys.max_array_length
   
let app = fun el l -> el::l


let _ = 
  Printf.printf "append time test\n"; 
  let a1 = Urray.make (len) 0 in
  let a2 = Array.make (len) 0 in
  let t1 = Sys.time() in
   ignore(Urray.copy a1);
   let t2 = Sys.time() -. t1 in
   let t3 = Sys.time() in
    ignore(Array.copy a2);
    let t4 = Sys.time() -. t3 in
    Printf.printf "%f\n" (t2/.t4);


  
  
  
