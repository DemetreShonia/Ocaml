#thread;;
#directory "+threads";;
#load "unix.cma";;
#load "threads.cma";;

let rec count n k = 
  print_int (Thread.id (Thread.self()));
  print_string "  ";
  print_int k;
  print_string "\n";
  if n = k then ()
  else count n (k+1);;


let spawn_counter n = Thread.create (count n) 0;;

Thread.join (spawn_counter 7);;