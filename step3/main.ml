open Printf;;
open Maze;;

let main() =
  if Array.length Sys.argv - 1 != 5 && Array.length Sys.argv - 1 != 1 && Array.length Sys.argv - 1 != 0
  then
    print_endline "Usage : -c [NUMBER OF COLUMNS] -r [NUMBER OF RAWS]..."
  else Maze.init ();
;;

main();;

