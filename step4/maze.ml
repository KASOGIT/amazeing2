(*
**
*)

type graph = {mutable check : bool; id : int};;
type door = {mutable state : bool; d1 : int; d2 : int};;
type get_door = {close: bool; pos: int};;
type get_case = {id: int; pos: int};;
  
let x = ref 0;;
let y = ref 0;;
let count = ref 0;;
let room_list = ref [||];;
let door_list = ref [||];;
let room_s = ref 0;;
let room_e = ref 0;;

module Maze =
  struct

(*
************** PRINT *************
*)
    
    let rec print_bord i =
      if i = (!x * 3 + 1)
      then
        begin
          print_string "+";
          print_bord (i - 1);
        end
      else
        if (i > 1)
        then
          begin
            print_string "-";
            print_bord (i - 1)
          end
        else
          begin
            print_string "+";
            print_newline ();
          end;;

    let print_open pos b =
      if b = 0
      then
        if !door_list.(pos).state = false
        then
          print_string "|"
        else
          print_string " "
      else
        if !door_list.(pos).state = false 
        then
          print_string "--"
        else
          print_string "  ";;

    (* let print_room a = *)
    (*     print_int a.id; *)
    (*     print_string " "; *)
    (*     print_string (string_of_bool a.check); *)
    (*     print_endline ""; *)
    (* ;; *)

    let print_door a =
      print_int a.d1;
      print_string " ";
      print_int a.d2;
      print_string " ";
      print_string (string_of_bool a.state);
      print_endline "";
    ;;

 (*   let rec print_door a = function
      | [] -> ()
      | hd::tl ->
         begin
           print_string "Door : ";
           print_string (string_of_bool hd.state);
           print_string " (";
           print_int hd.d1;
           print_string " -> ";
           print_int hd.d2;
           print_string ")";
           print_newline ();
           print_door tl;
         end;;*)

    let rec print_maze_line cpt cpt2 =
      if cpt = 0
      then
        begin
          print_string "|";
          print_maze_line (cpt + 1) cpt2
        end
      else
        if (cpt < (!x * 3))
        then
          if (cpt mod 3 <> 0)
          then
            begin
              print_string " ";
              print_maze_line (cpt + 1) cpt2
            end
          else
            begin
              print_open ((cpt2 / 2) * (!x - 1) + (cpt / 3 - 1)) 0;
              print_maze_line (cpt + 1) cpt2
            end
        else
            print_endline "|";
      ;;

      let rec print_maze_line2 cpt cpt2 =
        if cpt = 0
        then
          begin
            print_string "|";
            print_maze_line2 (cpt + 1) cpt2
          end
        else
          if (cpt < !x * 2)
          then
            if (cpt mod 2 == 0)
            then
              begin
                print_string "+";
                print_maze_line2 (cpt + 1) cpt2
              end
            else
              begin
                print_open ((cpt2 / 2) * !y + (cpt / 2) + ((!x - 1) * !y)) 1;
                print_maze_line2 (cpt + 1) cpt2
              end
          else
            print_endline "|";
      ;;
    
      let rec print_maze cpt =
        if (cpt < !y * 2 - 1)
        then
          if (cpt mod 2 = 0)
          then
            begin
              print_maze_line 0 (cpt + 1);
              print_maze (cpt + 1);
            end
          else
            begin
              print_maze_line2 0 cpt;
              print_maze (cpt + 1);
            end
        else
          print_bord (!x * 3 + 1);
      ;;

(*
***************** GEN_ROOM & DOOR *************
 *)
        
    let rec create_door_v i j id nb_door =
      if (j > 1)
      then if i > 0
           then
             begin
               !door_list.(nb_door) <- {state = false; d1 = id; d2 = id + !x};
               create_door_v (i - 1) j (id + 1) (nb_door + 1)
             end
           else
             create_door_v !x (j - 1) (id) (nb_door);
    ;;

   let rec create_door i j id nb_door =
      if (j > 0)
      then if i > 1
           then
             begin
               (!door_list).(nb_door) <- {state = false; d1 = id; d2 = id + 1};
               create_door (i - 1) j (id + 1) (nb_door + 1)
             end
           else
             create_door !x (j - 1) (id + 1) (nb_door)
      else
        create_door_v !x !y 0 (nb_door);
   ;;

    let rec create_room i j cpt =
      if (cpt < (i * j))
      then
        begin
          !room_list.(cpt) <- {check = false; id = cpt};
          create_room i j (cpt + 1);
        end
   ;;

(*
************ GEN_MAZE ****************
 *)
     
   let rec is_check_by_id id_room =
     if id_room < 0
     then false
     else
       begin
         if !room_list.(id_room).check = false
         then true
         else false
       end
   ;;

   let rec get_rand_case rand_id =
     if (is_check_by_id rand_id)
     then rand_id
     else (get_rand_case (Random.int ((Array.length !room_list) + 1)))

   (* let rec set_case_check_by_id room_list id = *)
   (*   match room_list with *)
   (*   | [] -> [] *)
   (*   | hd::tl -> *)
   (*      begin *)
   (*        if id = 0 *)
   (*        then {check = true; id = hd.id}::set_case_check_by_id tl (id - 1) *)
   (*        else hd::set_case_check_by_id tl (id - 1) *)
   (*      end;; *)

   (* let rec is_gen room_list = *)
   (*   match room_list with *)
   (*   | [] -> true *)
   (*   | hd::tl -> if hd.check = false then false else is_gen tl *)
   (* ;; *)

   let append l i = l @ [i];;

   let is_door_close elem p1 p2 =
     if ((elem.d1 = p1 && elem.d2 = p2) || (elem.d1 = p2 && elem.d2 = p1))
     then true
     else false
   ;;

   let rec search_door_and_check_state p1 p2 length =
     if length >= 0
     then
       begin
         if (is_door_close !door_list.(length) p1 p2)
         then
           begin
             if !door_list.(length).state = false
             then {close = true; pos = length}
             else {close = false; pos = length}
           end
         else (search_door_and_check_state p1 p2 (length - 1))
       end
     else {close = false; pos = 0};
   ;;
     
   let get_case_left pos acc =
     if ((pos) mod !x) >= 0
        && (is_check_by_id (pos - 1))
     then
       begin
         let fd = (search_door_and_check_state pos (pos - 1) ((Array.length !door_list) - 1)) in
         if fd.close
         then {id = !room_list.(pos - 1).id; pos = fd.pos}::acc
         else acc
       end
     else
       acc;
   ;;

   let get_case_right pos acc =
     if ((pos + 1) mod !x) <> 0
        && (is_check_by_id (pos + 1))
     then
       begin
         let fd = (search_door_and_check_state pos (pos + 1) ((Array.length !door_list) - 1)) in
         if fd.close
         then {id = !room_list.(pos + 1).id; pos = fd.pos}::acc
         else acc
       end
     else
       acc;
   ;;

   let get_case_up pos acc =
     if (pos - !x) >= 0
        && (is_check_by_id (pos - !x))
     then
       begin
         let fd = (search_door_and_check_state pos (pos - !x) ((Array.length !door_list) - 1)) in
         if fd.close
         then {id = !room_list.(pos - !x).id; pos = fd.pos}::acc
         else acc
       end       
     else
       acc;
   ;;

   let get_case_down pos acc =
     if (pos + !x) < (!x * !y)
        && (is_check_by_id (pos + !x))
     then
       begin
         let fd = (search_door_and_check_state pos (pos + !x) ((Array.length !door_list) - 1)) in
         if fd.close
         then {id = !room_list.(pos + !x).id; pos = fd.pos}::acc
         else acc
       end              
     else
       acc;
   ;;

   let get_near_available_room pos =
     (get_case_down pos (get_case_up pos (get_case_right pos (get_case_left pos []))));
   ;;

   (* let rec open_door pos p1 p2 = *)
   (*   if pos >= 0 *)
   (*   then *)
   (*     begin *)
   (*       if (is_door_close !door_list.(pos) p1 p2) *)
   (*       then (!door_list.(pos)).state <- true *)
   (*       else (open_door (pos - 1) p1 p2) *)
   (*     end *)
   (*   else *)
   (*     () *)
   (* ;; *)

   let print_elem elem =
     begin
       print_string "pos: ";
       print_int elem.id;
       print_newline();
     end;;
     
   let rec gen_maze pos stack_pos state =
     if state || not (Stack.is_empty stack_pos)
     then
       begin
         if !count < (!x * !y)
         then
           begin
             if state
             then
               begin
                 !room_list.(pos).check <- true;
                 incr count;
               end;
             let near_room = get_near_available_room pos in
             if (List.length near_room) > 0
             then
               begin
                 Stack.push pos stack_pos;
                 let rand_pos = (List.nth near_room (Random.int (List.length near_room))) in
                 !door_list.(rand_pos.pos).state <- true;
                 gen_maze rand_pos.id stack_pos true;
               end
             else
               gen_maze (Stack.pop stack_pos) stack_pos false
           end
         else
           ()
       end
     else
       ()
   ;;

(*
*************** SOLVE *************
*)

   let get_case_left_s pos acc =
     if ((pos) mod !x) >= 0
        && not (is_check_by_id (pos - 1))
     then
       begin
         let fd = (search_door_and_check_state pos (pos - 1) ((Array.length !door_list) - 1)) in
         if not fd.close
         then {id = !room_list.(pos - 1).id; pos = fd.pos}::acc
         else acc
       end
     else
       acc;
   ;;

   let get_case_right_s pos acc =
     if ((pos + 1) mod !x) <> 0
        && not (is_check_by_id (pos + 1))
     then
       begin
         let fd = (search_door_and_check_state pos (pos + 1) ((Array.length !door_list) - 1)) in
         if not fd.close
         then {id = !room_list.(pos + 1).id; pos = fd.pos}::acc
         else acc
       end
     else
       acc;
   ;;

   let get_case_up_s pos acc =
     if (pos - !x) >= 0
        && not (is_check_by_id (pos - !x))
     then
       begin
         let fd = (search_door_and_check_state pos (pos - !x) ((Array.length !door_list) - 1)) in
         if not fd.close
         then {id = !room_list.(pos - !x).id; pos = fd.pos}::acc
         else acc
       end       
     else
       acc;
   ;;

   let get_case_down_s pos acc =
     if (pos + !x) < (!x * !y)
        && not (is_check_by_id (pos + !x))
     then
       begin
         let fd = (search_door_and_check_state pos (pos + !x) ((Array.length !door_list) - 1)) in
         if not fd.close
         then {id = !room_list.(pos + !x).id; pos = fd.pos}::acc
         else acc
       end              
     else
       acc;
   ;;

   let get_near_available_room_s pos =
     (get_case_down_s pos (get_case_up_s pos (get_case_right_s pos (get_case_left_s pos []))));
   ;;
     
   let rec solve pos pos_end stack_pos =
     if (pos <> pos_end)
     then
       begin
         !room_list.(pos).check <- false;
         let near_room = get_near_available_room_s pos in
         if (List.length near_room) > 0
         then
           begin
             Stack.push pos stack_pos;
             let rand_pos = (List.nth near_room (Random.int (List.length near_room))) in
             solve rand_pos.id pos_end stack_pos;
           end
         else
           solve (Stack.pop stack_pos) pos_end stack_pos
       end
     else
       stack_pos
   ;;
         
(*
**************** INIT *************
*)
         
    let set_x arg_x =
      x := arg_x;;
     
    let set_y arg_y =
      y := arg_y;;

    let init() =
      begin
        Random.self_init ();
        let keyword = [("-c", Arg.Int (set_x), "Set x"); ("-r", Arg.Int (set_y), "Set y");] in
        let usage_words = "-c [NUMBER OF COLUMNS] -r [NUMBER OF RAWS]" in
        if Array.length Sys.argv - 1 = 0
        then
          begin
            x := 10;
            y := 10;
          end
        else
          Arg.parse keyword print_endline usage_words;
        if !x <= 1 || !y <= 1
        then
          print_endline "Usage : -c [NUMBER OF COLUMNS] -r [NUMBER OF RAWS]"
        else
          begin
            room_list := (Array.make (!x * !y) ({check = false; id = 0}));
            door_list := (Array.make ((!x - 1) * !y + !x * (!y - 1)) ({state = false; d1 = 0; d2 = 0}));
            create_room !x !y 0;
            create_door !x !y 0 0;
            room_s := (Random.int (!x * !y));
            room_s := !room_s - (!room_s mod !x);
            room_e := (Random.int (!x * !y));
            room_e := !room_e - (!x * ((!room_e / !x) mod !y));
            if (!room_s = !room_e)
            then
              room_s := !room_s + !x;
            gen_maze (get_rand_case (Random.int (Array.length !room_list))) (Stack.create()) true;
            (* let stack_pos = solve !room_s !room_e (Stack.create()) in *)
            (* let list_pos = stack_to_list (Stack.top stack_pos) stack_pos in *)
            print_bord (!x * 3 + 1);
            print_maze 0;
          end
      end
    ;;
  end
;;
