(*
**
*)

type graph = {mutable check : bool; id : int; mutable soluce : bool};;
type door = {mutable state : bool; d1 : int; d2 : int};;
type get_door = {close: bool; pos: int};;
type get_case = {id: int; pos: int};;
  
let x = ref 10;;
let y = ref 10;;
let count = ref 0;;
let room_list = ref [||];;
let door_list = ref [||];;
let room_s = ref 0;;
let room_e = ref 0;;
let form = ref true;;

module Maze =
  struct
    let rec stack_to_list elem stack_pos =
      if Stack.is_empty stack_pos = true
      then []
      else (elem)::stack_to_list (Stack.pop stack_pos) stack_pos;;

    let set_soluce a =
      !room_list.(a).soluce <- true;
    ;;

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

    let print_door a =
      print_int !count;
      print_string " : ";
      incr count;
      print_int a.d1;
      print_string " ";
      print_int a.d2;
      print_string " ";
      print_string (string_of_bool a.state);
      print_endline "";
    ;;

    let print_st_ed cpt cpt2 list_pos =
      if (((cpt2 * !x) + (cpt / 2)) = !room_e)
      then
        print_string "ED"
      else
        if (((cpt2 * !x) + (cpt / 2)) = !room_s)
        then
          print_string "ST"
        else
          if !room_list.((cpt2 * !x) + (cpt / 2)).soluce = true
          then
            print_string ".."
          else
            print_string "  ";
    ;;

    let rec print_maze_line cpt cpt2 list_pos =
      if cpt = 0
      then
        begin
          print_string "|";
          print_maze_line (cpt + 1) cpt2 list_pos
        end
      else
        if (cpt < (!x * 2))
        then
          if (cpt mod 2 <> 0)
          then
            begin
              print_st_ed cpt cpt2 list_pos;
              print_maze_line (cpt + 1) cpt2 list_pos
            end
          else
            begin
              print_open ((!x * cpt2 + (cpt / 2) - 1) * 4 + 1) 0;
              print_maze_line (cpt + 1) cpt2 list_pos
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
                print_open ((!x * cpt2 + (cpt / 2)) * 4 + 2) 1;
                print_maze_line2 (cpt + 1) cpt2
              end
          else
            print_endline "|";
      ;;
    
      let rec print_maze cpt list_pos =
        if (cpt < (!y * 2 - 1))
        then
          if (cpt mod 2 = 0)
          then
            begin
              print_maze_line 0 (cpt / 2) list_pos;
              print_maze (cpt + 1) list_pos;
            end
          else
            begin
              print_maze_line2 0 ((cpt - 1) / 2);
              print_maze (cpt + 1) list_pos;
            end
        else
          print_bord (!x * 3 + 1);
      ;;

      let rec print_bord_hexa cpt_bord size = 
        if (cpt_bord < size)
        then
          begin
            if (cpt_bord mod 2 = 0)
            then
              print_string "__"
            else
              print_string "    ";
            print_bord_hexa (cpt_bord + 1) size;
          end
        else
          begin
            print_endline "";
          end
      ;;

      let rec print_second_bord_hexa2 cpt_second size =
        if (cpt_second < size - 1)
        then
          begin
            if (cpt_second mod 2 = 0)
            then
              begin
                if (cpt_second = 0)
                then
                  print_string "   \\"
                else
                  print_string "/  \\";
              end
            else
              print_string "__";
            print_second_bord_hexa2 (cpt_second + 1) size;
          end
        else
          begin
            if (!x mod 2 = 1)
            then
              print_string "/"
            else
              print_string "__/";
            print_endline "";
          end
      ;;

      let rec print_second_bord_hexa cpt size value line =
        if (cpt < size)
        then
          begin
            if (cpt mod 2 = 0)
            then
              begin
                if (line <> 0 && !door_list.(((line - 0) * !x + cpt) * 6 + 5).state = true)
                then print_string " "
                else print_string "/";
                if ((line - 0) * !x + cpt) = !room_s
                then print_string "ST"
                else
                  begin
                    if ((line - 0) * !x + cpt) = !room_e
                    then print_string "ED"
                    else
                      begin
                        if (!room_list.((line - 0) * !x + cpt).soluce = true)
                        then print_string ".."
                        else print_string "  ";
                      end;
                  end;
                if (line <> 0 && !door_list.(((line - 0) * !x + cpt) * 6 + 1).state = true)
                then print_string " "
                else print_string "\\";
              end
            else
              begin
                if (line <> 0 && !door_list.(((line - 1) * !x + cpt) * 6 + 3).state = true)
                then
                  print_string "  "
                else
                  print_string "__";
              end;
            print_second_bord_hexa (cpt + 1) size value line;
          end
        else
          begin
            if value = 1
            then
              print_string "/";
            print_endline "";
          end
      ;;

      let rec print_bord_hexa2 cpt size value line =
        if (cpt < size)
        then
          begin
            if (cpt mod 2 = 0)
            then
              begin
                if !door_list.(((line - 1) * !x + cpt) * 6 + 3).state = true
                then
                  print_string "  "
                else
                  print_string "__";
              end
            else
              begin
                if (line <> 0 && !door_list.(((line - 1) * !x + cpt) * 6 + 5).state = true)
                then print_string " "
                else print_string "/";
                if ((line - 0) * !x + cpt) = !room_s
                then print_string "ST"
                else
                  begin
                    if ((line - 1) * !x + cpt) = !room_e
                    then print_string "ED"
                    else
                      begin
                        if (!room_list.((line - 1) * !x + cpt).soluce = true)
                             then print_string ".."
                        else print_string "  ";
                      end;
                  end;
                if (line <> 0 && !door_list.(((line - 1) * !x + cpt) * 6 + 1).state = true)
                then print_string " "
                else print_string "\\";
              end;
            print_bord_hexa2 (cpt + 1) size value line;
          end
        else
          begin
            if value = 1
            then
              print_string "/";
            print_endline "";
          end

      ;;

      let rec print_hexa_maze cpt cpt_mod = 
        if (cpt < (!y * 2 + 1))
        then
          begin
            if (cpt == 0)
            then
              begin
                print_string " ";
                print_bord_hexa 0 (!x - 1 + (!x mod 2));
                print_second_bord_hexa 0 !x 0 ((cpt - 1) / 2);
                print_hexa_maze 2 0;
              end
            else
              begin
                if (cpt_mod == 0)
                then
                  begin
                    print_string "\\";
                    if (!x mod 2 = 1)
                    then
                      print_bord_hexa2 0 !x 1 (cpt / 2)
                    else
                      print_bord_hexa2 0 !x 0 (cpt / 2);
                    print_hexa_maze (cpt + 1) 1;
                  end
                else
                  begin
                    if !x mod 2 = 0
                    then
                      print_second_bord_hexa 0 !x 1 ((cpt - 1) / 2)
                    else
                      print_second_bord_hexa 0 !x 0 ((cpt - 1) / 2);
                    print_hexa_maze (cpt + 1) 0;
                  end
              end
          end
        else
          if (cpt < (!y * 2 + 2))
          then
            begin
              print_second_bord_hexa2 0 !x;
              print_hexa_maze (cpt + 1) 0;
            end;
      ;;

      let rec create_door id =
        if (id < !x * !y)
        then
          begin
            !door_list.(id * 4) <- {state = false; d1 = id; d2 = id - !x};
            !door_list.(id * 4 + 1) <- {state = false; d1 = id; d2 = id + 1};
            !door_list.(id * 4 + 2) <- {state = false; d1 = id; d2 = id + !x};
            !door_list.(id * 4 + 3) <- {state = false; d1 = id; d2 = id - 1};
            create_door (id + 1);
          end
      ;;
                          
      let rec create_door_hexa id =
        if (id < !x * !y)
        then
          begin
            if (((id mod !x) mod 2) = 0)
            then
              begin
                !door_list.(id * 6) <- {state = false; d1 = id; d2 = id - !x};
                !door_list.(id * 6 + 1) <- {state = false; d1 = id; d2 = id - !x + 1};
                !door_list.(id * 6 + 2) <- {state = false; d1 = id; d2 = id + 1};
                !door_list.(id * 6 + 3) <- {state = false; d1 = id; d2 = id + !x};
                !door_list.(id * 6 + 4) <- {state = false; d1 = id; d2 = id - 1};
                !door_list.(id * 6 + 5) <- {state = false; d1 = id; d2 = id - !x - 1};
                create_door_hexa (id + 1);
              end
            else
              begin
                !door_list.(id * 6) <- {state = false; d1 = id; d2 = id - !x};
                !door_list.(id * 6 + 1) <- {state = false; d1 = id; d2 = id + 1};
                !door_list.(id * 6 + 2) <- {state = false; d1 = id; d2 = id + !x + 1};
                !door_list.(id * 6 + 3) <- {state = false; d1 = id; d2 = id + !x};
                !door_list.(id * 6 + 4) <- {state = false; d1 = id; d2 = id + !x - 1};
                !door_list.(id * 6 + 5) <- {state = false; d1 = id; d2 = id - 1};
                create_door_hexa (id + 1);
              end
          end
      ;;

    let rec create_room i j cpt =
      if (cpt < (i * j))
      then
        begin
          !room_list.(cpt) <- {check = false; id = cpt; soluce = false};
          create_room i j (cpt + 1);
        end
   ;;

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

   let append l i = l @ [i];;

   let is_door_close elem p1 p2 =
     if ((elem.d1 = p1 && elem.d2 = p2) || (elem.d1 = p2 && elem.d2 = p1))
     then true
     else false
   ;;

   let rec search_door_and_check_state pos =
     if !door_list.(pos).state = false
     then {close = true; pos = pos}
     else {close = false; pos = pos}
   ;;

   let get_case_left pos acc =
     if ((pos) mod !x) > 0
        && (is_check_by_id (pos - 1))
     then
       begin
         let fd = (search_door_and_check_state (4 * (pos - 1) + 1)) in
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
         let fd = (search_door_and_check_state (4 * pos + 1)) in
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
         let fd = (search_door_and_check_state (4 * (pos - !x) + 2)) in
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
         let fd = (search_door_and_check_state (4 * pos + 2)) in
         if fd.close
         then {id = !room_list.(pos + !x).id; pos = fd.pos}::acc
         else acc
       end              
     else
       acc;
   ;;

(*
***************** GET_CASE_HEXA *****************
*)

   let get_case_left_up pos acc =
     if ((pos) mod !x) > 0
        && (!door_list.(pos * 6 + 5).d2 >= 0)
        && (is_check_by_id (!door_list.(pos * 6 + 5).d2))
     then
       begin
         let fd = (search_door_and_check_state (pos * 6 + 5)) in
         if fd.close
         then {id = !door_list.(pos * 6 + 5).d2; pos = fd.pos}::acc
         else acc
       end
     else
       acc;
   ;;

   let get_case_left_down pos acc =
     if ((pos) mod !x) > 0
        && (!door_list.(pos * 6 + 4).d2 < (!x * !y))
        && (is_check_by_id (!door_list.(pos * 6 + 4).d2))
     then
       begin
         let fd = (search_door_and_check_state ((!door_list.(pos * 6 + 4).d2) * 6 + 1)) in
         if fd.close
         then {id = !door_list.(pos * 6 + 4).d2; pos = fd.pos}::acc
         else acc
       end
     else
       acc;
   ;;

   let get_case_right_up pos acc =
     if ((pos < !x && pos mod 2 > 0)
         || (pos >= !x))
        && ((pos + 1) mod !x) > 0
        && (is_check_by_id (!door_list.(pos * 6 + 1).d2))
     then
       begin
         let fd = (search_door_and_check_state (6 * pos + 1)) in
         if fd.close
         then {id = !door_list.(pos * 6 + 1).d2; pos = fd.pos}::acc
         else acc
       end
     else
       acc;
   ;;

   let get_case_right_down pos acc =
     if ((pos + 1) mod !x) > 0
        && (!door_list.(pos * 6 + 2).d2 < (!x * !y))
        && (is_check_by_id (!door_list.(pos * 6 + 2).d2))
     then
       begin
         let fd = (search_door_and_check_state (6 * (!door_list.(pos * 6 + 2).d2) + 5)) in
         if fd.close
         then {id = !door_list.(pos * 6 + 2).d2; pos = fd.pos}::acc
         else acc
       end
     else
       acc;
   ;;

   let get_case_up_h pos acc =
     if (!door_list.(pos * 6).d2 >= 0)
        && (is_check_by_id (!door_list.(pos * 6).d2))
     then
       begin
         let fd = (search_door_and_check_state ((pos - !x) * 6 + 3)) in
         if fd.close
         then {id = !door_list.(pos * 6).d2; pos = fd.pos}::acc
         else acc
       end
     else
       acc;
   ;;

   let get_case_down_h pos acc =
     if (!door_list.(pos * 6 + 3).d2 < (!x * !y))
        && (is_check_by_id (pos + !x))
     then
       begin
         let fd = (search_door_and_check_state (pos * 6 + 3)) in
         if fd.close
         then {id = !door_list.(pos * 6 + 3).d2; pos = fd.pos}::acc
         else acc
       end
     else
       acc;
   ;;


   let get_near_available_room pos square =
     if square then
       (get_case_down pos (get_case_up pos (get_case_right pos (get_case_left pos []))))
     else
       (get_case_down_h pos (get_case_up_h pos (get_case_right_up pos (get_case_right_down pos (get_case_left_up pos (get_case_left_down pos []))))))
   ;;
     
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
         if state
         then !room_list.(pos).check <- true;
         let near_room = get_near_available_room pos !form in
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
   ;;

(*
*************** SOLVE ***********************
*)


   let get_case_left_s pos acc =
     if ((pos) mod !x) > 0
        && not (is_check_by_id (pos - 1))
     then
       begin
         let fd = (search_door_and_check_state (4 * (pos - 1) + 1)) in
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
         let fd = (search_door_and_check_state (4 * pos + 1)) in
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
         let fd = (search_door_and_check_state (4 * (pos - !x) + 2)) in
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
         let fd = (search_door_and_check_state (4 * pos + 2)) in
         if not fd.close
         then {id = !room_list.(pos + !x).id; pos = fd.pos}::acc
         else acc
       end              
     else
       acc;
   ;;

     
(*
**************** GET CASE HEXA **************
*)

   let get_case_left_up_s pos acc =
     if ((pos) mod !x) > 0
        && (!door_list.(pos * 6 + 5).d2 >= 0)
        && not (is_check_by_id (!door_list.(pos * 6 + 5).d2))
     then
       begin
         let fd = (search_door_and_check_state (pos * 6 + 5)) in
         if not fd.close
         then {id = !door_list.(pos * 6 + 5).d2; pos = fd.pos}::acc
         else acc
       end
     else
       acc;
   ;;

   let get_case_left_down_s pos acc =
     if ((pos) mod !x) > 0
        && (!door_list.(pos * 6 + 4).d2 < (!x * !y))
        && not (is_check_by_id (!door_list.(pos * 6 + 4).d2))
     then
       begin
         let fd = (search_door_and_check_state ((!door_list.(pos * 6 + 4).d2) * 6 + 1)) in
         if not fd.close
         then {id = !door_list.(pos * 6 + 4).d2; pos = fd.pos}::acc
         else acc
       end
     else
       acc;
   ;;

   let get_case_right_up_s pos acc =
     if ((pos < !x && pos mod 2 > 0)
         || (pos >= !x))
        && ((pos + 1) mod !x) > 0
        && not (is_check_by_id (!door_list.(pos * 6 + 1).d2))
     then
       begin
         let fd = (search_door_and_check_state (6 * pos + 1)) in
         if not fd.close
         then {id = !door_list.(pos * 6 + 1).d2; pos = fd.pos}::acc
         else acc
       end
     else
       acc;
   ;;

   let get_case_right_down_s pos acc =
     if ((pos + 1) mod !x) > 0
        && (!door_list.(pos * 6 + 2).d2 < (!x * !y))
        && not (is_check_by_id (!door_list.(pos * 6 + 2).d2))
     then
       begin
         let fd = (search_door_and_check_state (6 * (!door_list.(pos * 6 + 2).d2) + 5)) in
         if not fd.close
         then {id = !door_list.(pos * 6 + 2).d2; pos = fd.pos}::acc
         else acc
       end
     else
       acc;
   ;;

   let get_case_up_h_s pos acc =
     if (!door_list.(pos * 6).d2 >= 0)
        && not (is_check_by_id (!door_list.(pos * 6).d2))
     then
       begin
         let fd = (search_door_and_check_state ((pos - !x) * 6 + 3)) in
         if not fd.close
         then {id = !door_list.(pos * 6).d2; pos = fd.pos}::acc
         else acc
       end
     else
       acc;
   ;;

   let get_case_down_h_s pos acc =
     if (!door_list.(pos * 6 + 3).d2 < (!x * !y))
        && not (is_check_by_id (pos + !x))
     then
       begin
         let fd = (search_door_and_check_state (pos * 6 + 3)) in
         if not fd.close
         then {id = !door_list.(pos * 6 + 3).d2; pos = fd.pos}::acc
         else acc
       end
     else
       acc;
   ;;
     
   let get_near_available_room_s pos square =
     if square
     then
       (get_case_down_s pos (get_case_up_s pos (get_case_right_s pos (get_case_left_s pos []))))
     else
       (get_case_down_h_s pos (get_case_up_h_s pos (get_case_right_up_s pos (get_case_right_down_s pos (get_case_left_up_s pos (get_case_left_down_s pos []))))))
   ;;

   let rec solve pos stack_pos =
     if (pos <> !room_e)
     then
       begin
         !room_list.(pos).check <- false;
         let near_room = get_near_available_room_s pos !form in
         if (List.length near_room) > 0
         then
           begin
             Stack.push pos stack_pos;
             let rand_pos = (List.nth near_room (Random.int (List.length near_room))) in
             solve rand_pos.id stack_pos;
           end
         else
           solve (Stack.pop stack_pos) stack_pos
       end
     else
       stack_pos
   ;;
     
(*
**************** INIT *********************
*)

    let set_x arg_x =
      x := arg_x;;
     
    let set_y arg_y =
      y := arg_y;;

    let test a =
      print_endline "carreee";
    ;;

    let init() =
      begin
        Random.self_init ();
        let keyword = [("-c", Arg.Int (set_x), "Set x"); ("-r", Arg.Int (set_y), "Set y"); ("--square", Arg.Set (form), "Choice form"); ("--hexagonal", Arg.Clear (form), "Choice form")] in
        let usage_words = "-c [NUMBER OF COLUMNS] -r [NUMBER OF RAWS]" in
        if (Array.length Sys.argv - 1 = 0) (* || Array.length Sys.argv - 1 = 1)*)
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
            if (Array.length Sys.argv - 1 = 0)
            then
              form := false;
            room_list := (Array.make (!x * !y) ({check = false; id = 0; soluce = false}));
            create_room !x !y 0;
            if (!form = true)
            then
              begin
                door_list := (Array.make (!x * !y * 4) ({state = false; d1 = 0; d2 = 0}));
                create_door 0;
                room_s := (Random.int (!x * !y));
                room_s := !room_s - (!room_s mod !x);
                room_e := (Random.int (!x * !y));
                room_e := !room_e - (!x * ((!room_e / !x) mod !y));
                if (!room_s = !room_e)
                then
                  room_s := !room_s + !x;
                gen_maze (get_rand_case (Random.int (Array.length !room_list))) (Stack.create()) true;
                let stack_pos = (solve !room_s (Stack.create())) in
                let list_pos = stack_to_list (Stack.top stack_pos) stack_pos in
                List.iter set_soluce list_pos;
                print_bord (!x * 3 + 1);
                print_maze 0 list_pos;
              end
            else
              begin
                door_list := (Array.make (!x * !y * 6) ({state = false; d1 = 0; d2 = 0}));
                create_door_hexa 0;
                room_s := (Random.int (!x * !y));
                room_s := !room_s - (!room_s mod !x);
                room_e := (Random.int (!x * !y));
                room_e := !room_e - (!x * ((!room_e / !x) mod !y));
                if (!room_s = !room_e)
                then
                  room_s := !room_s + !x;
                gen_maze (get_rand_case (Random.int (Array.length !room_list))) (Stack.create()) true;
                let stack_pos = (solve !room_s (Stack.create())) in
                let list_pos = stack_to_list (Stack.top stack_pos) stack_pos in
                List.iter set_soluce list_pos;
                print_hexa_maze 0 0;
              end
          end
      end
    ;;
  end
;;
