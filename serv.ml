open Lwt
open Lwt_io
open Str
open Array

(*
#use "topfind";;

#require "lwt.simple-top";;

#require "str";;

open Lwt;;

open Str;;

*)



type token  = |X |O |EMPTY
	
type game_state = |WIN of token |DRAW |CONTINUE

let token_to_str token = 
	match token with 
	| EMPTY -> " "
	| X -> "X"
	| O -> "O"

let empty_board n =
	Array.make 3 [|EMPTY; EMPTY; EMPTY|]	
	
let border = String.make 70 '#'

let read_from_client client= 
	client >>= fun cli ->
	let chan = Lwt_io.(of_fd ~mode:input cli) in
	Lwt_io.read_line chan
	
let send_to_client client msg=
	client >>= fun cli->
	let chan = Lwt_io.(of_fd ~mode:output cli) in
	Lwt_io.fprintf chan "SERVER MESSAGE: %s\n" msg;
	Lwt_io.flush chan

let board_to_str board = 
	let listed = Array.to_list (Array.map Array.to_list board) in
	let header = "\n   0   1   2	\n"	in
	let separator = "  ---+---+---\n" in 	
	let row_to_str lst = 
		 String.concat " | " (List.map token_to_str lst) ^ "\n" in
	let rec form_string start_with lst =
		match lst with 
		| [] -> ""
		| hd :: tl -> 
			separator ^ string_of_int start_with ^ "  " ^ 
									(row_to_str hd) ^ form_string (start_with+1) tl in
	header ^ (form_string 0 listed)
	
let send_board_to_player player board =
	 let str = board_to_str board in 
		send_to_client player str

let sock = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0

let setting_up_server_socket = 	
	let sockaddr = (Unix.ADDR_INET(Unix.inet_addr_of_string "127.0.0.1", 23233)) in
	Lwt_unix.set_close_on_exec sock;
	Lwt_unix.setsockopt sock Unix.SO_REUSEADDR true;
	Lwt_unix.bind sock sockaddr;
	Lwt_unix.listen sock 20


let ask_to_wait player = 
	send_to_client player "Your opponent turn. Please, wait" |> ignore;
	()

let check_if_correct str board = 
	let cell_is_empty x y board =
		match Array.get (Array.get board (int_of_string y)) (int_of_string x) with
		EMPTY -> true
		|_ -> false in		
	let is_in_board_range coordinate =
		(int_of_string coordinate <= 2) && (int_of_string coordinate >= 0) in		
	let command = Str.split (Str.regexp " ") str in	
	match command with
		|hd :: arg1 :: arg2 :: [] ->
			if (hd = "TURN" && 
				is_in_board_range arg1 &&  
				is_in_board_range arg2 &&
				cell_is_empty arg1 arg2 board)
					then true
					else false
		| _ -> false		

let make_move x y board token =
	let private_board = Array.map (Array.copy) board in
	 private_board.(y).(x) <- token;
	 private_board

let rec ask_for_move player token board =
	send_to_client player "Your move: " |> ignore;
	send_board_to_player player board;
	let move = read_from_client player in 
		move >>= fun str ->
		match (check_if_correct str board) with
		|false ->
				send_to_client player "Wrong move. Try again (F.e TURN 1 2)" |> ignore;
				ask_for_move player token board;
		|true ->
				match  Str.split (Str.regexp " ") str with
						|hd :: arg1 :: arg2 :: [] ->
						let x = int_of_string arg1 in
						let y = int_of_string arg2 in
					Lwt.return(make_move x y board token)

let there_are_empty_cells board =
	let is_empty cell =
			match cell with EMPTY -> true | _ -> false in
	let listed = Array.to_list (Array.map Array.to_list board) in
	(List.length (List.filter is_empty (List.flatten listed))) > 0
	
let check_win board =
	match board with
 | [|[|a; _; _|];
       [|_; b; _|];
       [|_; _; c|]|] when (a = b && b = c && a <> EMPTY) -> WIN a
    | [|[|_; _; a|];
       [|_; b; _|];
       [|c; _; _|]|] when (a = b && b = c && a <> EMPTY) -> WIN a

    | [|[|a; b; c|]; 
       [|_; _; _|];
       [|_; _; _|]|] when (a = b && b = c && a <> EMPTY) -> WIN a
    | [|[|_; _; _|];
       [|a; b; c|];
       [|_; _; _|]|] when (a = b && b = c && a <> EMPTY) -> WIN a
    | [|[|_; _; _|];
       [|_; _; _|];
       [|a; b; c|]|] when (a = b && b = c && a <> EMPTY) -> WIN a

    | [|[|a; _; _|];
       [|b; _; _|];
       [|c; _; _|]|] when (a = b && b = c && a <> EMPTY) -> WIN a
    | [|[|_; a; _|];
       [|_; b; _|];
       [|_; c; _|]|] when (a = b && b = c && a <> EMPTY) -> WIN a
    | [|[|_; _; a|];
       [|_; _; b|];
       [|_; _; c|]|] when (a = b && b = c && a <> EMPTY) -> WIN a
       
    | board when there_are_empty_cells board-> CONTINUE
		
	|	board -> DRAW

let final_game player1 player2 game_state board=
	let close_channel chan = 
		chan >>= fun descriptor ->
		Lwt_unix.shutdown descriptor SHUTDOWN_ALL;
		Lwt.return () in
	match game_state with
	| WIN X |WIN O ->
		send_board_to_player player1 board;
  	send_to_client player1 "You won!" |> ignore;
		send_board_to_player player2 board;
  	send_to_client player2 "You lost!" |> ignore;		
		close_channel player1;
		close_channel player2
	|DRAW -> 
		send_board_to_player player1 board;
		send_board_to_player player2 board;
		send_to_client player1 "DRAW! THANKS FOR THE GAME!" |> ignore;
		send_to_client player2 "DRAW! THANKS FOR THE GAME!" |> ignore;
		close_channel player1;
		close_channel player2

let rec game_loop player1 player2 board = 
	send_to_client player1 border;
	send_to_client player2 border;
	match check_win board with
	|WIN X ->
		final_game player1 player2 (WIN X) board;
	|WIN O ->
		final_game player2 player1 (WIN O) board;
	|CONTINUE ->
		let is_empty cell =
			match cell with EMPTY -> true | _ -> false in
		let listed = Array.to_list (Array.map Array.to_list board) in
		let current_player, token = 
				if ((List.length (List.filter is_empty (List.flatten listed))) mod 2) = 1 
					then 
						(ask_to_wait player2;
						send_board_to_player player2 board;
						player1, X)						
					else
						(ask_to_wait player1;
						send_board_to_player player1 board;
						 player2, O) in		
		let updated_board = ask_for_move current_player token board in
		updated_board >>= fun brd ->
		game_loop player1 player2 brd;
		Lwt.return()
		|DRAW -> final_game player1 player2 DRAW board		

let waiting_players = 
	Lwt_mvar.create_empty ()
	
let prepare_game_process pair_of_players= 
	pair_of_players >>= fun (player1, player2) ->	
	send_to_client player1 "You play for X" |>ignore ;
	send_to_client player2 "You play for O" |>ignore ;
	game_loop player1 player2 (empty_board 3)

let rec form_pairs () = 
	let player1 = Lwt_mvar.take waiting_players in 
		player1 >>= fun descriptor1 ->		
	let player2 = Lwt_mvar.take waiting_players in 
		player2 >>= fun descriptor2->
	prepare_game_process (Lwt.return (descriptor1, descriptor2)) |> ignore;
	form_pairs ()
	
let rec make_ready player = 	
		send_to_client player "Type in START";
		let answer = read_from_client player in 
			answer >>= fun str ->
			match str with
				|"START" -> 				
					Lwt_mvar.put waiting_players player;
					send_to_client player "Looking for opponent";
				| _ -> 
					send_to_client player "Unknown command. try again";
					make_ready player

let rec handle_income () =
	let in_conection = Lwt_unix.accept sock in 
	in_conection >>= fun (cli, addr) ->
	let player = Lwt.return cli in
	send_to_client player "Welcome to the server. To start game type in START and press Enter";
	make_ready player;
	handle_income () 

let _ = Lwt_main.run (
	form_pairs ();
	handle_income ());;
