open Lwt
open Str

type token  = |X |O |EMPTY
	
type game_state = |WIN of token |DRAW |CONTINUE
	
let empty_board = [|
					[|EMPTY; EMPTY; EMPTY|];
					[|EMPTY; EMPTY; EMPTY|];
					[|EMPTY; EMPTY; EMPTY|]|]
					
let ask_to_wait player = 
	send_to_client player "Your opponent turn. Please, wait";
	();;


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
		| _ -> false;;		

let make_move x y board token=
	 board.(y).(x) <- token;
	 board;;

let  rec ask_for_move player board =
	player >>= fun (descriptor, token) ->
	send_to_client descriptor "Your move: ";
	let move = read_from_client descriptor in 
		move >>= fun str ->
		match (check_if_correct str board) with
		|false ->
				send_to_client descriptor "Unknow command. Try again (F.e TURN 1 2)";
				ask_for_move player board;
		|true ->
				match  Str.split (Str.regexp " ") str with
						|hd :: arg1 :: arg2 :: [] ->
						let x = int_of_string arg1 in
						let y = int_of_string arg2 in
				let updated_board = 
					make_move x y board token in 
				Lwt.return updated_board;;

let there_are_empty_cells board =
	let is_empty cell =
			match cell with EMPTY -> true | _ -> false in
	let listed = Array.to_list (Array.map Array.to_list board) in
	(List.length (List.filter is_empty (List.flatten listed))) > 0
	
let check_win board =
	match board with
 | [|[|a; _; _|];
       [|_; b; _|];
       [|_; _; c|]|] when a = b && b = c -> WIN a
    | [|[|_; _; a|];
       [|_; b; _|];
       [|c; _; _|]|] when a = b && b = c -> WIN a

    | [|[|a; b; c|]; 
       [|_; _; _|];
       [|_; _; _|]|] when a = b && b = c -> WIN a
    | [|[|_; _; _|];
       [|a; b; c|];
       [|_; _; _|]|] when a = b && b = c -> WIN a
    | [|[|_; _; _|];
       [|_; _; _|];
       [|a; b; c|]|] when a = b && b = c -> WIN a

    | [|[|a; _; _|];
       [|b; _; _|];
       [|c; _; _|]|] when a = b && b = c -> WIN a
    | [|[|_; a; _|];
       [|_; b; _|];
       [|_; c; _|]|] when a = b && b = c -> WIN a
    | [|[|_; _; a|];
       [|_; _; b|];
       [|_; _; c|]|] when a = b && b = c -> WIN a
       
    | board when there_are_empty_cells board-> DRAW
		
		|	board -> CONTINUE;;

let final_game player1 player2 game_state =
	let close_channel chan = 
		chan >>= fun descriptor ->
		Lwt_unix.shutdown descriptor SHUTDOWN_ALL;
		Lwt.return () in
	match game_state with
	| WIN X |WIN O ->
  	send_to_client player1 "You won";
  	send_to_client player2 "You lost";
		close_channel player1;
		close_channel player2
	|DRAW -> 
		send_to_client player1 "DRAW! THANKS FOR THE GAME!";
		send_to_client player2 "DRAW! THANKS FOR THE GAME!";
		close_channel player1;
		close_channel player2

let rec game_loop game_variables = 
	game_variables >>= fun (player1, player2, board) ->
	match check_win board with
	|WIN X -> 
		final_game player1 player2;
		Lwt.return ()
	|WIN O ->
		final_game player2 player1;
		Lwt.return ()
	|CONTINUE ->
		let is_empty cell =
			match cell with EMPTY -> true | _ -> false in
		let listed = Array.to_list (Array.map Array.to_list board) in
		let current_player = 
				if ((List.length (List.filter is_empty (List.flatten listed))) mod 2) = 1 
					then 
						(ask_to_wait player2;
						Lwt.return (player1, X))						
					else
						(ask_to_wait player1;
						Lwt.return (player2, O)) in		
		let after_turn_board = ask_for_move current_player board in
		after_turn_board>>= fun board ->
		game_loop (Lwt.return (player1, player2, board))
		
		|DRAW -> final_game player1 player2 DRAW;;		
				
	
let prepare_game_process pair_of_players= 
	pair_of_players >>= fun (player1, player2) ->	
	send_to_client player1 "You play for X";
	send_to_client player2 "You play for O";
	game_loop (Lwt.return (player1, player2, empty_board));;
	
