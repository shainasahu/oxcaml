open! Core

type player_kind =
  | P1
  | P2

type suit = Hearts | Diamonds | Clubs | Spades
type rank =
  | Two | Three | Four | Five | Six | Seven
  | Eight | Nine | Ten | Jack | Queen | King | Ace

type card = { rank : rank; suit : suit }

type decision =
  | In_progress of { whose_turn : player_kind }
  | Winner of player_kind

type game_state =
  { hands : (player_kind * card list) list
  ; discard_pile : card list
  ; deck : card list
  ; decision : decision
  }

(* A move is either: 
   - Play a list of cards (multiples allowed if same rank)
   - Or draw one card and play it if applicable*)
type move =
  | Play of card list
  | Draw_and_maybe_play of card option

let initial_state : game_state =
  { hands = [ (P1, [ {rank = Five; suit = Hearts}; {rank = Seven; suit = Spades} ]); 
              (P2, [ {rank = Four; suit = Diamonds}; {rank = Eight; suit = Clubs} ])]
  ; discard_pile = []
  ; deck = [ {rank = Two; suit = Clubs}; 
             {rank = Three; suit = Diamonds}; 
            { rank = Nine; suit = Spades} ]
  ; decision = In_progress { whose_turn = P1 }
  }
;;

let move_play_5h : move = Play [ {rank = Five; suit = Hearts} ]

let state_after_play_5h : game_state =
  { hands =
      [ P1, [ {rank = Seven; suit = Spades} ]
      ; P2, [ {rank = Four; suit = Diamonds}; {rank = Eight; suit = Clubs} ]
      ]
  ; discard_pile = [ {rank = Five; suit = Hearts} ]
  ; deck = [ {rank = Two; suit = Clubs}; 
             {rank = Three; suit = Diamonds}; 
             {rank = Nine; suit = Spades} ]
  ; decision = In_progress { whose_turn = P2 }
  }
;;

let move_draw_none : move = Draw_and_maybe_play None

let state_after_draw_none : game_state =
  { hands =
      [ P1, [ {rank = Seven; suit = Spades} ]
      ; P2, [ {rank = Four; suit = Diamonds}; 
              {rank = Eight; suit = Clubs}; 
              {rank = Two; suit = Clubs} ]
      ]
  ; discard_pile = [ {rank = Five; suit = Hearts} ]
  ; deck = [ {rank = Three; suit = Diamonds}; {rank = Nine; suit = Spades} ]
  ; decision = In_progress { whose_turn = P1 }
  }
;;

let before_terminal_state : game_state =
  { hands =
      [ P1, [ {rank = Seven; suit = Spades} ]
      ; P2, [ {rank = Four; suit = Diamonds}; 
              {rank = Eight; suit = Clubs}; 
              {rank = Two; suit = Clubs} ]
      ]
  ; discard_pile = [ {rank = Five; suit = Hearts} ]
  ; deck = [ {rank = Three; suit = Diamonds}; {rank = Nine; suit = Spades} ]
  ; decision = In_progress { whose_turn = P1 }
  }
;;

let move_to_terminal_state : move = Play [ {rank = Seven; suit = Spades} ]

let terminal_state : game_state =
  { hands =
      [ P1, []
      ; P2, [ {rank = Four; suit = Diamonds}; 
              {rank = Eight; suit = Clubs}; 
              {rank = Two; suit = Clubs} ]
      ]
  ; discard_pile = [ {rank = Seven; suit = Spades}; {rank = Five; suit = Hearts} ]
  ; deck = [ {rank = Three; suit = Diamonds}; {rank = Nine; suit = Spades} ]
  ; decision = Winner P1
  }
;;