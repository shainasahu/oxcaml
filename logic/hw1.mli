open! Core

type player_kind =
  | P1
  | P2

type suit =
  | Hearts
  | Diamonds
  | Clubs
  | Spades

type rank =
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace

type card =
  { rank : rank
  ; suit : suit
  }

type decision =
  | In_progress of
      { whose_turn : player_kind
      ; declared_suit : suit option
      }
  | Winner of player_kind

type game_state =
  { hands : (player_kind * card list) list
  ; discard_pile : card list
  ; deck : card list
  ; decision : decision
  }

type move =
  | Play of card list
  | Draw_and_maybe_play of card option

val initial_state : game_state
val move_play_5h : move
val state_after_play_5h : game_state
val move_draw_none : move
val state_after_draw_none : game_state
val before_terminal_state : game_state
val move_to_terminal_state : move
val terminal_state : game_state
