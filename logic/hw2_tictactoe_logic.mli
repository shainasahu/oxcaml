open! Core

module Player_kind : sig
  type t =
    | P1
    | P2
  [@@deriving sexp, compare, equal]

  val opposite : t -> t
end

module Suit : sig
  type t =
    | Hearts
    | Diamonds
    | Clubs
    | Spades
  [@@deriving sexp, compare, equal, enumerate]
end

module Rank : sig
  type t =
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
  [@@deriving sexp, compare, equal, enumerate]
end

module Card : sig
  type t =
    { rank : Rank.t
    ; suit : Suit.t
    }
  [@@deriving sexp, compare, equal]
end

module Decision : sig
  type t =
    | In_progress of
        { whose_turn : Player_kind.t
        ; declared_suit : Suit.t option
        }
    | Winner of Player_kind.t
  [@@deriving sexp, compare, equal]

  val is_game_over : t -> bool
end

module Move : sig
  type t =
    | Play of Card.t list
    | Draw_and_maybe_play of Card.t option
    (** draw one card from the deck, if playable, must play *)
  [@@deriving sexp, compare, equal]
end

module Game_state : sig
  type t =
    { hands : (Player_kind.t * Card.t list) list
    ; discard_pile : Card.t list
    ; deck : Card.t list
    ; decision : Decision.t
    }
  [@@deriving sexp, compare, equal]

  module Create_error : sig
    type t = Invalid_initial_setup [@@deriving sexp, compare]
  end

  val create
    :  hands:(Player_kind.t * Card.t list) list
    -> deck:Card.t list
    -> discard_pile:Card.t list
    -> decision:Decision.t
    -> (t, Create_error.t list) Result.t

  module Move_error : sig
    type t =
      | Game_is_over
      | Card_not_in_hand
      | Invalid_play
      | Must_play_if_possible
      | Must_play_all_same_rank
      | Deck_empty
    [@@deriving sexp, compare]
  end

  val get_all_valid_moves : t -> Move.t list
  val make_move : t -> Move.t -> (t, Move_error.t) Result.t
  val top_discard : t -> Card.t option
  val create_random_initial_state : ?hand_size:int -> unit -> t

  module For_testing : sig
    val sample_state : t
    val empty_deck_state : t
  end
end
