use crate::C;
use crate::Square;


#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Piece {
    King(f32, C),
    Queen(f32, C),
    Bishop(f32, C),
    Knight(f32, C),
    Rook(f32, C),
    Pawn(f32, C),
}

impl Piece {
    pub fn square(&self) -> Square {
        Square {
            pieces: vec![match self {
                Piece::King(_, c) => Piece::King(100.0, *c),
                Piece::Queen(_, c) => Piece::Queen(100.0, *c),
                Piece::Bishop(_, c) => Piece::Bishop(100.0, *c),
                Piece::Knight(_, c) => Piece::Knight(100.0, *c),
                Piece::Rook(_, c) => Piece::Rook(100.0, *c),
                Piece::Pawn(_, c) => Piece::Pawn(100.0, *c),
            }],
        }
    }

    pub fn with_value(&self, val: f32) -> Piece {
        return match self {
            Piece::King(_, c) => Piece::King(val, *c),
            Piece::Queen(_, c) => Piece::Queen(val, *c),
            Piece::Bishop(_, c) => Piece::Bishop(val, *c),
            Piece::Knight(_, c) => Piece::Knight(val, *c),
            Piece::Rook(_, c) => Piece::Rook(val, *c),
            Piece::Pawn(_, c) => Piece::Pawn(val, *c),
        };
    }

    pub fn value(&self) -> f32 {
        match self {
            Piece::King(a, _) => *a,
            Piece::Queen(a, _) => *a,
            Piece::Bishop(a, _) => *a,
            Piece::Knight(a, _) => *a,
            Piece::Rook(a, _) => *a,
            Piece::Pawn(a, _) => *a,
        }
    }

    pub fn weight(&self) -> f32 {
        match self {
            Piece::King(a, _) => 20.0,
            Piece::Queen(a, _) => 7.0,
            Piece::Bishop(a, _) => 3.0,
            Piece::Knight(a, _) => 3.0,
            Piece::Rook(a, _) => 5.0,
            Piece::Pawn(a, _) => 2.0,
        }
    }

    pub fn color(&self) -> C {
        match self {
            Piece::King(_, c) => *c,
            Piece::Queen(_, c) => *c,
            Piece::Bishop(_, c) => *c,
            Piece::Knight(_, c) => *c,
            Piece::Rook(_, c) => *c,
            Piece::Pawn(_, c) => *c,
        }
    }

    pub fn text(&self, unicode: bool) -> String {
        if unicode {
            return match self {
                Piece::Pawn(_, _) => "♙".to_owned(),
                Piece::Bishop(_, _) => "♝".to_owned(),
                Piece::King(_, _) => "♚".to_owned(),
                Piece::Queen(_, _) => "♕".to_owned(),
                Piece::Knight(_, _) => "♞".to_owned(),
                Piece::Rook(_, _) => "♜".to_owned(),
            };
        } else {
            return match self {
                Piece::Pawn(_, _) => "p".to_owned(),
                Piece::Bishop(_, _) => "b".to_owned(),
                Piece::King(_, _) => "k".to_owned(),
                Piece::Queen(_, _) => "q".to_owned(),
                Piece::Knight(_, _) => "n".to_owned(),
                Piece::Rook(_, _) => "r".to_owned(),
            };
        }
    }
    pub fn coord(&self) -> u16 {
        return match self {
            Piece::King(_, _) => 0,
            Piece::Queen(_, _) => 1,
            Piece::Bishop(_, _) => 2,
            Piece::Knight(_, _) => 3,
            Piece::Rook(_, _) => 4,
            Piece::Pawn(_, _) => 5,
        };
    }
}