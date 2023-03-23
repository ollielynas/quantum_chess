use random_number::random;
use rgb2ansi256::rgb_to_ansi256;
use std::{
    collections::hash_map::RandomState,
    io::{self, stdout, Write},
};

use crossterm::{
    cursor::{self, Hide},
    event::{self, read, EnableMouseCapture, Event, KeyCode, KeyModifiers},
    execute, queue,
    style::{self, Color, Print, ResetColor, SetBackgroundColor, SetForegroundColor, Stylize},
    terminal::{self, Clear, ClearType, SetSize, SetTitle},
    ExecutableCommand, QueueableCommand, Result,
};



#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum C {
    Black,
    White,
}
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum Action {
    Observe,
    Manipulate,
}

impl C {
    fn color_val(&self) -> Color {
        // match self {
        //     C::Black => Color::AnsiValue(208),
        //     C::White => Color::AnsiValue(34),
        // }
        // match self {
        //     C::Black => Color::Black,
        //     C::White => Color::White,
        // }
        match self {
            C::Black => Color::Black,
            C::White => Color::White,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
enum Piece {
    King(f32, C),
    Queen(f32, C),
    Bishop(f32, C),
    Knight(f32, C),
    Rook(f32, C),
    Pawn(f32, C),
}

impl Piece {
    fn square(&self) -> Square {
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

    fn with_value(&self, val: f32) -> Piece {
        return match self {
            Piece::King(_, c) => Piece::King(val, *c),
            Piece::Queen(_, c) => Piece::Queen(val, *c),
            Piece::Bishop(_, c) => Piece::Bishop(val, *c),
            Piece::Knight(_, c) => Piece::Knight(val, *c),
            Piece::Rook(_, c) => Piece::Rook(val, *c),
            Piece::Pawn(_, c) => Piece::Pawn(val, *c),
        };
    }

    fn value(&self) -> f32 {
        match self {
            Piece::King(a, _) => *a,
            Piece::Queen(a, _) => *a,
            Piece::Bishop(a, _) => *a,
            Piece::Knight(a, _) => *a,
            Piece::Rook(a, _) => *a,
            Piece::Pawn(a, _) => *a,
        }
    }

    fn weight(&self) -> f32 {
        match self {
            Piece::King(a, _) => 20.0,
            Piece::Queen(a, _) => 7.0,
            Piece::Bishop(a, _) => 3.0,
            Piece::Knight(a, _) => 3.0,
            Piece::Rook(a, _) => 5.0,
            Piece::Pawn(a, _) => 2.0,
        }
    }

    fn color(&self) -> C {
        match self {
            Piece::King(_, c) => *c,
            Piece::Queen(_, c) => *c,
            Piece::Bishop(_, c) => *c,
            Piece::Knight(_, c) => *c,
            Piece::Rook(_, c) => *c,
            Piece::Pawn(_, c) => *c,
        }
    }

    fn text(&self, unicode: bool) -> String {
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
    fn coord(&self) -> u16 {
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
#[derive(Debug, Clone, PartialEq)]
struct Square {
    pieces: Vec<Piece>,
}

impl Square {
    fn new_blank() -> Square {
        return Square { pieces: Vec::new() };
    }

    fn add_piece(&mut self, piece: Piece) {
        let mut n = 100;
        for (i, p) in self.pieces.iter().enumerate() {
            if p.coord() == piece.coord() && p.color() == piece.color() {
                n = i;
            }
        }
        if n != 100 {
            self.pieces[n] = self.pieces[n].with_value(self.pieces[n].value() + piece.value());
            return;
        }
        self.pieces.push(piece);
    }

    fn minus_piece(&mut self, piece: Piece) {
        let mut n = 100;
        for (i, p) in self.pieces.iter().enumerate() {
            if p.coord() == piece.coord() && p.color() == piece.color() {
                n = i;
                break;
            }
        }
        if n != 100 {
            if self.pieces[n].value() - piece.value() < 0.0 {
                print!("cannot set probably below 0")
            }
            self.pieces[n] =
                self.pieces[n].with_value((self.pieces[n].value() - piece.value()).max(0.0));
            return;
        }
    }
}
fn truncate(s: &str, max_chars: usize) -> &str {
    match s.char_indices().nth(max_chars) {
        None => s,
        Some((idx, _)) => &s[..idx],
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
struct Pos {
    x: i32,
    y: i32,
}

impl Pos {
    fn new<T: Into<i32>>(x: T, y: T) -> Pos {
        Pos {
            x: x.into(),
            y: y.into(),
        }
    }
    fn add<T: Into<i32>>(&self, x: T, y: T) -> Pos {
        Pos {
            x: self.x + x.into(),
            y: self.y + y.into(),
        }
    }
}
#[derive(Debug, Clone, PartialEq)]

struct Board {
    grid: [[Square; 8]; 8],
    current_player: C,
    action: Action,
    unicode: bool,
    winner: Option<C>,
    bot:bool,
    last_move: Pos,
}

impl Board {
    fn get(&self, pos: Pos) -> Option<Square> {
        if (0..8).contains(&pos.x) && (0..8).contains(&pos.y) {
            return Some(self.grid[pos.x as usize][pos.y as usize].clone());
        }
        return None;
    }

    fn get_moves(&self, pos: Pos) -> Vec<(Pos, Piece)> {
        let mut move_list: Vec<(Pos, Piece)> = vec![];

        if let Some(s) = self.get(pos) {
            if s.pieces.len() == 1
                && s.pieces[0].value() == 100.0
                && s.pieces[0].color() == self.current_player
            {
                return vec![];
            }
        };

        for (x, y) in [
            (1, 2),
            (1, -2),
            (-1, 2),
            (-1, -2),
            (2, 1),
            (-2, 1),
            (2, -1),
            (-2, -1),
        ] {
            let new_pos = pos.add(x, y);
            let square = self.get(new_pos);
            if let Some(s) = &square {
                for piece in &s.pieces {
                    if let Piece::Knight(a, c) = piece {
                        if c == &self.current_player {
                            move_list.push((pos.add(x, y), Piece::Knight(*a, *c)))
                        }
                    }
                }
            }
        }

        for (x, y) in [
            (1, 0),
            (-1, 0),
            (0, 1),
            (0, -1),
            (1, 1),
            (-1, 1),
            (-1, -1),
            (1, -1),
        ] {
            for i in 1..8 {
                let mut break_later = false;
                let new_pos = pos.add(i * x, i * y);
                let square = self.get(new_pos);
                if let Some(s) = &square {
                    if s.pieces.len() == 1 && s.pieces[0].value() == 100.0 {
                        if s.pieces[0].color() != self.current_player {
                            break;
                        } else {
                            break_later = true;
                        }
                    }
                };
                match square {
                    Some(a) => {
                        for piece in a.pieces {
                            match piece {
                                Piece::Queen(n, c) if c == self.current_player => {
                                    move_list.push((new_pos, piece));
                                }
                                Piece::Pawn(n, c)
                                    if c == self.current_player
                                        && x == 0
                                        && i == 1
                                        && y == match self.current_player {
                                            C::Black => -1,
                                            C::White => 1,
                                        } =>
                                {
                                    move_list.push((new_pos, piece));
                                }
                                Piece::Pawn(n, c)
                                    if c == self.current_player
                                        && x == 0
                                        && i == 2
                                        && new_pos.y
                                            == match self.current_player {
                                                C::Black => 1,
                                                C::White => 6,
                                            }
                                        && y == match self.current_player {
                                            C::Black => -1,
                                            C::White => 1,
                                        } 
                                        && self.get(pos) != None
                                        && self.get(pos).unwrap().pieces.len() == 0=>
                                {
                                    move_list.push((new_pos, piece));
                                }
                                Piece::Pawn(n, c)
                                    if c == self.current_player
                                        && x != 0
                                        && i == 1
                                        && y == match self.current_player {
                                            C::Black => -1,
                                            C::White => 1,
                                        }
                                        && self.get(pos) != None
                                        && self.get(pos).unwrap().pieces.len() > 0 =>
                                {
                                    move_list.push((new_pos, piece));
                                }
                                Piece::Rook(n, c)
                                    if c == self.current_player && [x, y].contains(&0) =>
                                {
                                    move_list.push((new_pos, piece));
                                }
                                Piece::Bishop(n, c)
                                    if c == self.current_player && ![x, y].contains(&0) =>
                                {
                                    move_list.push((new_pos, piece));
                                }
                                Piece::King(n, c) if c == self.current_player && i == 1 => {
                                    move_list.push((new_pos, piece));
                                }
                                _ => {}
                            }
                        }
                    }
                    None => break,
                }
                if break_later {
                    break;
                }
            }
        }
        return move_list;
    }

    fn eval(&mut self) -> f32 {
        let mut score = 0.0;
        let mut kings:Vec<(Pos, f32, C)> = vec![];
        // if self.winner == Some(C::Black) {
        //     return 999999.99
        // }
        // if self.winner == Some(C::White) {
        //     return -999999.99
        // }

        for x in 0..8 {
            for y in 0..8 {
                let position = Pos::new(x,y);
                if let Some(sq) = self.get(position) {
                    for p in &sq.pieces {
                    if p.coord() == 0 {
                        kings.push((position, p.value(), p.color()));
                    }
                }
                }
        }}

        for x in 0..8 {
            for y in 0..8 {
                let position = Pos::new(x,y);
                let square = self.get(position).unwrap();
                let total: f32 = square.pieces.iter().map(|f| f.value()).sum::<f32>().max(100.0);
                let mut square_value = 0.0;
                let og_color = self.current_player;
                for i in &square.pieces {
                    square_value += i.value()/total * i.weight() * 100.0 * match i.color() {C::Black => 1.0, C::White => -1.0};
                    if i.coord() == 0 {
                        score += match i.color() {C::Black => 1.0, C::White => -1.0} *10.0;
                    }
                }
                score += square_value;
                if square_value < 0.0 {
                self.current_player = C::Black;
                for (_,p) in self.get_moves(position) {
                    score -= (square_value/100.0) * (p.value());
                    score += 1.0;
                    for k in &kings {
                        if k.2 == C::White {
                            score += k.1 / 1000.0 * ((position.x - k.0.x)as f32).abs()+((position.y - k.0.y)as f32).abs()
                        }
                    }
                }
            }
            if square_value > 0.0 {
                self.current_player = C::White;
                for (_,p) in self.get_moves(position) {
                    score -= 1.0;
                    score -= (square_value/100.0) * (p.value());
                    for (_,p) in self.get_moves(position) {
                    score -= (square_value/100.0) * (p.value());
                    for k in &kings {
                        if k.2 == C::Black {
                            score -= k.1 / 1000.0 * ((position.x - k.0.x)as f32).abs()+((position.y - k.0.y)as f32).abs()
                        }
                    }
                }
            }
            }
                
                self.current_player = og_color;
            }
        }

        return score
    } 

    fn bot_move(&mut self) {
        let og = self.clone();
        let mut moves: Vec<(Action, Pos, f32)> = vec![];
        for x in 0..8 {
            for y in 0..8 {
                let pos = Pos::new(x,y);
                if let Some(p) = self.get(pos).unwrap().pieces.get(0)  {
                    let mut ob_score = 0.0;
                    if p.value() < 100.0 {
                    for _ in 0..10 {
                        self.action = Action::Observe;
                        self.update(pos, true, (0,0), false);
                        ob_score += self.eval();
                        self.new_state(og.clone());
                    }
                    moves.push((Action::Observe, pos, ob_score/10.0));
                }
                }

                self.action = Action::Manipulate;
                if self.get_moves(pos) != vec![] {
                    self.update(pos, true, (0,0), false);
                    moves.push((Action::Manipulate, pos, self.eval()));
                }
                self.new_state(og.clone());
            }
        }

        if moves.len() == 0 {
            self.winner = Some(C::White);
            return
        }

        moves.sort_by_key(|x| -x.2 as i32);
        let before_action = og.action;
        self.action = moves[0].0;
        self.update(moves[0].1, true, (0,0), true);
        self.action = before_action;
    }

    fn new_state(&mut self, new:Board) {
        self.action = new.action;
        self.grid = new.grid;
        // self.unicode = new.unicode;
        self.current_player = new.current_player;
        self.winner = new.winner;
        self.last_move = new.last_move;
    }

    fn update(&mut self, mut mouse: Pos, click: bool, mouse_coords: (u16, u16), render:bool) -> Result<()> {
        let mut stdout = stdout();

        if let Some(x) = self.winner {
            if self.get(mouse) != None {
                mouse = Pos::new(20, 20);
            }
            if click {
                let new = Board::new();
                self.new_state(new);
            }
        }

        let mut stats = [
            [0.0, 0.0],
            [0.0, 0.0],
            [0.0, 0.0],
            [0.0, 0.0],
            [0.0, 0.0],
            [0.0, 0.0],
        ];
        
        
        let highlight_color = match mouse_coords {
            (a, 2) if a > 8 * 6 + 12 && a < 8 * 6 + 18 => Some(C::Black),
            (a, 2) if a < 8 * 6 + 24 && a > 8 * 6 + 18 => Some(C::White),
            _ => None,
        };
        if render {

        if click && mouse.y == 1 && [12, 13].contains(&mouse.x) {
            self.action = match self.action {
                Action::Manipulate => Action::Observe,
                Action::Observe => Action::Manipulate,
            }
        }

        if click && mouse.y == 8 && mouse.x < 2 {
            self.unicode = !self.unicode;
        }

        if click && mouse.y == 8 && mouse.x > 3 && mouse.x <= 5{
            self.bot = !self.bot;
        }


        queue!(
            stdout,
            cursor::MoveTo(8 * 6 + 5, 2),
            style::PrintStyledContent(format!("{:?}", self.current_player).white()),
            cursor::MoveTo(8 * 6 + 12, 2),
            style::PrintStyledContent("[Black|White]".white()),
            cursor::MoveTo(11 * 6 + 12, 3),
            style::PrintStyledContent("(click to switch)".dark_grey().italic()),
            cursor::MoveTo(30, 2 + 3 * 8),
            style::PrintStyledContent("AI: ".white()),
            style::PrintStyledContent("On".grey().on(match self.bot {true => Color::DarkYellow, false => Color::Black}).italic()),
            style::PrintStyledContent("/".white()),
            style::PrintStyledContent("Off".grey().on(match !self.bot {true => Color::DarkYellow, false => Color::Black}).italic()),
            cursor::MoveTo(3, 2 + 3 * 8),
            style::PrintStyledContent("Toggle Unicode".dark_grey().italic()),
            cursor::MoveTo(11 * 6 + 12, 2 + 3),
            style::PrintStyledContent(" Manipulate ".white().on(match self.action {
                Action::Manipulate => Color::DarkYellow,
                Action::Observe => Color::Black,
            })),
            cursor::MoveTo(11 * 6 + 12, 3 + 3),
            style::PrintStyledContent(" Observe ".white().on(match self.action {
                Action::Observe => Color::DarkYellow,
                Action::Manipulate => Color::Black,
            })),

        )?;

        let eval = self.eval();

        let bar = (eval.signum()*(eval.powi(2)).powf(0.25))/6.0;
        queue!(
                stdout,
                cursor::MoveTo(1, 1),
                style::PrintStyledContent(
                    format!("{} ",bar.round().abs()).white()),
                cursor::MoveTo(4, 1),
                style::PrintStyledContent(
                    format!("{}        ",eval.round()).grey().italic()),
                
        )?;
        for y in 2..8*3+2 {
            queue!(
                stdout,
                cursor::MoveTo(1, y),
                style::PrintStyledContent(
                    format!("  ").on(match (y-2) as f32 -12.0 > bar {true => Color::White, false => Color::Black})
                ),
            )?;
        }
        }



        if self.get(mouse) != None {
            let square = self.get(mouse).unwrap();
            let mut pieces = square.pieces;
            let total: f32 = pieces.iter().map(|f| f.value()).sum::<f32>().max(100.0);
            pieces.sort_by_key(|f| (f.value() * 100.0) as i32);

            if render {queue!(
                stdout,
                cursor::MoveTo(8 * 6 + 5, 3),
                style::PrintStyledContent(
                    format!("{}{}", "abcedfgh".chars().nth(mouse.x as usize)
                    .expect("well something must have gon wrong because you're not supposed to be able to have a coord larger than 7"), (8-mouse.y))
                        .white()
                ),
            )?;

            for (i, p) in pieces.iter().enumerate() {
                queue!(
                    stdout,
                    cursor::MoveTo(8 * 6 + 5, 4 + i as u16),
                    style::PrintStyledContent(
                        format!(
                            "{:?} {} {}%     ",
                            p.color(),
                            format!("{p:?}").split("(").collect::<Vec<&str>>()[0],
                            (1000.0 * p.value() / total).round() / 10.0
                        )
                        .white()
                    ),
                )?;
            }
            for i in 0..16 {
                queue!(
                    stdout,
                    cursor::MoveTo(8 * 6 + 5, 4 + pieces.len() as u16 + i),
                    style::PrintStyledContent("                   ".white()),
                )?;
            }
        }
            if click
                && self.action == Action::Observe
                && pieces.len() >= 1
                && pieces[0].value().round() != 100.0
            {
                let rand = random!(0, total.floor() as i32*100) as f32 / 100.0;
                let mut t = 0.0;
                for i in pieces {
                    t += i.value();
                    if t >= rand as f32 {
                        self.grid[mouse.x as usize][mouse.y as usize] = Square::new_blank();
                        self.grid[mouse.x as usize][mouse.y as usize].add_piece(i.with_value(100.0));
                        break;
                    }
                }
                if t < rand as f32 {
                    self.grid[mouse.x as usize][mouse.y as usize] = Square::new_blank();
                }

                self.current_player = match self.current_player {
                    C::Black => C::White,
                    C::White => C::Black,
                }
            }
        }

        let potential_moves = self.get_moves(mouse);
        let number_of_moves = potential_moves.len();
        if self.action == Action::Manipulate {
            if click && number_of_moves > 0 && self.get(mouse) != None {
                if number_of_moves == 1
                    && potential_moves[0].1.value() == 100.0
                    && self.grid[mouse.x as usize][mouse.y as usize].pieces.len() == 1
                    && self.grid[mouse.x as usize][mouse.y as usize].pieces[0].value() == 100.0
                {
                    self.grid[mouse.x as usize][mouse.y as usize] = Square::new_blank();
                }

                for (og_pos, piece) in &potential_moves {
                    let val = piece.value() / number_of_moves as f32;
                    if mouse.y == match self.current_player  {
                        C::Black => 7,
                        C::White => 0
                    } && piece.coord() == Piece::Pawn(100.0, self.current_player).coord()
                    && piece.color() == self.current_player {
                        self.grid[mouse.x as usize][mouse.y as usize].add_piece(Piece::Bishop(piece.value()/4.0, self.current_player));
                        self.grid[mouse.x as usize][mouse.y as usize].add_piece(Piece::Queen(piece.value()/4.0, self.current_player));
                        self.grid[mouse.x as usize][mouse.y as usize].add_piece(Piece::Knight(piece.value()/4.0, self.current_player));
                        self.grid[mouse.x as usize][mouse.y as usize].add_piece(Piece::Rook(piece.value()/4.0, self.current_player));

                    }else {
                    self.grid[mouse.x as usize][mouse.y as usize].add_piece(piece.with_value(val));
                    }
                    self.grid[og_pos.x as usize][og_pos.y as usize]
                        .minus_piece(piece.with_value(val));
                }
                self.current_player = match self.current_player {
                    C::Black => C::White,
                    C::White => C::Black,
                }
            }
        }

        for x in 0..8 {
            for y in 0..8 {
        self.grid[x as usize][y as usize]
                    .pieces
                    .retain(|f| f.value() != 0.0);
        }}

        if render {
        for x in 0..8 {
            for y in 0..8 {
                let square = self.get(Pos::new(x, y)).unwrap();
                
                let mut pieces = square.pieces;
                let total: f32 = pieces.iter().map(|f| f.value()).sum::<f32>().max(100.0);
                pieces.sort_by_key(|f| (f.value() * 100.0) as i32);

                let mut text = "".to_owned();

                for (pos, p) in self.get_moves(Pos::new(x, y)) {
                    if pos == mouse {
                        text = text + &p.text(self.unicode)
                    }
                }

                if (x + y) % 2 == 0 {
                    for l in 0..6 {
                        for i in 0..3 {
                            queue!(
                                stdout,
                                cursor::MoveTo(x * 6 + 4 + l, y * 3 + 2 + i),
                                style::PrintStyledContent(" ".on_white()),
                            )?;
                        }
                    }
                } else {
                    for l in 0..6 {
                        for i in 0..3 {
                            queue!(
                                stdout,
                                cursor::MoveTo(x * 6 + 4 + l, y * 3 + 2 + i),
                                style::PrintStyledContent(" ".on_black()),
                            )?;
                        }
                    }
                }

                if pieces.len() == 1 && pieces[0].value().round() == 100.0 {
                    queue!(
                        stdout,
                        cursor::MoveTo(x * 6 + 4, y * 3 + 2),
                        style::PrintStyledContent("x".yellow().on_red()),
                    )?;
                } else if pieces.len() > 1 || (pieces.len() == 1 && pieces[0].value() != 100.0) {
                    queue!(
                        stdout,
                        cursor::MoveTo(x * 6 + 4, y * 3 + 2),
                        style::PrintStyledContent("?".yellow().on(Color::AnsiValue(89))),
                    )?;
                }

                if Pos::new(x, y) == mouse {
                    if self.action == Action::Observe
                        && pieces.len() >= 1
                        && pieces[0].value().round() != 100.0
                    {
                        queue!(
                            stdout,
                            cursor::MoveTo(x * 6 + 5, y * 3 + 2),
                            style::PrintStyledContent(
                                match self.unicode {
                                    true => "👁",
                                    false => "I",
                                }
                                .dark_cyan()
                            ),
                        )?;
                    }

                    if number_of_moves != 0 && self.action == Action::Manipulate {
                        queue!(
                            stdout,
                            cursor::MoveTo(x * 6 + 5, y * 3 + 2),
                            style::PrintStyledContent(
                                format!("{number_of_moves}").black().on_green()
                            ),
                        )?;
                    }
                } else {
                    queue!(
                        stdout,
                        cursor::MoveTo(x * 6 + 5, y * 3 + 2),
                        style::PrintStyledContent("    ".black().on(match (x + y) % 2 {
                            0 => Color::White,
                            _ => Color::Black,
                        })),
                    )?;
                }

                if self.action == Action::Manipulate {
                    queue!(
                        stdout,
                        cursor::MoveTo(
                            (x + 1) * 6 + 4
                                - match self.unicode {
                                    true => text.len() as u16 / 3,
                                    false => text.len() as u16,
                                },
                            y * 3 + 2
                        ),
                        style::PrintStyledContent(text.with(Color::AnsiValue(16)).on_grey()),
                    )?;
                }

                for i in pieces.iter() {
                    let n = i.value() / total;
                    stats[i.coord() as usize][match i.color() {
                        C::White => 1,
                        C::Black => 0,
                    }] += i.value() / total;
                    
                    let gradient = [
                        colorous::VIRIDIS,
                        colorous::CIVIDIS,
                        colorous::TURBO,
                        colorous::CUBEHELIX,
                        colorous::SINEBOW,
                        colorous::COOL,
                        colorous::WARM,
                    ];
                    let pallet = gradient[5].eval_continuous(1.0-n as f64);
                    queue!(
                        stdout,
                        cursor::MoveTo(
                            4 + x * 6 + i.coord(),
                            3 + y * 3
                                + (match i.color() {
                                    C::White => 0,
                                    C::Black => 1,
                                })
                        ),
                        
                        style::PrintStyledContent(



                            i.text(self.unicode).with(i.color().color_val()).attribute(style::Attribute::Framed).on(
                                match potential_moves.contains(&(Pos::new(x, y), *i))
                                    && self.action == Action::Manipulate
                                {
                                    _ if Some(i.color()) == highlight_color =>
                                        Color::AnsiValue(220),
                                    false => Color::AnsiValue(rgb_to_ansi256(pallet.r, pallet.g, pallet.b))
                                    ,
                                    true => Color::Rgb {
                                        r: 0,
                                        b: 0,
                                        g: (200.0 * (i.value() / total)) as u8 + 50
                                    },
                                }
                            )
                        )
                    )?;
                }
            }
            }
        }

        queue!(
            stdout,
            cursor::MoveTo(12 * 6 + 7, 9),
            style::PrintStyledContent(" Black | White".white()),
            cursor::MoveTo(12 * 6 + 7, 10),
            style::PrintStyledContent("---------------".white()),
        )?;
        for (j, i) in stats.iter().enumerate() {
            queue!(
                stdout,
                cursor::MoveTo(12 * 6 + 7, 11 + j as u16),
                style::PrintStyledContent(
                    truncate(
                        &format!(
                            "{}: {:?}",
                            match j {
                                0 => Piece::King(0.0, C::White).text(self.unicode),
                                1 => Piece::Queen(0.0, C::White).text(self.unicode),
                                2 => Piece::Bishop(0.0, C::White).text(self.unicode),
                                3 => Piece::Knight(0.0, C::White).text(self.unicode),
                                4 => Piece::Rook(0.0, C::White).text(self.unicode),
                                5 => Piece::Pawn(0.0, C::White).text(self.unicode),
                                _ => "Err".to_owned(),
                            },
                            (i[0] * 10.0).round() / 10.0
                        ),
                        7
                    )
                    .white()
                ),
                cursor::MoveTo(12 * 6 + 14, 11 + j as u16),
                style::PrintStyledContent(
                    truncate(&format!("| {:?}", (i[1] * 10.0).round() / 10.0),6).white()
                ),
            )?;
        }

        if stats[0][0] == 0.0 {
            self.winner = Some(C::White)
        }
        if stats[0][1] == 0.0 {
            self.winner = Some(C::Black)
        }
        if render {
        if let Some(w) = self.winner {
            queue!(
                stdout,
                cursor::MoveTo(4 * 6 + 2, 12),
                style::PrintStyledContent(format!("{w:?} Won!").black().on_dark_yellow()),
            )?;
        }
        stdout.flush()?;
    }


        if render && self.current_player == C::Black && self.bot {
            self.bot_move()
        }

        Ok(())
    }

    fn new() -> Board {
        Board {
            bot: false,
            action: Action::Manipulate,
            current_player: C::White,
            unicode: true,
            winner: None,
            grid: [
                [
                    Piece::Rook(100.0, C::Black).square(),
                    Piece::Pawn(100.0, C::Black).square(),
                    Square::new_blank(),
                    Square::new_blank(),
                    Square::new_blank(),
                    Square::new_blank(),
                    Piece::Pawn(100.0, C::White).square(),
                    Piece::Rook(100.0, C::White).square(),
                ],
                [
                    Piece::Knight(100.0, C::Black).square(),
                    Piece::Pawn(100.0, C::Black).square(),
                    Square::new_blank(),
                    Square::new_blank(),
                    Square::new_blank(),
                    Square::new_blank(),
                    Piece::Pawn(100.0, C::White).square(),
                    Piece::Knight(100.0, C::White).square(),
                ],
                [
                    Piece::Bishop(100.0, C::Black).square(),
                    Piece::Pawn(100.0, C::Black).square(),
                    Square::new_blank(),
                    Square::new_blank(),
                    Square::new_blank(),
                    Square::new_blank(),
                    Piece::Pawn(100.0, C::White).square(),
                    Piece::Bishop(100.0, C::White).square(),
                ],
                [
                    Piece::King(100.0, C::Black).square(),
                    Piece::Pawn(100.0, C::Black).square(),
                    Square::new_blank(),
                    Square::new_blank(),
                    Square::new_blank(),
                    Square::new_blank(),
                    Piece::Pawn(100.0, C::White).square(),
                    Piece::King(100.0, C::White).square(),
                ],
                [
                    Piece::Queen(100.0, C::Black).square(),
                    Piece::Pawn(100.0, C::Black).square(),
                    Square::new_blank(),
                    Square::new_blank(),
                    Square::new_blank(),
                    Square::new_blank(),
                    Piece::Pawn(100.0, C::White).square(),
                    Piece::Queen(100.0, C::White).square(),
                ],
                [
                    Piece::Bishop(100.0, C::Black).square(),
                    Piece::Pawn(100.0, C::Black).square(),
                    Square::new_blank(),
                    Square::new_blank(),
                    Square::new_blank(),
                    Square::new_blank(),
                    Piece::Pawn(100.0, C::White).square(),
                    Piece::Bishop(100.0, C::White).square(),
                ],
                [
                    Piece::Knight(100.0, C::Black).square(),
                    Piece::Pawn(100.0, C::Black).square(),
                    Square::new_blank(),
                    Square::new_blank(),
                    Square::new_blank(),
                    Square::new_blank(),
                    Piece::Pawn(100.0, C::White).square(),
                    Piece::Knight(100.0, C::White).square(),
                ],
                [
                    Piece::Rook(100.0, C::Black).square(),
                    Piece::Pawn(100.0, C::Black).square(),
                    Square::new_blank(),
                    Square::new_blank(),
                    Square::new_blank(),
                    Square::new_blank(),
                    Piece::Pawn(100.0, C::White).square(),
                    Piece::Rook(100.0, C::White).square(),
                ],
            ],
        }
    }
}

fn main() {
    let mut board = Board::new();

    let mut stdout = stdout();

    queue!(
        stdout,
        terminal::Clear(ClearType::All),
        EnableMouseCapture,
        Hide,
        SetTitle("Quantum Chess"),
    );
    stdout.flush();
    let mut cursor = Pos::new(10, 10);
    let mut commit = Pos::new(10, 10);
    board.update(cursor, false, (0, 0), true);
    loop {
        match read().expect("well idk how this hapened") {
            Event::FocusGained => {}
            Event::FocusLost => {}
            Event::Key(event) => {
                if (event.code == KeyCode::Char('c')
                    && event.modifiers.contains(KeyModifiers::CONTROL))
                    || event.code == KeyCode::Esc
                {
                    return;
                }
            }
            Event::Mouse(event) => match event.kind {
                event::MouseEventKind::Down(_) => {
                    board.update(cursor, true, (event.column, event.row), true);
                }
                event::MouseEventKind::Up(_) => {}
                event::MouseEventKind::Drag(_) => {}
                event::MouseEventKind::Moved => {
                    if event.column >= 4 && event.row >= 2 {
                        let new_pos = Pos::new((event.column - 4) / 6, (event.row - 2) / 3);
                        if [2, 3].contains(&event.row)
                            && ((6 * 8 + 4)..(8 * 8 + 4)).contains(&event.column)
                        {
                            board.update(cursor, false, (event.column, event.row), true);
                        }
                        if new_pos != cursor {
                            cursor = new_pos;
                            board.update(cursor, false, (event.column, event.row), true);
                        }
                    }
                }
                event::MouseEventKind::ScrollDown => {}
                event::MouseEventKind::ScrollUp => {}
            },
            Event::Paste(data) => {}
            Event::Resize(width, height) => {
                queue!(stdout, terminal::Clear(ClearType::All),);
                board.update(cursor, false, (0, 0), true);
            }
        }
        // board.draw();
    }

    println!("Hello, world!");
}
