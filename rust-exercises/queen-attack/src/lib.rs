#[derive(Debug)]
pub struct ChessPosition(i32, i32);

#[derive(Debug)]
pub struct Queen {
    position: ChessPosition
}

impl ChessPosition {
    pub fn new(rank: i32, file: i32) -> Option<Self> {

        let constructor_rank = if rank >=0 && rank<8 { rank } else { -1 };
        let constructor_file = if file >=0 && file<8 { file } else { -1 };

        if constructor_rank != -1 && constructor_file != -1 {
            Some(ChessPosition(constructor_rank, constructor_file))
        } else {
            None
        }
    }
}

impl Queen {
    pub fn new(position: ChessPosition) -> Self {
        Queen { position }
    }

    pub fn can_attack(&self, other: &Queen) -> bool {

        return self.validate_horizontal(other.position.0) || self.validate_vertical(other.position.1) || self.validate_diagonal(&other.position);

    }

    pub fn validate_horizontal(&self, rank2: i32) -> bool {
        self.position.0 == rank2
    }

    pub fn validate_vertical(&self, file2: i32) -> bool {
        self.position.1 == file2
    }

    pub fn validate_diagonal(&self, position2: &ChessPosition) -> bool {
        (self.position.0 - position2.0).abs() == (self.position.1 - position2.1).abs()
    }
}
