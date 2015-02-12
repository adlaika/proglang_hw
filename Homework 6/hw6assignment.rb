# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece

  Cheat_Piece =
    [[[0, 0]]]

  All_My_Pieces = All_Pieces +
    ([rotations([[0, 0], [1, 0], [0, 1], [1, 1], [2, 1]]), # poochy square
      [[[0, 0], [-2, 0], [-1, 0], [1, 0], [2, 0]],        # 5-long
       [[0, 0], [0, -2], [0, -1], [0, 1], [0, 2]]],
      rotations([[0, 0], [1, 0], [0, 1]])])                # baby L

  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

  def self.cheat_piece (board)
    MyPiece.new(Cheat_Piece.sample, board)
  end

end

class MyBoard < Board
  def initialize (game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = MyPiece.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
    @cheat_toggle = false
  end

  # gets the information from the current piece about where it is and uses this
  # to store the piece on the board itself.  Then calls remove_filled.
  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..locations.size - 1).each{|index| #allows polyominos with size greater than 4
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] =
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

  # moves the current piece down by one, if this is not possible stores the
  # current piece and replaces it with a new one.
  def run
    ran = @current_block.drop_by_one
    if !ran
      store_current
      if !game_over?
        if @cheat_toggle
          cheat
        else
          next_piece
        end
      end
    end
    @game.update_score
    draw
  end

  def drop_all_the_way
    if @game.is_running?
      ran = @current_block.drop_by_one
      @current_pos.each{|block| block.remove}
      while ran
        @score += 1
        ran = @current_block.drop_by_one
      end
      draw
      store_current
      if !game_over?
        if @cheat_toggle
          cheat
        else
          next_piece
        end
      end
      @game.update_score
      draw
    end
  end

  def next_piece
    @current_block = MyPiece.next_piece(self)
    @current_pos = nil
  end

  def cheat_toggler
    if @score >=100 && @cheat_toggle == false
      @score -= 100
      @cheat_toggle = true
    end
  end

  def cheat
    @current_block = MyPiece.cheat_piece(self)
    @current_pos = nil
    @cheat_toggle = !@cheat_toggle
  end

  def rotate_180
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
    end
    draw
  end

end

class MyTetris < Tetris
  # creates a canvas and the board that interacts with it
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings
    super
    @root.bind('c', proc {@board.cheat_toggler})
    @root.bind('u', proc {@board.rotate_180})
  end

end
