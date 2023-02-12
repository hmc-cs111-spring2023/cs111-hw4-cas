import scala.util.Random
import scala.io.StdIn.readLine

/** *****************************************************************************
  * Representing a game
  *
  * We will represent the board as a string of four characters. Each character
  * will be one of the following: B = Blue, Y = Yellow, R = Red, G = Green
  */
type Color = Char
type Board = String
val validColors = List('B', 'Y', 'R', 'G')

/** Get a random color from the list of valid colors */
def getRandomColor(): Color =
  val rand = new scala.util.Random
  val ind = rand.nextInt(validColors.length())
  validColors.lift(ind)


/** Given four colors, make a board from them */
def makeBoardFromColors(c1: Color, c2: Color, c3: Color, c4: Color): Board =
  Board = c1.toString() + c2.toString() + c3.toString() + c4.toString()
  Board

/** Create a random board */
def getRandomBoard(): Board =
  val c1 = getRandomColor()
  val c2 = getRandomColor()
  val c3 = getRandomColor()
  val c4 = getRandomColor()
  makeBoardFromColors(c1, c2, c3, c4)

/** Play one round of the game */
def playRound(board: Board): (Int, Int) =
  correct_place = 0
  correct_val = 0
  println("Enter a guess for spot 1:")
  val spot_one = readLine().toString()
  println("Enter a guess for spot 2:")
  val spot_two = readLine().toString()
  println("Enter a guess for spot 3:")
  val spot_three = readLine().toString()
  println("Enter a guess for spot 4:")
  val spot_four = readLine().toString()
  if Board.lift(1) == spot_one then correct_place += 1
  else if Board contains spot_one then correct_val += 1
  else if Board.lift(2) == spot_two then correct_place += 1
  else if Board contains spot_two then correct_val += 1
  else if Board.lift(3) == spot_three then correct_place += 1
  else if Board contains spot_three then correct_val += 1
  else if Board.lift(4) == spot_four then correct_place += 1
  else if Board contains spot_four then correct_val += 1
  println(correct_place.toString() + "color(s) are in the correct place.")
  println(correct_val.toString() + "color(s) are correct but in the wrong place.")


/** Score a guess
  *
  * A score is a tuple of two integers. The first integer is the number of
  * correct positions, and the second integer is the number of remaining correct
  * colors.
  */
def scoreGuess(board: Board, guess: Board): (Int, Int) = {

  // The initial score is (0, 0)
  var correctPositions = 0
  var correctColors = 0

  // Get the unique colors on the board
  val boardColors = board.toSet

  // Check each guess position against the corresponding board position
  // or (if there is not a match at that position) against the remainder of
  // the board.
  for (i <- 0 to 3) {
    if (guess(i) == board(i)) {
      correctPositions += 1
    } else if (boardColors.contains(guess(i))) {
      correctColors += 1
    }
  }

  (correctPositions, correctColors)
}

/** *****************************************************************************
  * Main program
  */

// When true, the program will print out the board at the start of the game
val DEBUG = true

@main
def mastermind() = {

  // Create a new board
  val board = getRandomBoard()

  if (DEBUG) {
    println(s"[DEBUG] The board is $board")
  }

  // Play rounds until the user guesses the board
  var score = (0, 0)
  while (score != (4, 0)) {
    score = playRound(board)
    val (correctPlace, correctColor) = score
    println(s"$correctPlace color(s) are in the correct place.")
    println(s"$correctColor color(s) are correct but in the wrong place.\n")
  }

  // End the game
  println(s"Congratulations! You figured out the board was $board")
}
