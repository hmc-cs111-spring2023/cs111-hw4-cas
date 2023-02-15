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
  val ind :Int = rand.nextInt(validColors.length)
  validColors.apply(ind)


/** Given four colors, make a board from them */
def makeBoardFromColors(c1: Color, c2: Color, c3: Color, c4: Color): Board =
  c1.toString() + c2.toString() + c3.toString() + c4.toString()

/** Create a random board */
def getRandomBoard(): Board =
  val c1 = getRandomColor()
  val c2 = getRandomColor()
  val c3 = getRandomColor()
  val c4 = getRandomColor()
  makeBoardFromColors(c1, c2, c3, c4)

/** Play one round of the game */
def playRound(board: Board): (Int, Int) =
  var correct_place = 0
  var correct_val = 0
  println("Enter a guess for spot 1:")
  val spot_one = readLine().toString()
  println("Enter a guess for spot 2:")
  val spot_two = readLine().toString()
  println("Enter a guess for spot 3:")
  val spot_three = readLine().toString()
  println("Enter a guess for spot 4:")
  val spot_four = readLine().toString()
  val guess = spot_one + spot_two + spot_three + spot_four
  scoreGuess(board, guess)
  /*
  if board.apply(1).toString() == spot_one then correct_place += 1
  else if board contains spot_one then correct_val += 1
  else if board.apply(2).toString() == spot_two then correct_place += 1
  else if board contains spot_two then correct_val += 1
  else if board.apply(3).toString() == spot_three then correct_place += 1
  else if board contains spot_three then correct_val += 1
  else if board.apply(4).toString() == spot_four then correct_place += 1
  else if board contains spot_four then correct_val += 1 
  (correct_place, correct_val)*/


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
