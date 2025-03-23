# Copyright (C) 2018, Christopher Hume.  All rights reserved.
#
# You should have received a copy of the MIT License along with this program.
# If not, see https://opensource.org/licenses/MIT.
#
# 2018-09-09 CNHume Created File
#
from operator import itemgetter
import string

from Tile import Tile


class Board(object):
  """Scrabble Board"""
  BONUS_L2 = 'L2'
  BONUS_L3 = 'L3'
  BONUS_W2 = 'W2'
  BONUS_W3 = 'W3'

  BLANK = '_'
  COLON = ':'
  COMMA = ','
  DELIM = ', '
  EMPTY = ''
  FILEA = ord('a')
  KVP = '{}: {}'
  RANK1 = 1

  def __init__(self, size_x, size_y, bonusRecords, tiles):
    self.size_x = size_x
    self.size_y = size_y
    self.center_x = (self.size_x + 1) // 2 - 1
    self.center_y = (self.size_y + 1) // 2 - 1
    self.center = (self.center_x, self.center_y)
    self.blanks = Board.BLANK * self.size_x
    self.rows = self.blankRows()
    self.initBonuses(bonusRecords)
    self.tiles = tiles

  def blankRows(self):
    return [list(self.blanks) for y in range(self.size_y)]

  def transpose(self):
    self.rows = [[self.get2(x, y) for y in range(self.size_y)] for x in range(self.size_x)]
    # transpose dimensions:
    self.size_x, self.size_y = self.size_y, self.size_x

  def initBonuses(self, bonusRecords):
    # Creaate Bonus Sets:
    self.L2 = set()
    self.L3 = set()
    self.W2 = set()
    self.W3 = set()

    # Map Bonus Names to their corresponding Sets:
    self.bonusMap = {}
    self.bonusMap[Board.BONUS_L2] = self.L2
    self.bonusMap[Board.BONUS_L3] = self.L3
    self.bonusMap[Board.BONUS_W2] = self.W2
    self.bonusMap[Board.BONUS_W3] = self.W3

    self.parseBonuses(bonusRecords)
    #[Test]self.showBonuses()

    missing = [bonusName for bonusName, bonusSquares in self.bonusMap.items() if not bonusSquares]
    if missing:
      print('There are no bonus squares for {}.'.format(Board.DELIM.join(missing)))

  def parseBonuses(self, bonusRecords):
    #
    #[Note]The normal() method below only enforces 4-fold symmetry about the center-lines.
    # The standard set of Bonus Squares are actually 8-fold symmetric about the diagonals.
    # There are a total of 61 = 8 * 3 + 4 * 9 + 1 bonus squares.
    #
    # The Bonus File will specify the following Lower Quadrant Squares:
    #
    # L2: d1, g3, a4, h4 * 2, c7, g7, d8 * 2
    # L3: f2, b6, f6
    # W2: b2, c3, d4, e5, h8 * 1
    # W3: a1, h1 * 2, a8 * 2
    #
    for record in bonusRecords:
      terms = record.split(Board.COLON)
      if len(terms) != 2:
        print('{} is not a valid bonus record'.format(record))
        return

      bonusName = terms[0].strip().upper()
      names = terms[1].split(Board.COMMA)

      if bonusName in self.bonusMap:
        bonusSquares = self.bonusMap[bonusName]

        for name in names:
          squareName = name.strip()
          square = self.parseSquare(squareName)
          if square:
            bonusSquares.add(square)
      else:
        print('{} is not a valid bonus name'.format(bonusName))

  def parseSquare(self, squareName):
    square = self.parseSquare1(squareName)
    if not square:
      print('{} is not a valid square name'.format(squareName))
    return square

  def parseSquare1(self, squareName):
    sq = squareName.lower()

    if (len(sq) < 1):
      return None

    file = ord(sq[0])
    if file < Board.FILEA or Board.FILEA + self.size_x <= file:
      return None

    digits = sq[1:]
    if not digits.isdigit():
      return None

    rank = int(digits)
    if rank < Board.RANK1 or Board.RANK1 + self.size_y <= rank:
      return None

    x = file - Board.FILEA
    y = rank - Board.RANK1
    return (x, y)

  def showBonuses(self):
    sortedItems = sorted(self.bonusMap.items())
    for bonusName, bonusSquares in sortedItems:
      sortedSquares = sorted(bonusSquares, key=itemgetter(1, 0))
      squareNames = [Board.squareName(square) for square in sortedSquares]
      print(Board.KVP.format(bonusName, Board.DELIM.join(squareNames)))

  def display(self):
    files = Tile.spaced([Board.file(x) for x in range(self.size_x)])
    print
    print('    {}'.format(files))
    print
    for y in range(self.size_y):
      rank = self.rank(y).rjust(2)
      #row = [str(self.normal((x, y))) for x in range(self.size_x)]
      #print('{}  {}  {}'.format(rank, Tile.spaced(row), rank))
      print('{}  {}  {}'.format(rank, Tile.spaced(self.rows[y]), rank))
    print
    print('    {}'.format(files))
    print

  #@staticmethod
  #def overstr(dst, x, src):
  #  return dst[:x] + src + dst[x + len(src):]

  @staticmethod
  def rank(y):
    return str(Board.RANK1 + y)

  @staticmethod
  def file(x):
    return chr(Board.FILEA + x)

  @staticmethod
  def squareName2(x, y):
    return Board.file(x) + Board.rank(y)

  @staticmethod
  def squareName(square):
    x, y = square
    return Board.squareName2(x, y)

  def get2(self, x, y):
    return self.rows[y][x]

  def get(self, square):
    x, y = square
    return self.get2(x, y)

  def set2(self, x, y, value):
    self.rows[y][x] = value

  def set(self, square, value):
    x, y = square
    return self.set2(x, y, value)

  def left2(self, x, y):
    str = Board.EMPTY.join(self.rows[y])
    pos = str.rfind(Board.BLANK, 0, x)
    return str[pos + 1:x]

  def left(self, square):
    x, y = square
    return self.left2(x, y)

  def right2(self, x, y):
    str = Board.EMPTY.join(self.rows[y])
    pos = str.find(Board.BLANK, x + 1)
    return str[x + 1:] if pos < 0 else str[x + 1:pos]

  def right(self, square):
    x, y = square
    return self.right2(x, y)

  def above2(self, x, y):
    col = [self.get2(x, y2) for y2 in range(self.size_y)]
    str = Board.EMPTY.join(col)
    pos = str.rfind(Board.BLANK, 0, y)
    return str[pos + 1:y]

  def above(self, square):
    x, y = square
    return self.above2(x, y)

  def below2(self, x, y):
    col = [self.get2(x, y2) for y2 in range(self.size_y)]
    str = Board.EMPTY.join(col)
    pos = str.find(Board.BLANK, y + 1)
    return str[y + 1:] if pos < 0 else str[y + 1:pos]

  def below(self, square):
    x, y = square
    return self.below2(x, y)

  def flip_x(self, x):
    return self.size_x - x - 1

  def flip_y(self, y):
    return self.size_y - y - 1

  def normal2(self, x, y):
    normal_x = self.flip_x(x) if x > self.center_x else x
    normal_y = self.flip_x(y) if y > self.center_y else y
    return normal_x, normal_y

  def normal(self, square):
    x, y = square
    return self.normal2(x, y)

  def letterBonus(self, square):
    normal = self.normal(square)
    if normal in self.L3:
      return 3
    elif normal in self.L2:
      return 2
    else:
      return 1

  def wordBonus(self, square):
    normal = self.normal(square)
    if normal in self.W3:
      return 3
    elif normal in self.W2:
      return 2
    else:
      return 1

  def score(self, word, placements):
    score = 0
    wordBonus = 1
    for letter in word:
      score += self.tiles.letterValue(letter)

    for (square, letter) in placements:
      letterBonus = self.letterBonus(square) - 1
      if letterBonus:
        score += self.tiles.letterValue(letter) * letterBonus
      wordBonus *= self.wordBonus(square)

    return score * wordBonus

  def Test(self):
    self.set2(2, 2, 'A')
    self.set2(3, 2, 'B')
    self.set2(4, 2, 'C')
    self.set2(5, 2, 'A')
    self.set2(6, 2, 'T')
    self.set2(1, 3, 'D')
    self.set2(2, 3, 'E')
    self.set2(3, 3, 'F')

    self.display()
    left = self.left2(4, 3)
    right1 = self.right2(1, 2)
    right3 = self.right2(3, 2)
    above = self.above2(3, 4)
    below = self.below2(2, 1)

    self.transpose()
    self.display()
    tabove = self.above2(3, 4)
    tbelow1 = self.below2(2, 1)
    tbelow3 = self.below2(2, 3)
    tleft = self.left2(4, 3)
    tright = self.right2(1, 2)

    self.transpose()
    self.display()
    pass

  def contact_above2(self, x, y):
    return y > 0 and self.get2(x, y - 1) != Board.BLANK

  def contact_below2(self, x, y):
    return y + 1 < self.size_y and self.get2(x, y + 1) != Board.BLANK

  def contact_left2(self, x, y):
    return x > 0 and self.get2(x - 1, y) != Board.BLANK

  def contact_right2(self, x, y):
    return x + 1 < self.size_x and self.get2(x + 1, y) != Board.BLANK

  def contact(self, placements):
    for (x, y), letter in placements:
      if self.above2(x, y) or self.below2(x, y) or self.left2(x, y) or self.right2(x, y):
        return True
    return False

  def horizontal(self, placements):
    if not placements:
      return False

    (x0, y0), letter0 = placements[0]
    if len(placements) > 1:
      (x1, y1), letter1 = placements[1]
      return y0 == y1

    return self.contact_left2(x0, y0) or self.contact_right2(x0, y0)

  def tiled(self, placements, horizontal):
    empty = Board.EMPTY
    if not Board.linear(placements, horizontal):
      return empty

    sortedPlacements = sorted(placements, key=lambda placement: (placement[0][1], placement[0][0]))
    if Board.duplicate(sortedPlacements):
      return empty

    count = len(sortedPlacements)
    if count < 1:
      return empty

    # Find any gaps in the placements and
    # ensure that their cells are full:
    first, first_letter = sortedPlacements[0]
    last, last_letter = sortedPlacements[count - 1]

    letters = []
    prev_x, prev_y = first
    if horizontal:
      for (next_x, next_y), next_letter in sortedPlacements:
        while (prev_x < next_x):
          found = self.get2(prev_x, prev_y)
          if found == Board.BLANK:
            return empty
          letters.append(found)
          prev_x += 1

        if self.get2(prev_x, prev_y) != Board.BLANK:
          return empty
        # tiled square
        letters.append(next_letter)
        prev_x += 1
    else:
      for (next_x, next_y), next_letter in sortedPlacements:
        while (prev_y < next_y):
          found = self.get2(prev_x, prev_y)
          if found == Board.BLANK:
            return empty
          letters.append(found)
          prev_y += 1

        if self.get2(prev_x, prev_y) != Board.BLANK:
          return empty
        # tiled square
        letters.append(next_letter)
        prev_y += 1

    # The bonus for a tiled word depends on the product of all tiled squares
    middle = Board.EMPTY.join(letters)
    word = self.extent(middle, first, last, horizontal)
    return word

  def crosswords(self, placements, horizontal):
    pairs = []
    for placement in placements:
      square, letter = placement
      # first and last squares are the square containing the letter,
      # for crosswords:
      word = self.extent(letter, square, square, not horizontal)
      if len(word) > 1:
        # The bonus for a crossword depends on the crossed placement
        score = self.score(word, [placement])
        pair = word, score
        pairs.append(pair)
    return pairs

  def extent(self, middle, first, last, horizontal):
    if horizontal:
      left = self.left(first)
      right = self.right(last)
      return left + middle + right
    else:
      above = self.above(first)
      below = self.below(last)
      return above + middle + below

  def fillRacks(self, racks):
    for rack in racks:
      self.tiles.fill(rack)


def linear(placements, horizontal):
  if len(placements) > 1:
    (x, y), letter = placements[0]
    if horizontal:
      for placement in placements[1:]:
        (next_x, next_y), next_letter = placement
        if next_y != y:
          return False
    else:
      for placement in placements[1:]:
        (next_x, next_y), next_letter = placement
        if next_x != x:
          return False
  return True


def duplicate(sortedPlacements):
  if len(sortedPlacements) > 1:
    prev, prev_letter = sortedPlacements[0]
    for placement in sortedPlacements[1:]:
      (next, next_letter) = placement
      #[Assume]Placements are sorted to detect duplicates:
      if next == prev:
        return True
      prev = next
  return False
