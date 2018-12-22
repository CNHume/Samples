# -*- coding: utf-8 -*-
# Copyright (C) 2018, Christopher Hume.  All rights reserved.
#
# You should have received a copy of the MIT License along with this program.
# If not, see https://opensource.org/licenses/MIT.
#
# 2018-09-09 CNHume Created File
#
from operator import itemgetter
import string
import re

from Board import Board
from Tile import Tile

class Player(object):
  """Scrabble Player"""
  COMMA = u','
  EMPTY = u''
  EQUAL = u'='
  BINGO_VALUE = 50
  PERMUTE_LEN_MAX = 8
  SCORELESS_TURN_MAX = 6
  WHITESPACE = re.compile("\s*")

  def __init__(self, players, wordList, board, reverse, testing):
    self.reverse = reverse
    self.testing = testing
    self.players = players
    self.scores = [0] * self.players
    self.racks = [[] for player in range(self.players)]
    self.board = board
    self.board.fillRacks(self.racks)

    self.dictionary = set(wordList)
    if not wordList:
      print(u'There are no words.')

    self.turn = 0
    self.last_score_turn = self.turn
    self.first_pass_turn_player = None
    pass

  def Test(self):
    #Len Count
    #  1: 2
    #  2: 5
    #  3: 16
    #  4: 65
    #  5: 326
    #  6: 1957
    #  7: 13700
    #  8: 109601
    #  9: 986410
    # 10: 9864101

    cases = [u'a', u'ab', u'abc', u'abcd', u'abcde', u'abcdef', u'abcdefg', u'abcdefgh', u'abcdefghi']
    for case in cases:
      words = self.permute(case)
      print(Board.KVP.format(len(case), len(words)))

  def perform(self, commands):
    for command in commands:
      if not self.dispatch(command):
        break

  def start(self):
    loop = True
    while loop:
      turn_player = self.turn % self.players
      player_turn = self.turn / self.players
      self.showRack(turn_player)
      prompt = u'Player {}, Turn {}: '.format(turn_player + 1, player_turn + 1)
      command = Player.prompt(prompt)
      loop = self.dispatch(command)

    for player in range(self.players):
      self.debitRackValue(player)

    Player.showScores(self.scores)

  def help(self):
    print(u'board')
    print(u'exchange letters')
    print(u'exit')
    print(u'help')
    print(u'list letters')
    print(u'pass')
    print(u'play square=letter, ...')
    print(u'score')

  def dispatch(self, command):
    tokens = re.split(Player.WHITESPACE, command.strip())
    if not tokens:
      return True

    turn_player = self.turn % self.players
    rack = self.racks[turn_player]
    token = tokens.pop(0)
    rest = Player.EMPTY.join(tokens)
    verb = token.lower()
    if verb == Player.EMPTY:
      return True
    elif verb == u'board':
      self.board.display()
      return True
    elif verb == u'exchange':
      letters = rest.upper() if rest else rack
      self.board.tiles.exchange(rack, letters)
      self.showRack(turn_player)
      self.first_pass_turn_player = None
      self.turn += 1
      # Test for Scoreless Turn below
      pass
    elif verb == u'exit':
      return False
    elif verb == u'help':
      self.help()
      return True
    elif verb == u'list':
      letters = letters = rest.upper() if rest else rack
      self.list(letters)
      return True
    elif verb == u'pass':
      if self.first_pass_turn_player == turn_player:
        print(u'All Players passed; and Player {} has passed twice.'
              .format(turn_player + 1))
        return False
      
      if self.first_pass_turn_player is None:
        self.first_pass_turn_player = turn_player

      self.turn += 1
      # Test for Scoreless Turn below
      pass
    elif verb == u'play':
      placements = self.parsePlacements(rest)
      # Allow turn_player to try again after Illegal Tile Placement
      if not placements:
        return True

      word = self.legalPlacements(placements)
      if not word:
        return True

      if self.placeTiles(placements, word):
        self.first_pass_turn_player = None
        self.last_score_turn = self.turn

        if len(rack) == 0:
          print(u'Player {}, Rack Empty and Bag Empty.'
                .format(turn_player + 1))
          return False

        self.turn += 1
        # Test for Scoreless Turn below
      pass
    elif verb == u'score':
      Player.showScores(self.scores)
      return True
    else:
      print(u'{} is not a valid command.'.format(token))
      return True

    if self.last_score_turn + Player.SCORELESS_TURN_MAX <= self.turn:
      print(u'The limit of {} Scoreless Turns has been reached.'
            .format(Player.SCORELESS_TURN_MAX))
      return False

    return True
  
  def placeTiles(self, placements, word):
    letters = [letter for square, letter in placements]
 
    turn_player = self.turn % self.players
    rack = self.racks[turn_player]
    if not Tile.hasLetters(rack, letters):
      print(u'Player {}, Rack: {} missing some of the letters: {}'
            .format(turn_player + 1, Tile.spaced(rack), Tile.spaced(letters)))
      if not self.testing:
        return False

    # Two Blanks played on the first turn can result in a valid total of zero
    valid, total = self.evaluatePlacements(placements, word)
    if valid:
      for placement in placements:
        square, letter = placement
        self.board.set(square, letter)

      self.board.tiles.exchange(rack, letters)

      # Award 50-point bonus if a full rack was used
      if len(letters) == Tile.RACKSIZE:
        print(u'Bingo!')
        total += Player.BINGO_VALUE

    # Increase player score
    self.scores[turn_player] += total
    print(u'Player {} gained {} points'.format(turn_player + 1, total))
    return True

  def firstPlay(self):
    return self.board.get(self.board.center) == Board.BLANK

  def firstValid(self, placements):
    squares = [square for square, letter in placements]
    if len(squares) < 2 or squares[0] in squares[1:]:
      return False
    return self.board.center in squares

  def legalPlacements(self, placements):
    empty = Player.EMPTY
    if self.firstPlay():
      if not self.firstValid(placements):
        print(u'First word must cover the center square')
        if not self.testing:
          return empty
    elif not self.board.contact(placements):
      letters = [letter for square, letter in placements]
      print(u'The letters: {} are not in contact with an existing word'
            .format(Tile.spaced(letters)))
      if not self.testing:
        return empty

    # Return Main Word:
    horizontal = self.board.horizontal(placements)
    word = self.board.tiled(placements, horizontal)
    if not word:
      squareNames = [Board.squareName(square) for square, letter in placements]
      print(u'Illegal Tile Placement: {}'
            .format(Tile.spaced(squareNames)))

    return word

  def parsePlacements(self, statement):
    empty = []
    placements = []
    clauses = statement.split(Player.COMMA)
    for clause in clauses:
      placement = self.parsePlacement(clause)
      if not placement:
        return empty
      placements.append(placement)
    return placements

  #
  # Placements are of the form: square=letter where the letter may
  # be followed by a question mark to indicate use of a blank tile.
  #
  def parsePlacement(self, clause):
    empty = ()
    terms = clause.split(Player.EQUAL)
    if len(terms) != 2:
      print(u'{} is not a valid placement'.format(clause))
      return empty

    squareName = terms[0].strip()
    text = terms[1].strip()

    if len(text) == 0:
      print(u'no letter supplied')
      return empty

    blank = len(text) == 2 and text[1] == Tile.QUESTION
    if len(text) > 1 and not blank:
      print(u'{} is not a valid tile'.format(text))
      return empty

    letter = text[0]
    if not letter.isalpha():
      print(u'{} is not a valid letter'.format(letter))
      return empty

    square = self.board.parseSquare(squareName)
    if not square:
      return empty

    return square, letter.lower() if blank else letter.upper()

  def validWord(self, word, reverse):
    if not self.dictionary or word.lower() in self.dictionary:
      return word
 
    if reverse:
      rword = word[::-1]
      if rword.lower() in self.dictionary:
        return rword

    print(u'{} not found'.format(word))
    #[ToDo]Prompt for dictionary override [Y/N] here
    return Player.EMPTY

  def list(self, letters):
    alphas = [letter for letter in letters if letter.isalpha()]
    length = len(alphas)
    if length > Player.PERMUTE_LEN_MAX:
      print(u'Length of {} exceeds Maximum of {}'
            .format(length, Player.PERMUTE_LEN_MAX))
      if not self.testing:
        return

    permutations = self.permute(alphas)
    words = [word for word in permutations if word.lower() in self.dictionary] if self.dictionary else permutations

    for index, word in enumerate(sorted(words)):
      counter = str(index + 1)
      print(Board.KVP.format(counter.rjust(3), word))

  def evaluatePlacements(self, placements, tiledWord):
    total = 0
    tiledScore = self.board.score(tiledWord, placements)

    horizontal = self.board.horizontal(placements)
    pairs = self.board.crosswords(placements, horizontal)
    pairs.insert(0, (tiledWord, tiledScore))

    # Validate new words
    for word, score in pairs:
      valid = self.validWord(word, self.reverse)
      if not valid:
        if not self.testing:
          return False, total
      print('word: {}, score: {}'.format(valid, score))
      total += score

    return True, total

  def showRack(self, turn_player):
    rack = self.racks[turn_player]
    print(u'Player {}, Rack: {}'.format(turn_player + 1, Tile.spaced(rack)))

  def debitRackValue(self, player):
      rack = self.racks[player]
      debit = self.board.tiles.rackValue(rack)
      print(u'Player {}, Debit: {}'.format(player + 1, debit))
      self.scores[player] -= debit

  @staticmethod
  def showScores(scores):
    for player, score in sorted(enumerate(scores), key=itemgetter(1, 0), reverse=True):
      print(u'Player {}, Score: {}'.format(player + 1, score))

  @staticmethod
  def prompt(prompt):
    return raw_input(prompt)

  @staticmethod
  def permute(letters):
    """Return every permutation of letters"""
    chosen = set()
    words = [Player.EMPTY]
    for choice in range(len(letters)):
      letter = letters[choice]
      if letter not in chosen:
        chosen.add(letter)
        unused = letters[:choice] + letters[choice + 1:]
        for suffix in Player.permute(unused):
          words.append(letter + suffix)
    return words
  
  @staticmethod
  def findFirst(predicate, lines, filename=None):
    """Find first line satisfying predicate"""
    try:
      # Python generator
      found = (index for index, element in enumerate(lines) if predicate(element))
      found_index = next(found)
    except StopIteration:
      print(u'Only blank lines found in {0}'.format(filename))
      found_index = None

    return found_index
