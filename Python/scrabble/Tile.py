# -*- coding: utf-8 -*-
# Copyright (C) 2018, Christopher Hume.  All rights reserved.
#
# You should have received a copy of the MIT License along with this program.
# If not, see https://opensource.org/licenses/MIT.
#
# 2018-09-22 CNHume Created File
#
import string
import random

class Tile(object):
  '''Scrabble Tiles'''
  ALPHA = ord('A')
  ASTERISK = '*'
  COMMA = ','
  DELIM = ', '
  EMPTY = ''
  EQUAL = '='
  KVP = '{}: {}'
  QUESTION = '?'
  RACKSIZE = 7
  SPACE = ' '

  def __init__(self, tileRecords):
    self.tiles = Tile.parseTiles(tileRecords)
    if not self.tiles:
      print('There are no tile records.')
    self.bag = Tile.letterBag(self.tiles)

  @staticmethod
  def parseTiles(tileRecords):
    empty = {}
    tiles = {}

    #
    # The Tiles File specifies value count pairs for each letter
    # according to the pattern: letter=value*count
    #
    # ?=0*2,A=1*9,B=3*2,C=3*2,D=2*4,E=1*12,F=4*2,G=2*3,H=4*2,I=1*9,J=8*1,K=5*1,L=1*4,M=3*2,N=1*6,O=1*8,P=3*2,Q=10*1,R=1*6,S=1*4,T=1*6,U=1*4,V=4*2,W=4*2,X=8*1,Y=4*2,Z=10*1
    #
    for record in tileRecords:
      clauses = record.split(Tile.COMMA)
      for clause in clauses:
        # Ignore empty clauses
        if len(clause.strip()) == 0:
          continue
        tile = Tile.parseTile(clause)
        if tile:
          letter, values = tile
          if letter in tiles:
            print('Duplicate tile values specified for {}'.format(letter))
            return empty
          else:
            tiles[letter] = values
        else:
          return empty

    # Verify that all of the tiles are represented:
    if len(tiles) < 27:
      missing = []
      for index in range(27):
        letter = chr(Tile.ALPHA + index) if index < 26 else Tile.QUESTION
        if letter not in tiles:
          missing.append(letter)
      print('No tile values specified for the letters: {}'.format(Tile.DELIM.join(missing)))
      return empty

    return tiles

  @staticmethod
  def parseTile(clause):
    empty = ()
    tile = ()

    terms = clause.split(Tile.EQUAL)
    if len(terms) != 2:
      print('{} is not a valid tile'.format(clause))
      return empty

    letter = terms[0].strip()
    if len(letter) != 1 or letter != Tile.QUESTION and not letter.isalpha():
      print('{} is not a valid letter'.format(letter))
      return empty

    values = terms[1].split(Tile.ASTERISK)
    if len(values) != 2:
      print('{} are not valid terms'.format(terms))
      return empty

    value = Tile.parseInt(values[0].strip())
    count = Tile.parseInt(values[1].strip())
    tile = letter.upper(), (value, count)

    return tile

  @staticmethod
  def parseInt(str):
    return int(str) if str.isdigit() else None

  @staticmethod
  def letterBag(tiles):
    bag = []

    for letter, values in tiles.items():
      #[Test]print(Tile.KVP.format(letter, values))
      value, count = values
      for index in range(count):
        bag.append(letter)

    return bag
    
  def letterValue(self, letter):
    if letter.islower():
      # lower case indicates the presence of a blank tile
      return 0
 
    value, count = self.tiles[letter]
    return value

  def rackValue(self, rack):
    return sum(map(self.letterValue, rack))

  def fill(self, rack):
    fill_count = Tile.RACKSIZE - len(rack)
    bag_count = len(self.bag)
    if bag_count < fill_count:
      fill_count = bag_count

    for n in range(fill_count):
      rack.append(Tile.choose(self.bag))

  def remove(self, rack, letters):
    missing = []
    for letter in letters:
      symbol = letter if letter.isupper() else Tile.QUESTION
      if symbol in rack:
        rack.remove(symbol)
      else:
        missing.append(symbol)

    if missing:
      copula = 'is' if len(missing) < 2 else 'are'
      print('{} {} not among the remaining letters {}'.format(Tile.spaced(missing), copula, Tile.spaced(rack)))
  
  def exchange(self, rack, letters):
    self.remove(rack, letters)
    self.fill(rack)
  
  @staticmethod
  def choose(list):
    if list:
      choice = random.randrange(0, len(list))
      return list.pop(choice)
    else:
      return Tile.EMPTY

  # Verify that every required letter corresponds to a letter in the rack
  @staticmethod
  def hasLetters(rack, letters):
    letterSet = Tile.counterSet(letters)
    #rack = list('TAU?FOM')
    rackSet = Tile.counterSet(rack)

    for letter, letter_count in letterSet.items():
      symbol = letter if letter.isupper() else Tile.QUESTION
      rack_count = rackSet[symbol] if symbol in rackSet else 0
      if rack_count < letter_count:
        return False

    return True

  @staticmethod
  def hasLettersGeneral(rack, letters):
    letterSet = Tile.counterSet(letters)
    rackSet = Tile.counterSet(rack)
    blank_count = rackSet[Tile.QUESTION] if Tile.QUESTION in rackSet else 0
    for letter, letter_count in letterSet.items():
      #letter = letter.toupper()
      rack_count = rackSet[letter] if letter in rackSet else 0
      blanks_used = letter_count - rack_count
      if blank_count < blanks_used:
        return False
      if blanks_used > 0:
        blank_count -= blanks_used
    return True

  @staticmethod
  def counterSet(elements):
    counts = {}
    for element in elements:
      symbol = Tile.QUESTION if element.islower() else element
      counts[symbol] = counts[symbol] + 1 if symbol in counts else 1
    return counts
  
  @staticmethod
  def spaced(letters):
    return Tile.SPACE.join(letters)
