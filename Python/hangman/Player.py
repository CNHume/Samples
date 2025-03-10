# -*- coding: utf-8 -*-
# Copyright (C) 2018, Christopher Hume.  All rights reserved.
#
# You should have received a copy of the MIT License along with this program.
# If not, see https://opensource.org/licenses/MIT.
#
# 2018-07-11 CNHume Removed incorrect break after a successful guess
# 2018-07-09 CNHume Created File for Eric Peterson of Rigetti Computing
#
import string

class Player(object):
  '''Hangman Player'''
  SPACE = ' '
  BLANK = '_'

  def __init__(self, word, figures):
    self.word = word.upper()
    self.figures = figures

  def play(self, trials):
    show = self.figures and len(self.figures) == trials + 1
    guesses = set()
    length = len(self.word)
    template = Player.BLANK * length
    # Mutable status:
    status = list(template)

    print('You have {} guesses'.format(trials))
    trial = 0
    while trial < trials:
      if show:
        Player.show(self.figures[trial])
      Player.display(status)
      guess = Player.prompt('Player Guess: ')
      if guess in guesses:
          print('You have already tried {}'.format(guess))
          continue

      guesses.add(guess)
      found = False
      for index in range(length):
        if self.word[index] == guess:
           found = True
           status[index] = guess

      if found:
        print('Correct!')
        if Player.BLANK not in status:
          # Player Won
          Player.display(status)
          return True
      else:
        trial += 1
        print('{}/{} incorrect guesses'.format(trial, trials))

    # Player Lost
    if show:
      Player.show(self.figures[trial])
    Player.display(self.word)
    return False

  @staticmethod
  def show(records):
    print
    for record in records:
      print(record)
    print

  @staticmethod
  def display(letters):
    print('Word: {}'.format(Player.spaced(letters)))

  @staticmethod
  def spaced(letters):
    return Player.SPACE.join(letters)

  @staticmethod
  def prompt(prompt):
    while True:
      raw = input(prompt)
      line = raw.splitlines()[0]
      if len(line) == 1 and line in string.ascii_letters:
         return line.upper()
      else:
        print('Please enter a single letter')
