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
  """Hangman Player"""
  SPACE = u' '
  UNDERSCORE = u'_'

  def __init__(self, word, figures):
    self.word = word.upper()
    self.figures = figures

  def play(self, trials):
    show = self.figures and len(self.figures) == trials + 1
    guesses = set()
    length = len(self.word)
    template = u''.rjust(length, Player.UNDERSCORE)
    status = list(template)

    print(u"You have {} guesses".format(trials))
    trial = 0
    while trial < trials:
      if show:
        Player.show(self.figures[trial])
      Player.display(status)
      guess = Player.prompt(u"Player Guess: ")
      if guess in guesses:
          print(u"You have already tried {}".format(guess))
          continue

      guesses.add(guess)
      found = False
      for index in range(length):
        if self.word[index] == guess:
           found = True
           status[index] = guess

      if found:
        print(u"Correct!")
        if Player.UNDERSCORE not in status:
          # Player Won
          Player.display(status)
          return True
      else:
        trial += 1
        print(u"{}/{} incorrect guesses".format(trial, trials))

    # Player Lost
    if show:
      Player.show(self.figures[trial])
    answer = list(self.word)
    Player.display(answer)
    return False

  @staticmethod
  def show(records):
    print(u"")
    for record in records:
      print(record)
    print(u"")

  @staticmethod
  def display(letters):
    print(u"Word: {}".format(Player.spaced(letters)))

  @staticmethod
  def spaced(letters):
    return Player.SPACE.join([letter for letter in letters])

  @staticmethod
  def prompt(prompt):
    while True:
      input = raw_input(prompt)
      if len(input) == 1 and input in string.letters:
         return input.upper()
      else:
        print(u"Please enter a single letter")
