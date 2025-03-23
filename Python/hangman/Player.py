# Copyright (C) 2018, Christopher Hume.  All rights reserved.
#
# You should have received a copy of the MIT License along with this program.
# If not, see https://opensource.org/licenses/MIT.
#
# 2018-07-11 CNHume Removed incorrect break after a successful guess
# 2018-07-09 CNHume Created File for Eric Peterson of Rigetti Computing
#
import string

SPACE = ' '
BLANK = '_'


class Player(object):
  """Hangman Player"""

  def __init__(self, word, figures):
    self.word = word.upper()
    self.figures = figures

  def play(self, trials):
    showFigure = self.figures and len(self.figures) == trials + 1
    guesses = set()
    length = len(self.word)
    template = BLANK * length
    # Mutable status:
    status = list(template)

    print('You have {} guesses'.format(trials))
    trial = 0
    while trial < trials:
      if showFigure:
        show(self.figures[trial])
      display(status)
      guess = prompt('Player Guess: ')
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
        if BLANK not in status:
          # Player Won
          display(status)
          return True
      else:
        trial += 1
        print('{}/{} incorrect guesses'.format(trial, trials))

    # Player Lost
    if show:
      show(self.figures[trial])
    display(self.word)
    return False


def show(records):
  print
  for record in records:
    print(record)
  print


def display(letters):
  print('Word: {}'.format(spaced(letters)))


def spaced(letters):
  return SPACE.join(letters)


def prompt(prompt):
  while True:
    raw = input(prompt)
    line = raw.splitlines()[0]
    if len(line) == 1 and line in string.ascii_letters:
      return line.upper()
    else:
      print('Please enter a single letter')
