# -*- coding: utf-8 -*-
# Copyright (C) 2018, Christopher Hume.  All rights reserved.
#
# You should have received a copy of the MIT License along with this program.
# If not, see https://opensource.org/licenses/MIT.
#
# 2018-07-11 CNHume Added Command and FileManager classes
# 2018-07-09 CNHume Created File for Eric Peterson of Rigetti Computing
#
# References
# ----------
# Hangman (game) from Wikipedia
# See https://en.wikipedia.org/wiki/Hangman_(game)
#
# 49 unbeatable words for the game 'hangman' from Laura Hale Brockway
# See https://www.prdaily.com/Main/Articles/20880.aspx
#
import random
import sys
import traceback

from Command import Command
from Player import Player
from FileManager import FileManager

def main():
  # Command Line Defaults:
  SETUP_PATH = u''
  ART_FILE = u'art'                     # Hangman ASCII Art
  WORD_FILE = u'words'                  # Word File (Hangman Dictionary)
  FILE_EXT = u'txt'                     # Word File Extension
  TRIALS = 6                            # Head, Body, 2 Arms, 2 Legs

  try:
    command = Command(WORD_FILE, ART_FILE, FILE_EXT, TRIALS)
    if command.Parse(sys.argv):
      verbose = command.verbose
      artManager = FileManager(SETUP_PATH, command.file_ext, verbose)
      artManager.load(command.art_file)
      figures = artManager.paragraphs()

      wordManager = FileManager(SETUP_PATH, command.file_ext, verbose)
      wordManager.load(command.word_file)
  
      if wordManager.length > 0:
        choice = random.randrange(0, wordManager.length)
        word = wordManager.records[choice]

        player = Player(word, figures)
        result = player.play(command.trials)
        message = u'You win!' if result else u"You're hung."
        print(message)
      else:
        print(u'There are no words.')

  except Exception as ex:
    #type_name = type(ex).__name__
    trace = traceback.format_exc()
    print(trace)

  #[Debug]
  raw_input(u'Press Enter')

if __name__ == '__main__':
  main()
  pass
