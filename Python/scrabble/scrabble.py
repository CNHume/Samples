# -*- coding: utf-8 -*-
# Copyright (C) 2018, Christopher Hume.  All rights reserved.
#
# You should have received a copy of the MIT License along with this program.
# If not, see https://opensource.org/licenses/MIT.
#
# The name Scrabble is a trademark of Hasbro in the United States and Canada.
# Outside these two countries it is a trademark of Mattel.
# See https://en.wikipedia.org/wiki/Scrabble
#
# Regarding NSA Tournament Rules (1995)
# See http://www.poslarchive.com/math/scrabble/rules/nsa.html
#
# 2018-09-09 CNHume Created File
#
import sys
import traceback

from Command import Command
from Board import Board
from Player import Player
from Tile import Tile
from FileManager import FileManager

def main():
  # Command Line Defaults:
  SETUP_PATH = 'Game Data'
  BONUS_FILE = 'bonus'
  TILES_FILE = 'tiles'
  WORDS_FILE = 'scrabbleWords'
  FILE_EXT = 'txt'                     # Default File Extension
  PLAYERS = 2
  BOARD_SIZE = 15

  try:
    command = Command(BONUS_FILE, TILES_FILE, WORDS_FILE, FILE_EXT, PLAYERS, BOARD_SIZE)
    if command.Parse(sys.argv):
      verbose = command.verbose

      if command.players <= 0:
        print('The # of players must be greater than 0')
        return

      tileManager = FileManager(SETUP_PATH, command.file_ext, verbose)
      tileManager.load(command.tiles_file)
      tiles = Tile(tileManager.records)

      bonusManager = FileManager(SETUP_PATH, command.file_ext, verbose)
      bonusManager.load(command.bonus_file)

      #
      # Test Board
      #
      #testBoard = Board(command.size_x, command.size_y, bonusManager.records, tiles)
      #testBoard.Test()

      board = Board(command.size_x, command.size_y, bonusManager.records, tiles)

      wordManager = FileManager(SETUP_PATH, command.file_ext, verbose)
      wordManager.load(command.words_file)
      player = Player(command.players, wordManager.records, board, command.reverse, command.debug)
      #player.Test()

      testCommands = ['play g8=c,h8=a,i8=t',
        'play k8=t,l8=o,m8=n,n8=i,o8=c',
        'play k9=u,k10=f,k11=f',
        'play i9=o,j9=v,l9=m',
        'play j8=a,j10=o,j11=i,j12=d',
        'board']
      #player.perform(testCommands)

      turn_player = player.start()

  except Exception as ex:
    #type_name = type(ex).__name__
    trace = traceback.format_exc()
    print(trace)

  #[Debug]
  # input('Press Enter')

if __name__ == '__main__':
  main()
  pass
