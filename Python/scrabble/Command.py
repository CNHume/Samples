# -*- coding: utf-8 -*-
# Copyright (C) 2018, Christopher Hume.  All rights reserved.
#
# You should have received a copy of the MIT License along with this program.
# If not, see https://opensource.org/licenses/MIT.
#
# 2018-09-09 CNHume Created Command class
#
import sys

class Command(object):
  """Command Class"""
  VERSION = 1.0

  def __init__(self, bonus_path, tiles_path, words_path, file_ext, players, size):
    self.debug = False                  # -d run in debug mode
    self.verbose = False                # -v emit verbose data
    self.reverse = False                # -r allow reversed words (right to left or below to above)
    self.players = players
    self.size_x = size
    self.size_y = size
    self.bonus_path = bonus_path
    self.tiles_path = tiles_path
    self.words_path = words_path
    self.file_ext = file_ext

  @staticmethod
  def arg_msg(s, name="argument"):
    msg = "Invalid "
    msg += name
    msg += ": "
    msg += s
    return msg

  def Parse(self, argv):
    """Command Line Parser"""
    argc = len(argv)
    usage = False
    n = 0
    script_name = argv[n]
    n += 1
    while n < argc and not usage:       # parse any switches
      token = argv[n]
      token_len = len(token)
      if token_len < 1 or token[0] != '-':
        break                           # end of switches
      elif token_len < 2:
        usage = True
      else:                             # parse single character switch
        if token[1] == 'd':             # the debug switch
          if token_len > 2:             # superfluous value specified
            usage = True
          else:
            self.debug = True
        elif token[1] == 'r':           # the reverse switch
          if token_len > 2:             # superfluous value specified
            usage = True
          else:
            self.reverse = True
        elif token[1] == 'v':           # the verbose switch
          if token_len > 2:             # superfluous value specified
            usage = True
          else:
            self.verbose = True
        elif token[1] == 'b':           # the bonus_path switch
          if token_len > 2:             # whitespace optional
            self.bonus_path = token[2:].decode("utf-8")
          elif n < argc:                # whitespace allowed
            n += 1
            self.bonus_path = argv[n].decode("utf-8")
        elif token[1] == 'p':           # the players switch
          if token_len > 2:             # whitespace optional
            players = token[2:]
          elif n < argc:                # whitespace allowed
            n += 1
            players = argv[n]
          self.players = int(players)
        elif token[1] == 's':           # the size switch
          if token_len > 2:             # whitespace optional
            size = token[2:]
          elif n < argc:                # whitespace allowed
            n += 1
            size = argv[n]
          if size.isdigit():
            self.size_x = int(size)
            self.size_y = self.size_x
        elif token[1] == 't':           # the tiles_path switch
          if token_len > 2:             # whitespace optional
            self.tiles_path = token[2:].decode("utf-8")
          elif n < argc:                # whitespace allowed
            n += 1
            self.tiles_path = argv[n].decode("utf-8")
        elif token[1] == 'w':           # the words_path switch
          if token_len > 2:             # whitespace optional
            self.words_path = token[2:].decode("utf-8")
          elif n < argc:                # whitespace allowed
            n += 1
            self.words_path = argv[n].decode("utf-8")
        elif token[1] == 'x':           # the file_ext switch
          if token_len > 2:             # whitespace optional
            self.file_ext = token[2:]
          elif n < argc:                # whitespace allowed
            n += 1
            self.file_ext = argv[n]
        elif token[1] == '?':           # the help switch
          usage = True
        else:                           # switch unknown
          usage = True
      n += 1

    if n < argc:                        # superfluous argument specified
      usage = True

    if self.verbose:
      self.Log()

    if usage:                           # throw usage line if parse failed
      print(u'Usage: python {0} [-d] [-v] [-r] [-b bonus_path] [-s size] [-t tiles_path] [-w words_path] [-x file_ext]'\
        .format(script_name))

    return not usage

  def Log(self):
    print(u'{0}: {1}'.format(u'debug', self.debug))
    print(u'{0}: {1}'.format(u'verbose', self.verbose))
    print(u'{0}: {1}'.format(u'reverse', self.reverse))
    print(u'{0}: {1}'.format(u'bonus_path', self.bonus_path))
    print(u'{0}: {1}'.format(u'tiles_path', self.tiles_path))
    print(u'{0}: {1}'.format(u'words_path', self.words_path))
    print(u'{0}: {1}'.format(u'file_ext', self.file_ext))
    print(u'{0}: {1}'.format(u'players', self.players))
    print(u'{0}: {1}'.format(u'size_x', self.size_x))
    print(u'{0}: {1}'.format(u'size_y', self.size_y))
